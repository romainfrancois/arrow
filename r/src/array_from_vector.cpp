// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

#include <memory>

#include "./arrow_types.h"
#include "./arrow_vctrs.h"

#if defined(ARROW_R_WITH_ARROW)
#include <arrow/array/array_base.h>
#include <arrow/builder.h>
#include <arrow/chunked_array.h>
#include <arrow/util/bitmap_writer.h>
#include <arrow/visitor_inline.h>

using arrow::internal::checked_cast;

namespace arrow {
namespace r {

template <typename T>
inline bool is_na(T value) {
  return false;
}

template <>
inline bool is_na<int64_t>(int64_t value) {
  return value == NA_INT64;
}

template <>
inline bool is_na<double>(double value) {
  return ISNA(value);
}

template <>
inline bool is_na<int>(int value) {
  return value == NA_INTEGER;
}

template <typename T>
int64_t time_cast(T value);

template <>
inline int64_t time_cast<int>(int value) {
  return static_cast<int64_t>(value) * 1000;
}

template <>
inline int64_t time_cast<double>(double value) {
  return static_cast<int64_t>(value * 1000);
}

}  // namespace r
}  // namespace arrow

// ---------------- new api

namespace arrow {

namespace internal {

template <typename T, typename Target,
          typename std::enable_if<std::is_signed<Target>::value, Target>::type = 0>
Status int_cast(T x, Target* out) {
  if (static_cast<int64_t>(x) < std::numeric_limits<Target>::min() ||
      static_cast<int64_t>(x) > std::numeric_limits<Target>::max()) {
    return Status::Invalid("Value is too large to fit in C integer type");
  }
  *out = static_cast<Target>(x);
  return Status::OK();
}

template <typename T>
struct usigned_type;

template <typename T, typename Target,
          typename std::enable_if<std::is_unsigned<Target>::value, Target>::type = 0>
Status int_cast(T x, Target* out) {
  // we need to compare between unsigned integers
  uint64_t x64 = x;
  if (x64 < 0 || x64 > std::numeric_limits<Target>::max()) {
    return Status::Invalid("Value is too large to fit in C integer type");
  }
  *out = static_cast<Target>(x);
  return Status::OK();
}

template <typename Int>
Status double_cast(Int x, double* out) {
  *out = static_cast<double>(x);
  return Status::OK();
}

template <>
Status double_cast<int64_t>(int64_t x, double* out) {
  constexpr int64_t kDoubleMax = 1LL << 53;
  constexpr int64_t kDoubleMin = -(1LL << 53);

  if (x < kDoubleMin || x > kDoubleMax) {
    return Status::Invalid("integer value ", x, " is outside of the range exactly",
                           " representable by a IEEE 754 double precision value");
  }
  *out = static_cast<double>(x);
  return Status::OK();
}

// used for int and int64_t
template <typename T>
Status float_cast(T x, float* out) {
  constexpr int64_t kHalfFloatMax = 1LL << 24;
  constexpr int64_t kHalfFloatMin = -(1LL << 24);

  int64_t x64 = static_cast<int64_t>(x);
  if (x64 < kHalfFloatMin || x64 > kHalfFloatMax) {
    return Status::Invalid("integer value ", x, " is outside of the range exactly",
                           " representable by a IEEE 754 half precision value");
  }

  *out = static_cast<float>(x);
  return Status::OK();
}

template <>
Status float_cast<double>(double x, float* out) {
  // TODO: is there some sort of floating point overflow ?
  *out = static_cast<float>(x);
  return Status::OK();
}

}  // namespace internal

namespace r {

class VectorConverter {
 public:
  virtual ~VectorConverter() = default;

  virtual Status Init(ArrayBuilder* builder) = 0;

  virtual Status Ingest(SEXP obj) = 0;

  virtual Status GetResult(std::shared_ptr<arrow::Array>* result) {
    return builder_->Finish(result);
  }

  ArrayBuilder* builder() const { return builder_; }

 protected:
  ArrayBuilder* builder_;
};

class NullVectorConverter : public VectorConverter {
 public:
  using BuilderType = NullBuilder;

  ~NullVectorConverter() {}

  Status Init(ArrayBuilder* builder) override {
    builder_ = builder;
    typed_builder_ = checked_cast<BuilderType*>(builder_);
    return Status::OK();
  }

  Status Ingest(SEXP obj) override {
    RETURN_NOT_OK(typed_builder_->AppendNulls(XLENGTH(obj)));
    return Status::OK();
  }

 protected:
  BuilderType* typed_builder_;
};

template <typename Type, typename Enable = void>
struct Unbox {};

// unboxer for int type
template <typename Type>
struct Unbox<Type, enable_if_integer<Type>> {
  using BuilderType = typename TypeTraits<Type>::BuilderType;
  using ArrayType = typename TypeTraits<Type>::ArrayType;
  using CType = typename ArrayType::value_type;

  static inline Status Ingest(BuilderType* builder, SEXP obj) {
    switch (TYPEOF(obj)) {
      case INTSXP:
        return IngestRange<int>(builder, INTEGER(obj), XLENGTH(obj));
      case REALSXP:
        if (Rf_inherits(obj, "integer64")) {
          return IngestRange<int64_t>(builder, reinterpret_cast<int64_t*>(REAL(obj)),
                                      XLENGTH(obj));
        }
        return IngestRange(builder, REAL(obj), XLENGTH(obj));

      // TODO: handle raw and logical
      default:
        break;
    }

    return Status::Invalid("Cannot convert R vector of type <", Rf_type2char(TYPEOF(obj)),
                           "> to integer Arrow array");
  }

  template <typename T>
  static inline Status IngestRange(BuilderType* builder, T* p, R_xlen_t n) {
    RETURN_NOT_OK(builder->Resize(n));
    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (is_na<T>(*p)) {
        builder->UnsafeAppendNull();
      } else {
        CType value = 0;
        RETURN_NOT_OK(internal::int_cast(*p, &value));
        builder->UnsafeAppend(value);
      }
    }
    return Status::OK();
  }
};

template <>
struct Unbox<DoubleType> {
  static inline Status Ingest(DoubleBuilder* builder, SEXP obj) {
    switch (TYPEOF(obj)) {
      // TODO: handle RAW
      case INTSXP:
        return IngestIntRange<int>(builder, INTEGER(obj), XLENGTH(obj), NA_INTEGER);
      case REALSXP:
        if (Rf_inherits(obj, "integer64")) {
          return IngestIntRange<int64_t>(builder, reinterpret_cast<int64_t*>(REAL(obj)),
                                         XLENGTH(obj), NA_INT64);
        }
        return IngestDoubleRange(builder, REAL(obj), XLENGTH(obj));
    }
    return Status::Invalid("Cannot convert R object to double type");
  }

  template <typename T>
  static inline Status IngestIntRange(DoubleBuilder* builder, T* p, R_xlen_t n, T na) {
    RETURN_NOT_OK(builder->Resize(n));
    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (*p == NA_INTEGER) {
        builder->UnsafeAppendNull();
      } else {
        double value = 0;
        RETURN_NOT_OK(internal::double_cast(*p, &value));
        builder->UnsafeAppend(value);
      }
    }
    return Status::OK();
  }

  static inline Status IngestDoubleRange(DoubleBuilder* builder, double* p, R_xlen_t n) {
    RETURN_NOT_OK(builder->Resize(n));
    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (ISNA(*p)) {
        builder->UnsafeAppendNull();
      } else {
        builder->UnsafeAppend(*p);
      }
    }
    return Status::OK();
  }
};

template <>
struct Unbox<FloatType> {
  static inline Status Ingest(FloatBuilder* builder, SEXP obj) {
    switch (TYPEOF(obj)) {
      // TODO: handle RAW
      case INTSXP:
        return IngestIntRange<int>(builder, INTEGER(obj), XLENGTH(obj), NA_INTEGER);
      case REALSXP:
        if (Rf_inherits(obj, "integer64")) {
          return IngestIntRange<int64_t>(builder, reinterpret_cast<int64_t*>(REAL(obj)),
                                         XLENGTH(obj), NA_INT64);
        }
        return IngestDoubleRange(builder, REAL(obj), XLENGTH(obj));
    }
    return Status::Invalid("Cannot convert R object to double type");
  }

  template <typename T>
  static inline Status IngestIntRange(FloatBuilder* builder, T* p, R_xlen_t n, T na) {
    RETURN_NOT_OK(builder->Resize(n));
    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (*p == NA_INTEGER) {
        builder->UnsafeAppendNull();
      } else {
        float value = 0;
        RETURN_NOT_OK(internal::float_cast(*p, &value));
        builder->UnsafeAppend(value);
      }
    }
    return Status::OK();
  }

  static inline Status IngestDoubleRange(FloatBuilder* builder, double* p, R_xlen_t n) {
    RETURN_NOT_OK(builder->Resize(n));
    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (ISNA(*p)) {
        builder->UnsafeAppendNull();
      } else {
        float value;
        RETURN_NOT_OK(internal::float_cast(*p, &value));
        builder->UnsafeAppend(value);
      }
    }
    return Status::OK();
  }
};

template <>
struct Unbox<BooleanType> {
  static inline Status Ingest(BooleanBuilder* builder, SEXP obj) {
    switch (TYPEOF(obj)) {
      case LGLSXP: {
        R_xlen_t n = XLENGTH(obj);
        RETURN_NOT_OK(builder->Resize(n));
        int* p = LOGICAL(obj);
        for (R_xlen_t i = 0; i < n; i++, ++p) {
          if (*p == NA_LOGICAL) {
            builder->UnsafeAppendNull();
          } else {
            builder->UnsafeAppend(*p == 1);
          }
        }
        return Status::OK();
      }

      default:
        break;
    }

    // TODO: include more information about the R object and the target type
    return Status::Invalid("Cannot convert R object to boolean type");
  }
};

template <>
struct Unbox<Date32Type> {
  static inline Status Ingest(Date32Builder* builder, SEXP obj) {
    switch (TYPEOF(obj)) {
      case INTSXP:
        if (Rf_inherits(obj, "Date")) {
          return IngestIntRange(builder, INTEGER(obj), XLENGTH(obj));
        }
        break;
      case REALSXP:
        if (Rf_inherits(obj, "Date")) {
          return IngestDoubleRange(builder, REAL(obj), XLENGTH(obj));
        }
        break;
      default:
        break;
    }
    return Status::Invalid("Cannot convert R object to date32 type");
  }

  static inline Status IngestIntRange(Date32Builder* builder, int* p, R_xlen_t n) {
    RETURN_NOT_OK(builder->Resize(n));
    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (*p == NA_INTEGER) {
        builder->UnsafeAppendNull();
      } else {
        builder->UnsafeAppend(*p);
      }
    }
    return Status::OK();
  }

  static inline Status IngestDoubleRange(Date32Builder* builder, double* p, R_xlen_t n) {
    RETURN_NOT_OK(builder->Resize(n));
    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (ISNA(*p)) {
        builder->UnsafeAppendNull();
      } else {
        builder->UnsafeAppend(static_cast<int>(*p));
      }
    }
    return Status::OK();
  }
};

template <>
struct Unbox<Date64Type> {
  constexpr static int64_t kMillisecondsPerDay = 86400000;

  static inline Status Ingest(Date64Builder* builder, SEXP obj) {
    switch (TYPEOF(obj)) {
      case INTSXP:
        // number of days since epoch
        if (Rf_inherits(obj, "Date")) {
          return IngestDateInt32Range(builder, INTEGER(obj), XLENGTH(obj));
        }
        break;

      case REALSXP:
        // (fractional number of days since epoch)
        if (Rf_inherits(obj, "Date")) {
          return IngestDateDoubleRange<kMillisecondsPerDay>(builder, REAL(obj),
                                                            XLENGTH(obj));
        }

        // number of seconds since epoch
        if (Rf_inherits(obj, "POSIXct")) {
          return IngestDateDoubleRange<1000>(builder, REAL(obj), XLENGTH(obj));
        }
    }
    return Status::Invalid("Cannot convert R object to date64 type");
  }

  // ingest a integer vector that represents number of days since epoch
  static inline Status IngestDateInt32Range(Date64Builder* builder, int* p, R_xlen_t n) {
    RETURN_NOT_OK(builder->Resize(n));
    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (*p == NA_INTEGER) {
        builder->UnsafeAppendNull();
      } else {
        builder->UnsafeAppend(*p * kMillisecondsPerDay);
      }
    }
    return Status::OK();
  }

  // ingest a numeric vector that represents (fractional) number of days since epoch
  template <int64_t MULTIPLIER>
  static inline Status IngestDateDoubleRange(Date64Builder* builder, double* p,
                                             R_xlen_t n) {
    RETURN_NOT_OK(builder->Resize(n));

    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (ISNA(*p)) {
        builder->UnsafeAppendNull();
      } else {
        builder->UnsafeAppend(static_cast<int64_t>(*p * MULTIPLIER));
      }
    }
    return Status::OK();
  }
};

template <typename Type, class Derived>
class TypedVectorConverter : public VectorConverter {
 public:
  using BuilderType = typename TypeTraits<Type>::BuilderType;

  Status Init(ArrayBuilder* builder) override {
    builder_ = builder;
    typed_builder_ = checked_cast<BuilderType*>(builder_);
    return Status::OK();
  }

  Status Ingest(SEXP obj) override { return Unbox<Type>::Ingest(typed_builder_, obj); }

 protected:
  BuilderType* typed_builder_;
};

template <typename Type>
class NumericVectorConverter
    : public TypedVectorConverter<Type, NumericVectorConverter<Type>> {};

class BooleanVectorConverter
    : public TypedVectorConverter<BooleanType, BooleanVectorConverter> {};

class Date32Converter : public TypedVectorConverter<Date32Type, Date32Converter> {};
class Date64Converter : public TypedVectorConverter<Date64Type, Date64Converter> {};

inline int64_t get_time_multiplier(TimeUnit::type unit) {
  switch (unit) {
    case TimeUnit::SECOND:
      return 1;
    case TimeUnit::MILLI:
      return 1000;
    case TimeUnit::MICRO:
      return 1000000;
    case TimeUnit::NANO:
      return 1000000000;
    default:
      return 0;
  }
}

template <typename Type>
class TimeConverter : public VectorConverter {
  using BuilderType = typename TypeTraits<Type>::BuilderType;

 public:
  explicit TimeConverter(TimeUnit::type unit)
      : unit_(unit), multiplier_(get_time_multiplier(unit)) {}

  Status Init(ArrayBuilder* builder) override {
    builder_ = builder;
    typed_builder_ = checked_cast<BuilderType*>(builder);
    return Status::OK();
  }

  Status Ingest(SEXP obj) override {
    if (valid_R_object(obj)) {
      int difftime_multiplier;
      RETURN_NOT_OK(GetDifftimeMultiplier(obj, &difftime_multiplier));
      return Ingest_POSIXct(REAL(obj), XLENGTH(obj), difftime_multiplier);
    }

    return Status::Invalid("Cannot convert R object to timestamp type");
  }

 protected:
  TimeUnit::type unit_;
  BuilderType* typed_builder_;
  int64_t multiplier_;

  Status Ingest_POSIXct(double* p, R_xlen_t n, int difftime_multiplier) {
    RETURN_NOT_OK(typed_builder_->Resize(n));

    for (R_xlen_t i = 0; i < n; i++, ++p) {
      if (ISNA(*p)) {
        typed_builder_->UnsafeAppendNull();
      } else {
        typed_builder_->UnsafeAppend(
            static_cast<int64_t>(*p * multiplier_ * difftime_multiplier));
      }
    }
    return Status::OK();
  }

  virtual bool valid_R_object(SEXP obj) = 0;

  // only used for Time32 and Time64
  virtual Status GetDifftimeMultiplier(SEXP obj, int* res) {
    std::string unit(CHAR(STRING_ELT(Rf_getAttrib(obj, symbols::units), 0)));
    if (unit == "secs") {
      *res = 1;
    } else if (unit == "mins") {
      *res = 60;
    } else if (unit == "hours") {
      *res = 3600;
    } else if (unit == "days") {
      *res = 86400;
    } else if (unit == "weeks") {
      *res = 604800;
    } else {
      return Status::Invalid("unknown difftime unit");
    }
    return Status::OK();
  }
};

class TimestampConverter : public TimeConverter<TimestampType> {
 public:
  explicit TimestampConverter(TimeUnit::type unit) : TimeConverter<TimestampType>(unit) {}

 protected:
  bool valid_R_object(SEXP obj) override {
    return TYPEOF(obj) == REALSXP && Rf_inherits(obj, "POSIXct");
  }

  Status GetDifftimeMultiplier(SEXP obj, int* res) override {
    *res = 1;
    return Status::OK();
  }
};

class Time32Converter : public TimeConverter<Time32Type> {
 public:
  explicit Time32Converter(TimeUnit::type unit) : TimeConverter<Time32Type>(unit) {}

 protected:
  bool valid_R_object(SEXP obj) override {
    return TYPEOF(obj) == REALSXP && Rf_inherits(obj, "difftime");
  }
};

class Time64Converter : public TimeConverter<Time64Type> {
 public:
  explicit Time64Converter(TimeUnit::type unit) : TimeConverter<Time64Type>(unit) {}

 protected:
  bool valid_R_object(SEXP obj) override {
    return TYPEOF(obj) == REALSXP && Rf_inherits(obj, "difftime");
  }
};

template <typename Builder>
class BinaryVectorConverter : public VectorConverter {
 public:
  ~BinaryVectorConverter() {}

  Status Init(ArrayBuilder* builder) {
    typed_builder_ = checked_cast<Builder*>(builder);
    return Status::OK();
  }

  Status Ingest(SEXP obj) {
    ARROW_RETURN_IF(TYPEOF(obj) != VECSXP, Status::RError("Expecting a list"));
    R_xlen_t n = XLENGTH(obj);

    // Reserve enough space before appending
    int64_t size = 0;
    for (R_xlen_t i = 0; i < n; i++) {
      SEXP obj_i = VECTOR_ELT(obj, i);
      if (!Rf_isNull(obj_i)) {
        ARROW_RETURN_IF(TYPEOF(obj_i) != RAWSXP,
                        Status::RError("Expecting a raw vector"));
        size += XLENGTH(obj_i);
      }
    }
    RETURN_NOT_OK(typed_builder_->Reserve(size));

    // append
    for (R_xlen_t i = 0; i < n; i++) {
      SEXP obj_i = VECTOR_ELT(obj, i);
      if (Rf_isNull(obj_i)) {
        RETURN_NOT_OK(typed_builder_->AppendNull());
      } else {
        RETURN_NOT_OK(typed_builder_->Append(RAW(obj_i), XLENGTH(obj_i)));
      }
    }
    return Status::OK();
  }

  Status GetResult(std::shared_ptr<arrow::Array>* result) {
    return typed_builder_->Finish(result);
  }

 private:
  Builder* typed_builder_;
};

class FixedSizeBinaryVectorConverter : public VectorConverter {
 public:
  ~FixedSizeBinaryVectorConverter() {}

  Status Init(ArrayBuilder* builder) {
    typed_builder_ = checked_cast<FixedSizeBinaryBuilder*>(builder);
    return Status::OK();
  }

  Status Ingest(SEXP obj) {
    ARROW_RETURN_IF(TYPEOF(obj) != VECSXP, Status::RError("Expecting a list"));
    R_xlen_t n = XLENGTH(obj);

    // Reserve enough space before appending
    int32_t byte_width = typed_builder_->byte_width();
    for (R_xlen_t i = 0; i < n; i++) {
      SEXP obj_i = VECTOR_ELT(obj, i);
      if (!Rf_isNull(obj_i)) {
        ARROW_RETURN_IF(TYPEOF(obj_i) != RAWSXP,
                        Status::RError("Expecting a raw vector"));
        ARROW_RETURN_IF(XLENGTH(obj_i) != byte_width,
                        Status::RError("Expecting a raw vector of ", byte_width,
                                       " bytes, not ", XLENGTH(obj_i)));
      }
    }
    RETURN_NOT_OK(typed_builder_->Reserve(n * byte_width));

    // append
    for (R_xlen_t i = 0; i < n; i++) {
      SEXP obj_i = VECTOR_ELT(obj, i);
      if (Rf_isNull(obj_i)) {
        RETURN_NOT_OK(typed_builder_->AppendNull());
      } else {
        RETURN_NOT_OK(typed_builder_->Append(RAW(obj_i)));
      }
    }
    return Status::OK();
  }

  Status GetResult(std::shared_ptr<arrow::Array>* result) {
    return typed_builder_->Finish(result);
  }

 private:
  FixedSizeBinaryBuilder* typed_builder_;
};

template <typename StringBuilder>
class StringVectorConverter : public VectorConverter {
 public:
  ~StringVectorConverter() {}

  Status Init(ArrayBuilder* builder) {
    typed_builder_ = checked_cast<StringBuilder*>(builder);
    return Status::OK();
  }

  Status Ingest(SEXP obj) {
    ARROW_RETURN_IF(TYPEOF(obj) != STRSXP,
                    Status::RError("Expecting a character vector"));

    cpp11::strings s(arrow::r::utf8_strings(obj));
    RETURN_NOT_OK(typed_builder_->Reserve(s.size()));

    // we know all the R strings are utf8 already, so we can get
    // a definite size and then use UnsafeAppend*()
    int64_t total_length = 0;
    for (cpp11::r_string si : s) {
      total_length += cpp11::is_na(si) ? 0 : si.size();
    }
    RETURN_NOT_OK(typed_builder_->ReserveData(total_length));

    // append
    for (cpp11::r_string si : s) {
      if (si == NA_STRING) {
        typed_builder_->UnsafeAppendNull();
      } else {
        typed_builder_->UnsafeAppend(CHAR(si), si.size());
      }
    }

    return Status::OK();
  }

  Status GetResult(std::shared_ptr<arrow::Array>* result) {
    return typed_builder_->Finish(result);
  }

 private:
  StringBuilder* typed_builder_;
};

#define NUMERIC_CONVERTER(TYPE_ENUM, TYPE)                                               \
  case Type::TYPE_ENUM:                                                                  \
    *out =                                                                               \
        std::unique_ptr<NumericVectorConverter<TYPE>>(new NumericVectorConverter<TYPE>); \
    return Status::OK()

#define SIMPLE_CONVERTER_CASE(TYPE_ENUM, TYPE) \
  case Type::TYPE_ENUM:                        \
    *out = std::unique_ptr<TYPE>(new TYPE);    \
    return Status::OK()

#define TIME_CONVERTER_CASE(TYPE_ENUM, DATA_TYPE, TYPE)                                \
  case Type::TYPE_ENUM:                                                                \
    *out =                                                                             \
        std::unique_ptr<TYPE>(new TYPE(checked_cast<DATA_TYPE*>(type.get())->unit())); \
    return Status::OK()

std::shared_ptr<arrow::Array> Array__from_vector(
    SEXP x, const std::shared_ptr<arrow::DataType>& type, bool type_inferred) {

  // general conversion with converter and builder
  std::unique_ptr<arrow::r::VectorConverter> converter;

  // Create ArrayBuilder for type
  std::unique_ptr<arrow::ArrayBuilder> type_builder;
  StopIfNotOk(arrow::MakeBuilder(gc_memory_pool(), type, &type_builder));
  StopIfNotOk(converter->Init(type_builder.get()));

  // ingest R data and grab the result array
  StopIfNotOk(converter->Ingest(x));
  std::shared_ptr<arrow::Array> result;
  StopIfNotOk(converter->GetResult(&result));
  return result;
}

}  // namespace r
}  // namespace arrow

// [[arrow::export]]
std::shared_ptr<arrow::ChunkedArray> ChunkedArray__from_list(cpp11::list chunks,
                                                             SEXP s_type) {
  std::vector<std::shared_ptr<arrow::Array>> vec;

  // the type might be NULL, in which case we need to infer it from the data
  // we keep track of whether it was inferred or supplied
  bool type_inferred = Rf_isNull(s_type);
  R_xlen_t n = XLENGTH(chunks);

  std::shared_ptr<arrow::DataType> type;
  if (type_inferred) {
    if (n == 0) {
      cpp11::stop("type must be specified for empty list");
    }
    type = arrow::r::InferArrowType(VECTOR_ELT(chunks, 0));
  } else {
    type = cpp11::as_cpp<std::shared_ptr<arrow::DataType>>(s_type);
  }

  if (n == 0) {
    std::shared_ptr<arrow::Array> array;
    std::unique_ptr<arrow::ArrayBuilder> type_builder;
    StopIfNotOk(arrow::MakeBuilder(gc_memory_pool(), type, &type_builder));
    StopIfNotOk(type_builder->Finish(&array));
    vec.push_back(array);
  } else {
    // the first - might differ from the rest of the loop
    // because we might have inferred the type from the first element of the list
    //
    // this only really matters for dictionary arrays
    vec.push_back(arrow::r::vec_to_arrow(chunks[0], type, type_inferred));

    for (R_xlen_t i = 1; i < n; i++) {
      vec.push_back(arrow::r::vec_to_arrow(chunks[i], type, false));
    }
  }

  return std::make_shared<arrow::ChunkedArray>(std::move(vec));
}

#endif

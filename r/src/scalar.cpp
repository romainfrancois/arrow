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

#include "./arrow_types.h"

#if defined(ARROW_R_WITH_ARROW)

#include <arrow/array/array_base.h>
#include <arrow/array/util.h>
#include <arrow/scalar.h>
#include <arrow/type.h>

namespace cpp11 {

R6 r6_Scalar(const std::shared_ptr<arrow::Scalar>& ptr) {
  std::string type = "Scalar";
  if (ptr->type->id() == arrow::Type::STRUCT) {
    type = "StructScalar";
  }
  return r6(ptr, type);
}

}  // namespace cpp11

// [[arrow::export]]
R6 Array__GetScalar(const std::shared_ptr<arrow::Array>& x, int64_t i) {
  return cpp11::r6_Scalar(ValueOrStop(x->GetScalar(i)));
}

// [[arrow::export]]
std::string Scalar__ToString(const std::shared_ptr<arrow::Scalar>& s) {
  return s->ToString();
}

// [[arrow::export]]
R6 Scalar__CastTo(const std::shared_ptr<arrow::Scalar>& s,
                  const std::shared_ptr<arrow::DataType>& t) {
  return cpp11::r6_Scalar(ValueOrStop(s->CastTo(t)));
}

// [[arrow::export]]
R6 StructScalar__field(const std::shared_ptr<arrow::StructScalar>& s, int i) {
  return cpp11::r6_Scalar(ValueOrStop(s->field(i)));
}

// [[arrow::export]]
R6 StructScalar__GetFieldByName(const std::shared_ptr<arrow::StructScalar>& s,
                                const std::string& name) {
  return cpp11::r6_Scalar(ValueOrStop(s->field(name)));
}

// [[arrow::export]]
SEXP Scalar__as_vector(const std::shared_ptr<arrow::Scalar>& scalar) {
  auto array = ValueOrStop(arrow::MakeArrayFromScalar(*scalar, 1));

  // defined in array_to_vector.cpp
  SEXP Array__as_vector(const std::shared_ptr<arrow::Array>& array);
  return Array__as_vector(array);
}

// [[arrow::export]]
bool Scalar__is_valid(const std::shared_ptr<arrow::Scalar>& s) { return s->is_valid; }

// [[arrow::export]]
std::shared_ptr<arrow::DataType> Scalar__type(const std::shared_ptr<arrow::Scalar>& s) {
  return s->type;
}

#endif

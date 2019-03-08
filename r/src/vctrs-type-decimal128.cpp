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

#include <arrow/util/decimal.h>
#include "./arrow_types.h"

template <typename Vector, typename value_type>
Rcpp::ComplexVector IntVector_to_Decimal128(Vector x) {
  auto n = x.size();
  Rcpp::ComplexVector res(Rcpp::no_init(n));
  std::copy_n(reinterpret_cast<value_type*>(x.begin()), n,
              reinterpret_cast<arrow::Decimal128*>(res.begin()));
  return res;
}

// [[Rcpp::export]]
Rcpp::ComplexVector IntegerVector_to_Decimal128(Rcpp::IntegerVector_ x) {
  return IntVector_to_Decimal128<Rcpp::IntegerVector_, int32_t>(x);
}

// [[Rcpp::export]]
Rcpp::ComplexVector Integer64Vector_to_Decimal128(Rcpp::Integer64Vector_ x) {
  return IntVector_to_Decimal128<Rcpp::Integer64Vector_, int64_t>(x);
}

template <typename OutputVector, typename value_type>
OutputVector Decimal128_To_Int(Rcpp::ComplexVector_ x, value_type NA) {
  auto n = x.size();
  OutputVector res(Rcpp::no_init(n));

  auto p_res = reinterpret_cast<value_type*>(res.begin());
  auto p_x = reinterpret_cast<arrow::Decimal128*>(x.begin());

  auto construct = [NA](arrow::Decimal128 decimal) {
    value_type value;
    auto status = decimal.ToInteger<value_type>(&value);
    if (!status.ok()) {
      value = NA;
    }
    return value;
  };
  std::transform(p_x, p_x + n, p_res, construct);

  return res;
}

// [[Rcpp::export]]
Rcpp::NumericVector Decimal128_To_Integer64(Rcpp::ComplexVector_ x) {
  Rcpp::Integer64Vector res =
      Decimal128_To_Int<Rcpp::Integer64Vector, int64_t>(x, arrow::r::NA_INT64);
  res.attr("class") = "integer64";
  return res;
}

// [[Rcpp::export]]
Rcpp::IntegerVector Decimal128_To_Integer(Rcpp::ComplexVector_ x) {
  return Decimal128_To_Int<Rcpp::IntegerVector, int32_t>(x, NA_INTEGER);
}

// [[Rcpp::export]]
Rcpp::CharacterVector format_decimal128(arrow::r::Decimal128Record record) {
  auto data = record.data();
  auto n = data.size();
  auto p = reinterpret_cast<arrow::Decimal128*>(data.begin());

  Rcpp::CharacterVector res(p, p + n, [record](arrow::Decimal128 decimal) {
    return decimal.ToString(record.scale());
  });
  return res;
}

// [[Rcpp::export]]
std::string Decimal128Array__FormatValue(
    const std::shared_ptr<arrow::Decimal128Array>& array, int64_t i) {
  return array->FormatValue(i);
}

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

// [[arrow::export]]
std::shared_ptr<arrow::dataset::FieldExpression> dataset___expr__field_ref(std::string name) {
  return std::make_shared<arrow::dataset::FieldExpression>(std::move(name));
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::ComparisonExpression> dataset___expr__equal(
    const std::shared_ptr<arrow::dataset::Expression>& lhs,
    const std::shared_ptr<arrow::dataset::Expression>& rhs) {
  return arrow::dataset::equal(lhs, rhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::ComparisonExpression> dataset___expr__not_equal(
    const std::shared_ptr<arrow::dataset::Expression>& lhs,
    const std::shared_ptr<arrow::dataset::Expression>& rhs) {
  return arrow::dataset::not_equal(lhs, rhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::ComparisonExpression> dataset___expr__greater(
    const std::shared_ptr<arrow::dataset::Expression>& lhs,
    const std::shared_ptr<arrow::dataset::Expression>& rhs) {
  return arrow::dataset::greater(lhs, rhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::ComparisonExpression> dataset___expr__greater_equal(
    const std::shared_ptr<arrow::dataset::Expression>& lhs,
    const std::shared_ptr<arrow::dataset::Expression>& rhs) {
  return arrow::dataset::greater_equal(lhs, rhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::ComparisonExpression> dataset___expr__less(
    const std::shared_ptr<arrow::dataset::Expression>& lhs,
    const std::shared_ptr<arrow::dataset::Expression>& rhs) {
  return arrow::dataset::less(lhs, rhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::ComparisonExpression> dataset___expr__less_equal(
    const std::shared_ptr<arrow::dataset::Expression>& lhs,
    const std::shared_ptr<arrow::dataset::Expression>& rhs) {
  return arrow::dataset::less_equal(lhs, rhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::AndExpression> dataset___expr__and(
    const std::shared_ptr<arrow::dataset::Expression>& lhs,
    const std::shared_ptr<arrow::dataset::Expression>& rhs) {
  return arrow::dataset::and_(lhs, rhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::OrExpression> dataset___expr__or(
    const std::shared_ptr<arrow::dataset::Expression>& lhs,
    const std::shared_ptr<arrow::dataset::Expression>& rhs) {
  return arrow::dataset::or_(lhs, rhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::NotExpression> dataset___expr__not(
    const std::shared_ptr<arrow::dataset::Expression>& lhs) {
  return arrow::dataset::not_(lhs);
}

// [[arrow::export]]
std::shared_ptr<arrow::dataset::ScalarExpression> dataset___expr__scalar(SEXP x) {
  switch (TYPEOF(x)) {
    case LGLSXP:
      return arrow::dataset::scalar(Rf_asLogical(x));
    case REALSXP:
      return arrow::dataset::scalar(Rf_asReal(x));
    case INTSXP:
      return arrow::dataset::scalar(Rf_asInteger(x));
    default:
      // TODO more types (character, factor, Date, POSIXt, etc.)
      Rcpp::stop(
          tfm::format("R object of type %s not supported", Rf_type2char(TYPEOF(x))));
  }
  return nullptr;
}

// [[arrow::export]]
std::string dataset___expr__ToString(const std::shared_ptr<arrow::dataset::Expression>& x) {
  return x->ToString();
}

#endif

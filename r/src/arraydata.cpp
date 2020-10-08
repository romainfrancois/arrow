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
#include <arrow/array/data.h>

// [[arrow::export]]
R6 ArrayData__get_type(const std::shared_ptr<arrow::ArrayData>& x) {
  return cpp11::r6_DataType(x->type);
}

// [[arrow::export]]
int ArrayData__get_length(const std::shared_ptr<arrow::ArrayData>& x) {
  return x->length;
}

// [[arrow::export]]
int ArrayData__get_null_count(const std::shared_ptr<arrow::ArrayData>& x) {
  return x->null_count;
}

// [[arrow::export]]
int ArrayData__get_offset(const std::shared_ptr<arrow::ArrayData>& x) {
  return x->offset;
}

// [[arrow::export]]
cpp11::list ArrayData__buffers(const std::shared_ptr<arrow::ArrayData>& x) {
  return arrow::r::to_r_list(x->buffers, [](const std::shared_ptr<arrow::Buffer>& buf) {
    return cpp11::r6(buf, "Buffer");
  });
}

#endif

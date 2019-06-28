# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

context("test-type")

test_that("type() gets the right type for arrow::Array", {
  a <- array(1:10)
  expect_equal(type(a), a$type)
})

test_that("type() gets the right type for arrow::ChunkedArray", {
  a <- chunked_array(1:10, 1:10)
  expect_equal(type(a), a$type)
})

test_that("type() infers from R type", {
  expect_equal(type(1:10), int32())
  expect_equal(type(1), float64())
  expect_equal(type(TRUE), boolean())
  expect_equal(type(raw()), int8())
  expect_equal(type(""), utf8())
  expect_equal(
    type(iris$Species),
    dictionary(int8(), array(levels(iris$Species)), FALSE)
  )
  expect_equal(
    type(lubridate::ymd_hms("2019-02-14 13:55:05")),
    timestamp(TimeUnit$MICRO, "GMT")
  )
  expect_equal(
    type(hms::hms(56, 34, 12)),
    time32(unit = TimeUnit$SECOND)
  )
  expect_equal(
    type(bit64::integer64()),
    int64()
  )
})

test_that("type() can infer struct types from data frames", {
  df <- tibble::tibble(x = 1:10, y = rnorm(10), z = letters[1:10])
  expect_equal(type(df), struct(x = int32(), y = float64(), z = utf8()))
})

test_that("type() recognize vctrs_list_of", {
  expect_equal(type(vctrs::list_of(1L, c(1L, 2L, 3L))), list_of(int32()))
  expect_equal(type(vctrs::list_of(1, 2, 1:3)), list_of(float64()))
  expect_equal(type(vctrs::list_of(mtcars, mtcars)), list_of(struct(!!!purrr::map(mtcars, type))))
})

test_that("type() infers via vctrs::vec_type_common()", {
  expect_equal(type(list(1, c(2, 3))), list_of(float64()))
  expect_equal(type(list(mtcars, mtcars)), list_of(struct(!!!purrr::map(mtcars, type))))
})

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

#include <arrow/table.h>
#include <parquet/arrow/reader.h>
#include <parquet/arrow/writer.h>
#include <parquet/exception.h>

// [[arrow::export]]
R6 parquet___arrow___ArrowReaderProperties__Make(bool use_threads) {
  return cpp11::r6(std::make_shared<parquet::ArrowReaderProperties>(use_threads),
                   "ParquetReaderProperties");
}

// [[arrow::export]]
void parquet___arrow___ArrowReaderProperties__set_use_threads(
    const std::shared_ptr<parquet::ArrowReaderProperties>& properties, bool use_threads) {
  properties->set_use_threads(use_threads);
}

// [[arrow::export]]
bool parquet___arrow___ArrowReaderProperties__get_use_threads(
    const std::shared_ptr<parquet::ArrowReaderProperties>& properties, bool use_threads) {
  return properties->use_threads();
}

// [[arrow::export]]
bool parquet___arrow___ArrowReaderProperties__get_read_dictionary(
    const std::shared_ptr<parquet::ArrowReaderProperties>& properties, int column_index) {
  return properties->read_dictionary(column_index);
}

// [[arrow::export]]
void parquet___arrow___ArrowReaderProperties__set_read_dictionary(
    const std::shared_ptr<parquet::ArrowReaderProperties>& properties, int column_index,
    bool read_dict) {
  properties->set_read_dictionary(column_index, read_dict);
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__OpenFile(
    const std::shared_ptr<arrow::io::RandomAccessFile>& file,
    const std::shared_ptr<parquet::ArrowReaderProperties>& props) {
  std::unique_ptr<parquet::arrow::FileReader> reader;
  parquet::arrow::FileReaderBuilder builder;
  PARQUET_THROW_NOT_OK(builder.Open(file));
  PARQUET_THROW_NOT_OK(builder.properties(*props)->Build(&reader));
  return cpp11::r6(std::shared_ptr<parquet::arrow::FileReader>(std::move(reader)),
                   "ParquetFileReader");
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__ReadTable1(
    const std::shared_ptr<parquet::arrow::FileReader>& reader) {
  std::shared_ptr<arrow::Table> table;
  PARQUET_THROW_NOT_OK(reader->ReadTable(&table));
  return cpp11::r6(table, "Table");
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__ReadTable2(
    const std::shared_ptr<parquet::arrow::FileReader>& reader,
    const std::vector<int>& column_indices) {
  std::shared_ptr<arrow::Table> table;
  PARQUET_THROW_NOT_OK(reader->ReadTable(column_indices, &table));
  return cpp11::r6(table, "Table");
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__ReadRowGroup1(
    const std::shared_ptr<parquet::arrow::FileReader>& reader, int i) {
  std::shared_ptr<arrow::Table> table;
  PARQUET_THROW_NOT_OK(reader->ReadRowGroup(i, &table));
  return cpp11::r6(table, "Table");
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__ReadRowGroup2(
    const std::shared_ptr<parquet::arrow::FileReader>& reader, int i,
    const std::vector<int>& column_indices) {
  std::shared_ptr<arrow::Table> table;
  PARQUET_THROW_NOT_OK(reader->ReadRowGroup(i, column_indices, &table));
  return cpp11::r6(table, "Table");
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__ReadRowGroups1(
    const std::shared_ptr<parquet::arrow::FileReader>& reader,
    const std::vector<int>& row_groups) {
  std::shared_ptr<arrow::Table> table;
  PARQUET_THROW_NOT_OK(reader->ReadRowGroups(row_groups, &table));
  return cpp11::r6(table, "Table");
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__ReadRowGroups2(
    const std::shared_ptr<parquet::arrow::FileReader>& reader,
    const std::vector<int>& row_groups, const std::vector<int>& column_indices) {
  std::shared_ptr<arrow::Table> table;
  PARQUET_THROW_NOT_OK(reader->ReadRowGroups(row_groups, column_indices, &table));
  return cpp11::r6(table, "Table");
}

// [[arrow::export]]
int64_t parquet___arrow___FileReader__num_rows(
    const std::shared_ptr<parquet::arrow::FileReader>& reader) {
  return reader->parquet_reader()->metadata()->num_rows();
}

// [[arrow::export]]
int parquet___arrow___FileReader__num_columns(
    const std::shared_ptr<parquet::arrow::FileReader>& reader) {
  return reader->parquet_reader()->metadata()->num_columns();
}

// [[arrow::export]]
int parquet___arrow___FileReader__num_row_groups(
    const std::shared_ptr<parquet::arrow::FileReader>& reader) {
  return reader->num_row_groups();
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__ReadColumn(
    const std::shared_ptr<parquet::arrow::FileReader>& reader, int i) {
  std::shared_ptr<arrow::ChunkedArray> array;
  PARQUET_THROW_NOT_OK(reader->ReadColumn(i - 1, &array));
  return cpp11::r6(array, "ChunkedArray");
}

namespace parquet {

class WriterPropertiesBuilder : public WriterProperties::Builder {
 public:
  using WriterProperties::Builder::Builder;
};

class ArrowWriterPropertiesBuilder : public ArrowWriterProperties::Builder {
 public:
  using ArrowWriterProperties::Builder::Builder;
};

}  // namespace parquet

// [[arrow::export]]
R6 parquet___ArrowWriterProperties___create(bool allow_truncated_timestamps,
                                            bool use_deprecated_int96_timestamps,
                                            int timestamp_unit) {
  auto builder = std::make_shared<parquet::ArrowWriterPropertiesBuilder>();
  builder->store_schema();

  if (allow_truncated_timestamps) {
    builder->allow_truncated_timestamps();
  }
  if (use_deprecated_int96_timestamps) {
    builder->enable_deprecated_int96_timestamps();
  }
  if (timestamp_unit > -1) {
    // -1 is passed in for NULL/default
    builder->coerce_timestamps(static_cast<arrow::TimeUnit::type>(timestamp_unit));
  }

  return cpp11::r6(builder->build(), "ParquetArrowWriterProperties");
}

// [[arrow::export]]
R6 parquet___WriterProperties___Builder__create() {
  return cpp11::r6(std::make_shared<parquet::WriterPropertiesBuilder>(),
                   "ParquetWriterPropertiesBuilder");
}

// [[arrow::export]]
void parquet___WriterProperties___Builder__version(
    const std::shared_ptr<parquet::WriterPropertiesBuilder>& builder,
    const parquet::ParquetVersion::type& version) {
  builder->version(version);
}

// [[arrow::export]]
void parquet___ArrowWriterProperties___Builder__set_compressions(
    const std::shared_ptr<parquet::WriterPropertiesBuilder>& builder,
    const std::vector<std::string>& paths, cpp11::integers types) {
  auto n = types.size();
  if (n == 1) {
    builder->compression(static_cast<arrow::Compression::type>(types[0]));
  } else {
    for (decltype(n) i = 0; i < n; i++) {
      builder->compression(paths[i], static_cast<arrow::Compression::type>(types[i]));
    }
  }
}

// [[arrow::export]]
void parquet___ArrowWriterProperties___Builder__set_compression_levels(
    const std::shared_ptr<parquet::WriterPropertiesBuilder>& builder,
    const std::vector<std::string>& paths, cpp11::integers levels) {
  auto n = levels.size();
  if (n == 1) {
    builder->compression_level(levels[0]);
  } else {
    for (decltype(n) i = 0; i < n; i++) {
      builder->compression_level(paths[i], levels[i]);
    }
  }
}

// [[arrow::export]]
void parquet___ArrowWriterProperties___Builder__set_use_dictionary(
    const std::shared_ptr<parquet::WriterPropertiesBuilder>& builder,
    const std::vector<std::string>& paths, cpp11::logicals use_dictionary) {
  auto n = use_dictionary.size();
  if (n == 1) {
    if (use_dictionary[0] == TRUE) {
      builder->enable_dictionary();
    } else {
      builder->disable_dictionary();
    }
  } else {
    builder->disable_dictionary();
    for (decltype(n) i = 0; i < n; i++) {
      if (use_dictionary[i] == TRUE) {
        builder->enable_dictionary(paths[i]);
      } else {
        builder->disable_dictionary(paths[i]);
      }
    }
  }
}

// [[arrow::export]]
void parquet___ArrowWriterProperties___Builder__set_write_statistics(
    const std::shared_ptr<parquet::WriterPropertiesBuilder>& builder,
    const std::vector<std::string>& paths, cpp11::logicals write_statistics) {
  auto n = write_statistics.size();
  if (n == 1) {
    if (write_statistics[0] == TRUE) {
      builder->enable_statistics();
    } else {
      builder->disable_statistics();
    }
  } else {
    builder->disable_statistics();
    for (decltype(n) i = 0; i < n; i++) {
      if (write_statistics[i] == TRUE) {
        builder->enable_statistics(paths[i]);
      } else {
        builder->disable_statistics(paths[i]);
      }
    }
  }
}

// [[arrow::export]]
void parquet___ArrowWriterProperties___Builder__data_page_size(
    const std::shared_ptr<parquet::WriterPropertiesBuilder>& builder,
    int64_t data_page_size) {
  builder->data_pagesize(data_page_size);
}

// [[arrow::export]]
R6 parquet___WriterProperties___Builder__build(
    const std::shared_ptr<parquet::WriterPropertiesBuilder>& builder) {
  return cpp11::r6(builder->build(), "ParquetWriterProperties");
}

// [[arrow::export]]
R6 parquet___arrow___ParquetFileWriter__Open(
    const std::shared_ptr<arrow::Schema>& schema,
    const std::shared_ptr<arrow::io::OutputStream>& sink,
    const std::shared_ptr<parquet::WriterProperties>& properties,
    const std::shared_ptr<parquet::ArrowWriterProperties>& arrow_properties) {
  std::unique_ptr<parquet::arrow::FileWriter> writer;
  PARQUET_THROW_NOT_OK(
      parquet::arrow::FileWriter::Open(*schema, arrow::default_memory_pool(), sink,
                                       properties, arrow_properties, &writer));

  return cpp11::r6(std::shared_ptr<parquet::arrow::FileWriter>(std::move(writer)),
                   "ParquetFileWriter");
}

// [[arrow::export]]
void parquet___arrow___FileWriter__WriteTable(
    const std::shared_ptr<parquet::arrow::FileWriter>& writer,
    const std::shared_ptr<arrow::Table>& table, int64_t chunk_size) {
  PARQUET_THROW_NOT_OK(writer->WriteTable(*table, chunk_size));
}

// [[arrow::export]]
void parquet___arrow___FileWriter__Close(
    const std::shared_ptr<parquet::arrow::FileWriter>& writer) {
  PARQUET_THROW_NOT_OK(writer->Close());
}

// [[arrow::export]]
void parquet___arrow___WriteTable(
    const std::shared_ptr<arrow::Table>& table,
    const std::shared_ptr<arrow::io::OutputStream>& sink,
    const std::shared_ptr<parquet::WriterProperties>& properties,
    const std::shared_ptr<parquet::ArrowWriterProperties>& arrow_properties) {
  PARQUET_THROW_NOT_OK(parquet::arrow::WriteTable(*table, arrow::default_memory_pool(),
                                                  sink, table->num_rows(), properties,
                                                  arrow_properties));
}

// [[arrow::export]]
R6 parquet___arrow___FileReader__GetSchema(
    const std::shared_ptr<parquet::arrow::FileReader>& reader) {
  std::shared_ptr<arrow::Schema> schema;
  StopIfNotOk(reader->GetSchema(&schema));
  return cpp11::r6(schema, "Schema");
}

#endif

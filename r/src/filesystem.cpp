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

#include <arrow/filesystem/filesystem.h>
#include <arrow/filesystem/localfs.h>

namespace fs = ::arrow::fs;

// FileInfo

// [[arrow::export]]
fs::FileType fs___FileInfo__type(const std::shared_ptr<fs::FileInfo>& x) {
  return x->type();
}

// [[arrow::export]]
void fs___FileInfo__set_type(const std::shared_ptr<fs::FileInfo>& x, fs::FileType type) {
  x->set_type(type);
}

// [[arrow::export]]
std::string fs___FileInfo__path(const std::shared_ptr<fs::FileInfo>& x) {
  return x->path();
}

// [[arrow::export]]
void fs___FileInfo__set_path(const std::shared_ptr<fs::FileInfo>& x,
                             const std::string& path) {
  x->set_path(path);
}

// [[arrow::export]]
int64_t fs___FileInfo__size(const std::shared_ptr<fs::FileInfo>& x) { return x->size(); }

// [[arrow::export]]
void fs___FileInfo__set_size(const std::shared_ptr<fs::FileInfo>& x, int64_t size) {
  x->set_size(size);
}

// [[arrow::export]]
std::string fs___FileInfo__base_name(const std::shared_ptr<fs::FileInfo>& x) {
  return x->base_name();
}

// [[arrow::export]]
std::string fs___FileInfo__extension(const std::shared_ptr<fs::FileInfo>& x) {
  return x->extension();
}

// [[arrow::export]]
SEXP fs___FileInfo__mtime(const std::shared_ptr<fs::FileInfo>& x) {
  SEXP res = PROTECT(Rf_allocVector(REALSXP, 1));
  // .mtime() gets us nanoseconds since epoch, POSIXct is seconds since epoch as a double
  REAL(res)[0] = static_cast<double>(x->mtime().time_since_epoch().count()) / 1000000000;
  Rf_classgets(res, arrow::r::data::classes_POSIXct);
  UNPROTECT(1);
  return res;
}

// [[arrow::export]]
void fs___FileInfo__set_mtime(const std::shared_ptr<fs::FileInfo>& x, SEXP time) {
  auto nanosecs =
      std::chrono::nanoseconds(static_cast<int64_t>(REAL(time)[0] * 1000000000));
  x->set_mtime(fs::TimePoint(nanosecs));
}

// Selector

// [[arrow::export]]
std::string fs___FileSelector__base_dir(
    const std::shared_ptr<fs::FileSelector>& selector) {
  return selector->base_dir;
}

// [[arrow::export]]
bool fs___FileSelector__allow_not_found(
    const std::shared_ptr<fs::FileSelector>& selector) {
  return selector->allow_not_found;
}

// [[arrow::export]]
bool fs___FileSelector__recursive(const std::shared_ptr<fs::FileSelector>& selector) {
  return selector->recursive;
}

// [[arrow::export]]
R6 fs___FileSelector__create(const std::string& base_dir, bool allow_not_found,
                             bool recursive) {
  auto selector = std::make_shared<fs::FileSelector>();
  selector->base_dir = base_dir;
  selector->allow_not_found = allow_not_found;
  selector->recursive = recursive;
  return cpp11::r6(selector, "FileSelector");
}

// FileSystem

template <typename T>
std::vector<std::shared_ptr<T>> shared_ptr_vector(const std::vector<T>& vec) {
  std::vector<std::shared_ptr<fs::FileInfo>> res(vec.size());
  std::transform(vec.begin(), vec.end(), res.begin(),
                 [](const fs::FileInfo& x) { return std::make_shared<fs::FileInfo>(x); });
  return res;
}

// [[arrow::export]]
cpp11::list fs___FileSystem__GetTargetInfos_Paths(
    const std::shared_ptr<fs::FileSystem>& file_system,
    const std::vector<std::string>& paths) {
  auto results = ValueOrStop(file_system->GetFileInfo(paths));
  return arrow::r::to_r_list(shared_ptr_vector(results),
                             [](const std::shared_ptr<fs::FileInfo>& info) {
                               return cpp11::r6(info, "FileInfo");
                             });
}

// [[arrow::export]]
cpp11::list fs___FileSystem__GetTargetInfos_FileSelector(
    const std::shared_ptr<fs::FileSystem>& file_system,
    const std::shared_ptr<fs::FileSelector>& selector) {
  auto results = ValueOrStop(file_system->GetFileInfo(*selector));

  return arrow::r::to_r_list(shared_ptr_vector(results),
                             [](const std::shared_ptr<fs::FileInfo>& info) {
                               return cpp11::r6(info, "FileInfo");
                             });
}

// [[arrow::export]]
void fs___FileSystem__CreateDir(const std::shared_ptr<fs::FileSystem>& file_system,
                                const std::string& path, bool recursive) {
  StopIfNotOk(file_system->CreateDir(path, recursive));
}

// [[arrow::export]]
void fs___FileSystem__DeleteDir(const std::shared_ptr<fs::FileSystem>& file_system,
                                const std::string& path) {
  StopIfNotOk(file_system->DeleteDir(path));
}

// [[arrow::export]]
void fs___FileSystem__DeleteDirContents(
    const std::shared_ptr<fs::FileSystem>& file_system, const std::string& path) {
  StopIfNotOk(file_system->DeleteDirContents(path));
}

// [[arrow::export]]
void fs___FileSystem__DeleteFile(const std::shared_ptr<fs::FileSystem>& file_system,
                                 const std::string& path) {
  StopIfNotOk(file_system->DeleteFile(path));
}

// [[arrow::export]]
void fs___FileSystem__DeleteFiles(const std::shared_ptr<fs::FileSystem>& file_system,
                                  const std::vector<std::string>& paths) {
  StopIfNotOk(file_system->DeleteFiles(paths));
}

// [[arrow::export]]
void fs___FileSystem__Move(const std::shared_ptr<fs::FileSystem>& file_system,
                           const std::string& src, const std::string& dest) {
  StopIfNotOk(file_system->Move(src, dest));
}

// [[arrow::export]]
void fs___FileSystem__CopyFile(const std::shared_ptr<fs::FileSystem>& file_system,
                               const std::string& src, const std::string& dest) {
  StopIfNotOk(file_system->CopyFile(src, dest));
}

// [[arrow::export]]
R6 fs___FileSystem__OpenInputStream(const std::shared_ptr<fs::FileSystem>& file_system,
                                    const std::string& path) {
  return cpp11::r6(ValueOrStop(file_system->OpenInputStream(path)), "InputStream");
}

// [[arrow::export]]
R6 fs___FileSystem__OpenInputFile(const std::shared_ptr<fs::FileSystem>& file_system,
                                  const std::string& path) {
  return cpp11::r6(ValueOrStop(file_system->OpenInputFile(path)), "RandomAccessFile");
}

// [[arrow::export]]
R6 fs___FileSystem__OpenOutputStream(const std::shared_ptr<fs::FileSystem>& file_system,
                                     const std::string& path) {
  return cpp11::r6(ValueOrStop(file_system->OpenOutputStream(path)), "OutputStream");
}

// [[arrow::export]]
R6 fs___FileSystem__OpenAppendStream(const std::shared_ptr<fs::FileSystem>& file_system,
                                     const std::string& path) {
  return cpp11::r6(ValueOrStop(file_system->OpenAppendStream(path)), "OutputStream");
}

// [[arrow::export]]
std::string fs___FileSystem__type_name(
    const std::shared_ptr<fs::FileSystem>& file_system) {
  return file_system->type_name();
}

// [[arrow::export]]
R6 fs___LocalFileSystem__create() {
  return cpp11::r6(std::make_shared<fs::LocalFileSystem>(), "LocalFileSystem");
}

// [[arrow::export]]
R6 fs___SubTreeFileSystem__create(const std::string& base_path,
                                  const std::shared_ptr<fs::FileSystem>& base_fs) {
  return cpp11::r6(std::make_shared<fs::SubTreeFileSystem>(base_path, base_fs),
                   "SubTreeFileSystem");
}

// [[arrow::export]]
std::shared_ptr<fs::FileSystem> fs___SubTreeFileSystem__base_fs(
    const std::shared_ptr<fs::SubTreeFileSystem>& file_system) {
  return file_system->base_fs();
}

// [[arrow::export]]
std::string fs___SubTreeFileSystem__base_path(
    const std::shared_ptr<fs::SubTreeFileSystem>& file_system) {
  return file_system->base_path();
}

// [[arrow::export]]
cpp11::writable::list fs___FileSystemFromUri(const std::string& path) {
  using cpp11::literals::operator"" _nm;

  std::string out_path;
  cpp11::sexp out_fs =
      cpp11::r6_FileSystem(ValueOrStop(fs::FileSystemFromUri(path, &out_path)));
  return cpp11::writable::list({"fs"_nm = out_fs, "path"_nm = out_path});
}

// [[arrow::export]]
void fs___CopyFiles(const std::shared_ptr<fs::FileSystem>& source_fs,
                    const std::shared_ptr<fs::FileSelector>& source_sel,
                    const std::shared_ptr<fs::FileSystem>& destination_fs,
                    const std::string& destination_base_dir,
                    int64_t chunk_size = 1024 * 1024, bool use_threads = true) {
  StopIfNotOk(fs::CopyFiles(source_fs, *source_sel, destination_fs, destination_base_dir,
                            chunk_size, use_threads));
}

#endif

#if defined(ARROW_R_WITH_S3)

#include <arrow/filesystem/s3fs.h>

// [[s3::export]]
R6 fs___S3FileSystem__create(bool anonymous = false, std::string access_key = "",
                             std::string secret_key = "", std::string session_token = "",
                             std::string role_arn = "", std::string session_name = "",
                             std::string external_id = "", int load_frequency = 900,
                             std::string region = "", std::string endpoint_override = "",
                             std::string scheme = "", bool background_writes = true) {
  fs::S3Options s3_opts;
  // Handle auth (anonymous, keys, default)
  // (validation/internal coherence handled in R)
  if (anonymous) {
    s3_opts = fs::S3Options::Anonymous();
  } else if (access_key != "" && secret_key != "") {
    s3_opts = fs::S3Options::FromAccessKey(access_key, secret_key, session_token);
  } else if (role_arn != "") {
    s3_opts = fs::S3Options::FromAssumeRole(role_arn, session_name, external_id,
                                            load_frequency);
  } else {
    s3_opts = fs::S3Options::Defaults();
  }

  // Now handle the rest of the options
  /// AWS region to connect to (default determined by AWS SDK)
  if (region != "") {
    s3_opts.region = region;
  }
  /// If non-empty, override region with a connect string such as "localhost:9000"
  s3_opts.endpoint_override = endpoint_override;
  /// S3 connection transport, default "https"
  if (scheme != "") {
    s3_opts.scheme = scheme;
  }
  /// Whether OutputStream writes will be issued in the background, without blocking
  /// default true
  s3_opts.background_writes = background_writes;

  StopIfNotOk(fs::EnsureS3Initialized());
  return cpp11::r6(ValueOrStop(fs::S3FileSystem::Make(s3_opts)), "S3FileSystem");
}

// [[s3::export]]
std::string fs___S3FileSystem__region(const std::shared_ptr<fs::S3FileSystem>& fs) {
  return fs->region();
}

#endif

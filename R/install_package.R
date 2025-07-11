#' Validate and prepare URL
#' @param url URL
#' @return URL
#' @keywords internal
validate_and_prepare_url <- function(url) {
  if (missing(url) || is.null(url) || url == "") {
    stop("请填写完整地址！", call. = FALSE)
  }
  
  complete_url <- build_cloud_url(url)
  return(complete_url)
}

#' Detect system environment
#' @return list
#' @keywords internal
detect_system_environment <- function() {
  sys <- Sys.info()[["sysname"]]
  if (sys == "Windows") {
    os_type <- "WIN"
    extract_func <- unzip
  } else if (sys == "Darwin") {
    os_type <- "MAC"
    extract_func <- function(file, exdir) {
      untar(file, exdir = exdir)
    }
  } else {
    stop("当前仅支持Windows和macOS系统。", call. = FALSE)
  }
  
  lib_path <- .libPaths()[1]
  cat("安装路径：", lib_path, "\n")
  
  return(list(
    os_type = os_type,
    extract_func = extract_func,
    lib_path = lib_path
  ))
}

#' Request download permission
#' @param url URL
#' @param machine_code machine
#' @param os_type system type
#' @return list
#' @keywords internal
request_download_permission <- function(url, machine_code, os_type) {
  post_data <- list(
    machine_code = machine_code,
    os_type = os_type
  )

  cat("准备下载...\n")
  res <- tryCatch({
    httr::POST(
      url = url,
      body = jsonlite::toJSON(post_data, auto_unbox = TRUE),
      httr::add_headers("Content-Type" = "application/json"),
      httr::timeout(60)
    )
  }, error = function(e) {
    if (grepl("Timeout", e$message, ignore.case = TRUE)) {
      stop("网络请求超时，请检查网络连接后重试。", call. = FALSE)
    } else {
      stop("网络连接失败，请检查网络设置后重试。", call. = FALSE)
    }
  })

  status_code <- httr::status_code(res)
  if (status_code != 200) {
    if (status_code >= 500) {
      stop("服务器内部错误，请稍后重试。", call. = FALSE)
    } else if (status_code == 404) {
      stop("服务不可用，请检查服务地址。", call. = FALSE)
    }
  }

  text_content <- httr::content(res, "text", encoding = "UTF-8")
  resjson <- tryCatch({
    jsonlite::fromJSON(text_content)
  }, error = function(e) {
    stop("服务器响应格式异常，请稍后重试。", call. = FALSE)
  })

  if (!isTRUE(resjson$success)) {
    if (!is.null(resjson$code)) {
      if (resjson$code == "USER_NOT_REGISTERED") {
        stop("设备未注册，请先完成用户注册。\n机器码：", machine_code, call. = FALSE)
      } else if (resjson$code == "ACCOUNT_DISABLED") {
        stop("账户无法使用。", call. = FALSE)
      } else if (resjson$code == "QUOTA_EXCEEDED") {
        stop("下载受限。", call. = FALSE)
      } else if (resjson$code == "INVALID_OS_TYPE") {
        stop("系统类型参数无效。", call. = FALSE)
      }
    }
    stop("服务器处理异常，请稍后重试。", call. = FALSE)
  }

  return(list(
    package_name = resjson$data$package_name,
    download_url = resjson$data$download_url,
    version = resjson$data$version
  ))
}

#' Download package file
#' @param download_url URL
#' @param os_type type
#' @return path
#' @keywords internal
download_package_file <- function(download_url, os_type) {
  file_ext <- if (os_type == "MAC") ".tgz" else ".zip"
  temp_file <- tempfile(fileext = file_ext)
  download_success <- FALSE

  # Primary download method using httr
  if (requireNamespace("httr", quietly = TRUE)) {
    cat("正在下载...\n")
    tryCatch({
      response <- httr::GET(
        download_url,
        httr::timeout(120),
        httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
      )

      if (httr::status_code(response) == 200) {
        writeBin(httr::content(response, "raw"), temp_file)
        if (file.exists(temp_file) && file.size(temp_file) > 1000) {
          download_success <- TRUE
          cat("文件下载成功！\n")
        }
      }
    }, error = function(e) {
      cat("下载失败，尝试备用方案...\n")
    })
  }

  if (!download_success) {
    cat("使用备用方案下载...\n")

    if (file.exists(temp_file)) file.remove(temp_file)

    tryCatch({
      old_method <- getOption("download.file.method")
      old_ca_bundle <- Sys.getenv("CURL_CA_BUNDLE")

      options(download.file.method = "libcurl")
      Sys.setenv(CURL_CA_BUNDLE = "")

      utils::download.file(download_url, temp_file, mode = "wb", quiet = TRUE)

      if (!is.null(old_method)) options(download.file.method = old_method)
      Sys.setenv(CURL_CA_BUNDLE = old_ca_bundle)

      if (file.exists(temp_file) && file.size(temp_file) > 1000) {
        download_success <- TRUE
        cat("备用方案下载成功！\n")
      }
    }, error = function(e) {
      if (!is.null(old_method)) options(download.file.method = old_method)
      Sys.setenv(CURL_CA_BUNDLE = old_ca_bundle)
    })
  }

  if (!download_success) {
    if (file.exists(temp_file)) file.remove(temp_file)
    error_msg <- paste0(
      "文件下载失败。请尝试以下解决方案：\n",
      "1. 检查网络连接\n",
      "2. 检查环境\n",
      "3. 重启R后重试\n",
      "如问题持续，请联系技术支持。"
    )
    stop(error_msg, call. = FALSE)
  }

  if (!file.exists(temp_file) || file.size(temp_file) < 1000) {
    if (file.exists(temp_file)) file.remove(temp_file)
    stop("下载文件无效或发生中断，请稍后重试。", call. = FALSE)
  }
  
  cat("文件下载完成！\n")
  return(temp_file)
}

#' Install and verify package
#' @param temp_file path
#' @param package_name name of the package
#' @param lib_path path
#' @param extract_func extraction function
#' @return TRUE
#' @keywords internal
install_and_verify_package <- function(temp_file, package_name, lib_path, extract_func) {
  target_dir <- file.path(lib_path, package_name)
  
  # Remove existing package if present
  if (dir.exists(target_dir)) {
    cat("发现现有包，正在删除...\n")
    unlink(target_dir, recursive = TRUE, force = TRUE)

    if (dir.exists(target_dir)) {
      stop("无法删除现有包，可能被占用。请重启R后重试。", call. = FALSE)
    }
    cat("现有包删除成功。\n")
  }

  cat("正在安装包...\n")
  tryCatch({
    extract_func(temp_file, exdir = lib_path)
  }, error = function(e) {
    stop("安装失败，请检查文件完整性或重试。", call. = FALSE)
  })

  if (dir.exists(target_dir)) {
    desc_file <- file.path(target_dir, "DESCRIPTION")
    if (file.exists(desc_file)) {
      cat("安装成功！\n")

      tryCatch({
        library(package_name, character.only = TRUE)
        cat("验证成功，可以正常使用。\n")
      }, error = function(e) {
        cat("安装完成，但加载时出现警告。\n")
        cat("这可能是正常的，请尝试重启R后重试。\n")
      })

    } else {
      stop("安装失败：包结构不完整", call. = FALSE)
    }
  } else {
    stop("安装失败：目标目录未创建", call. = FALSE)
  }

  return(TRUE)
}

#' Clean up temporary files
#' @param temp_file path
#' @keywords internal
cleanup_temp_files <- function(temp_file) {
  if (file.exists(temp_file)) {
    file.remove(temp_file)
  }
}

#' Install Package
#' @param url Character string.
#' @return TRUE
#' @export
install_package <- function(url) {
  complete_url <- validate_and_prepare_url(url)
  
  sys_env <- detect_system_environment()
  
  if (!exists("machine_code", mode = "function")) {
    stop("验证码生成失败", call. = FALSE)
  }
  mcode <- machine_code()
  
  download_info <- request_download_permission(complete_url, mcode, sys_env$os_type)
  cat(sprintf("准备下载%s版本文件...\n", download_info$version))
  
  temp_file <- download_package_file(download_info$download_url, sys_env$os_type)
  
  install_and_verify_package(temp_file, download_info$package_name, 
                           sys_env$lib_path, sys_env$extract_func)
  
  cleanup_temp_files(temp_file)
  
  invisible(TRUE)
}


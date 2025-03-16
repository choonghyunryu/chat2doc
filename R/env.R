#' Get API keys from package file
#' @description 패키지 파일에 등록된 openai API key와 anthropic API key, google API key를 조회합니다.
#' @details regist_api_key()를 사용하지 않고, set_api_key()로 API key를 설정한 경우라면, get_api_key() 대신에
#' Sys.getenv("OPENAI_API_KEY"), Sys.getenv("ANTHROPIC_API_KEY"), Sys.getenv("GOOGLE_API_KEY"),
#' Sys.getenv("XAI_API_KEY")를 사용하세요.
#' @examples
#' \dontrun{
#' # get_api_key()
#' }
#' @export
#' @import dplyr
#' @importFrom base64enc base64decode
get_api_key <- function() {
  openai_file <- system.file(".openaiKey", package = "chat2doc")
  anthropic_file  <- system.file(".anthropicKey", package = "chat2doc")
  google_file  <- system.file(".googleKey", package = "chat2doc")
  xai_file  <- system.file(".xaiKey", package = "chat2doc")

  # for openai API key
  if (openai_file != "") {
    con <- file(openai_file, "r")

    tryCatch({
      openai_api_key <- readLines(con) %>%
        base64enc::base64decode() %>%
        rawToChar()
    },
    finally = {
      close(con)
    })
  } else {
    openai_api_key = NULL
  }

  # for anthropic API key
  if (anthropic_file != "") {
    con <- file(anthropic_file, "r")

    tryCatch({
      anthropic_api_key <- readLines(con) %>%
        base64enc::base64decode() %>%
        rawToChar()
    },
    finally = {
      close(con)
    })
  } else {
    anthropic_api_key = NULL
  }

  # for google API key
  if (google_file != "") {
    con <- file(google_file, "r")

    tryCatch({
      google_api_key <- readLines(con) %>%
        base64enc::base64decode() %>%
        rawToChar()
    },
    finally = {
      close(con)
    })
  } else {
    google_api_key = NULL
  }

  # for xAI API key
  if (xai_file != "") {
    con <- file(xai_file, "r")

    tryCatch({
      xai_api_key <- readLines(con) %>%
        base64enc::base64decode() %>%
        rawToChar()
    },
    finally = {
      close(con)
    })
  } else {
    xai_api_key = NULL
  }

  list(openai_api_key = openai_api_key,
       anthropic_api_key = anthropic_api_key,
       google_api_key = google_api_key,
       xai_api_key = xai_api_key)
}


#' Set API key to system environment
#' @description 모델과 인터페이스하기 위한 회사별 API key를 설정합니다.
#' @param company character. 등록할 API key의 회사명.
#' @param api_key character. 등록할 API key.
#' @details 만약에 여러 사용자가 사용하는 환경이 아닌 개인 컴퓨터에 chat2doc 패키지를 설치한 경우라면,
#' set_api_key() 대신에 매번 API key를 등록할 필요없는 regist_api_key()를 사용하세요.
#' @examples
#' \dontrun{
#' # 실제 사용자가 할당받은 openai API key를 사용합니다.
#' # set_api_key("openai", "XXXXXXXXXXX")
#' }
#' @export
set_api_key <- function(company = c("openai", "anthropic", "google", "xai"), api_key = NULL) {
  company <- match.arg(company)

  if (is.null(api_key)) {
    stop("API key is required.")
  }

  company <- paste0(toupper(company), "_API_KEY")

  do.call(Sys.setenv, setNames(list(api_key), company))
}

#' Regist API key to package file
#' @description openai와 인터페이스하기 위한 openai API key를 등록합니다.
#' @param company character. 등록할 API key의 회사명.
#' @param api_key character. 등록할 API key.
#' @details 만약에 개인 컴퓨터가 아닌 여러 사용자가 사용하는 환경에 chat2doc 패키지를 설치한 경우라면,
#' API key의 보안을 위해서 regist_api_key()대신 set_api_key()를 사용하세요.
#' @examples
#' \dontrun{
#' # 실제 사용자가 할당받은 openai API key를 사용합니다.
#' # regist_api_key("openai", "XXXXXXXXXXX")
#' }
#' @export
#' @import dplyr
#' @importFrom base64enc base64encode
regist_api_key <- function(company = c("openai", "anthropic", "google", "xai"), api_key = NULL) {
  company <- match.arg(company)

  if (is.null(api_key)) {
    stop("API key is required.")
  }

  key_file <- file.path(system.file(package = "chat2doc"), paste0(".", company, "Key"))

  decode_api_key <- api_key %>%
    charToRaw() %>%
    base64enc::base64encode()

  if (!file.exists(key_file)) {
    con <- file(key_file, "w")
    tryCatch({
      cat(decode_api_key, file = con, sep = "\n")
    }, finally = {
      close(con)
    })
  }

  set_api_key(company, api_key)
}


set_gptenv <- function(name, value) {
  assign(name, value, envir = .chat2docEnv)
}

unset_gptenv <- function(name) {
  value <- get_gptenv(name)
  if (!is.null(value)) {
    rm(list = name, envir = .chat2docEnv)
  }
}

get_gptenv <- function(name) {
  if (missing(name)) {
    as.list(.chat2docEnv)
  } else {
    .chat2docEnv[[name]]
  }
}

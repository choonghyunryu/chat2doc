#' Get API keys from package file
#' @description 패키지 파일에 등록된 openai API key와 anthropic API key를 조회합니다.
#' @details regist_openai_key(), regist_anthropic_key()를 사용하지 않고, set_api_key(),
#' set_anthropic_key()로 API key를 설정한 경우라면, get_api_key() 대신에
#' Sys.getenv("OPENAI_API_KEY"), Sys.getenv("ANTHROPIC_API_KEY")를 사용하세요.
#' @examples
#' \dontrun{
#' # get_api_key()
#' }
#' @export
#' @import dplyr
#' @importFrom base64enc base64decode
get_api_key <- function() {
  openai_file <- system.file(".openapiKey", package = "chat2doc")
  anthropic_file  <- system.file(".anthropicKey", package = "chat2doc")

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

  list(openai_api_key = openai_api_key,
       anthropic_api_key = anthropic_api_key)
}


#' Set openai API key to system environment
#' @description openai와 인터페이스하기 위한 openai API key를 설정합니다.
#' @param api_key character. 등록할 openai API key.
#' @details 만약에 여러 사용자가 사용하는 환경이 아닌 개인 컴퓨터에 chat2doc 패키지를 설치한 경우라면,
#' set_openai_key() 대신에 매번 API key를 등록할 필요없는 regist_openai_key()를 사용하세요.
#' @examples
#' \dontrun{
#' # 실제 사용자가 할당받은 openai API key를 사용합니다.
#' # set_openai_key("XXXXXXXXXXX")
#' }
#' @export
set_openai_key <- function(api_key = NULL) {
  if (is.null(api_key)) {
    stop("OpanAI API key is required.")
  }

  Sys.setenv(
    OPENAI_API_KEY = api_key
  )
}


#' Set anthropic API key to system environment
#' @description anthropic AI와 인터페이스하기 위한 anthropic API key를 설정합니다.
#' @param api_key character. 등록할 anthropic API key.
#' @details 만약에 여러 사용자가 사용하는 환경이 아닌 개인 컴퓨터에 chat2doc 패키지를 설치한 경우라면,
#' set_anthropic_key() 대신에 매번 API key를 등록할 필요없는 regist_anthropic_key()를 사용하세요.
#' @examples
#' \dontrun{
#' # 실제 사용자가 할당받은 anthropic API key를 사용합니다.
#' # set_anthropic_key("XXXXXXXXXXX")
#' }
#' @export
set_anthropic_key <- function(api_key = NULL) {
  if (is.null(api_key)) {
    stop("anthropic API key is required.")
  }
  Sys.setenv(
    ANTHROPIC_API_KEY = api_key
  )
}


#' Regist openai API key to package file
#' @description openai와 인터페이스하기 위한 openai API key를 등록합니다.
#' @param api_key character. 등록할 openai API key.
#' @details 만약에 개인 컴퓨터가 아닌 여러 사용자가 사용하는 환경에 chat2doc 패키지를 설치한 경우라면,
#' API key의 보안을 위해서 regist_openai_key()대신 set_openai_key()를 사용하세요.
#' @examples
#' \dontrun{
#' # 실제 사용자가 할당받은 openai API key를 사용합니다.
#' # regist_openai_key("XXXXXXXXXXX")
#' }
#' @export
#' @import dplyr
#' @importFrom base64enc base64encode
regist_openai_key <- function(api_key = NULL) {
  if (is.null(api_key)) {
    stop("OpanAI API key is required.")
  }

  key_file <- file.path(system.file(package = "chat2doc"), ".openapiKey")

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

  Sys.setenv(
    OPENAI_API_KEY = api_key
  )
}


#' Regist anthropic API key to package file
#' @description anthropic과 인터페이스하기 위한 anthropic API key를 등록합니다.
#' @param api_key character. 등록할 anthropic API key.
#' @details 만약에 개인 컴퓨터가 아닌 여러 사용자가 사용하는 환경에 chat2doc 패키지를 설치한 경우라면,
#' API key의 보안을 위해서 regist_anthropic_key()대신 set_anthropic_key()를 사용하세요.
#' @examples
#' \dontrun{
#' # 실제 사용자가 할당받은 anthropic API key를 사용합니다.
#' # regist_anthropic_key("XXXXXXXXXXX")
#' }
#' @export
#' @import dplyr
#' @importFrom base64enc base64encode
regist_anthropic_key <- function(api_key = NULL) {
  if (is.null(api_key)) {
    stop("anthropic API key is required.")
  }

  key_file <- file.path(system.file(package = "chat2doc"), ".anthropicKey")

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

  Sys.setenv(
    ANTHROPIC_API_KEY = api_key
  )
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

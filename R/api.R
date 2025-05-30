#' List openai models
#' @description 현재 사용 가능한 모델을 나열하고 소유자 및 생성일자 등,
#' 각 모델에 대한 기본 정보를 제공.
#' @param api_key character. openai API key.
#' @return a data.frame.
#' Names of data.frame is as follows.
#' \itemize{
#'   \item id : 모델의 고유 식별자. 이 식별자는 모델을 호출하거나 다른 API 요청에서 참조할 때 사용됨.
#'   \item object : 객체의 종류를 나타내는 문자열. 모든 "List models" API 결과의 object 값은 "model".
#'   \item created : 모델이 생성된 날짜와 시간.
#'   \item owned_by : 모델의 오너.
#' }
#' @examples
#' \dontrun{
#' list_models_openai()
#' }
#' @export
#' @importFrom httr GET add_headers content_type_json status_code
#' @importFrom lubridate as_datetime
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom dplyr mutate
#' @name list_models_openai
#' @rdname list_models_openai
list_models_openai <- function(api_key = Sys.getenv("OPENAI_API_KEY")) {
  # OpenAI 엔드포인트 URL
  url <- "https://api.openai.com/v1/models"

  # API 요청
  response <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json())

  # 응답 확인
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")

    models <- jsonlite::fromJSON(content) |>
      purrr::pluck("data") |>
      as.data.frame() |>
      dplyr::mutate(created = lubridate::as_datetime(created))

    # API 목록 출력
    return(models)
  } else {
    cat(paste("Error:", httr::status_code(response), "\n"))
  }
}



#' List anthropic models
#' @description 현재 사용 가능한 모델을 나열하고 소유자 및 생성일자 등,
#' 각 모델에 대한 기본 정보를 제공.
#' @param api_key character. anthropic API key.
#' @return a data.frame.
#' Names of data.frame is as follows.
#' \itemize{
#'   \item type : 모델의 유형.
#'   \item id : 모델의 고유 식별자. 이 식별자는 모델을 호출하거나 다른 API 요청에서 참조할 때 사용됨.
#'   \item display_name : 모델의 이름.
#'   \item created_at : 모델이 생성된 날짜와 시간.
#' }
#' @examples
#' \dontrun{
#' list_models_anthropic()
#' }
#' @export
#' @importFrom httr GET add_headers content_type_json status_code
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom dplyr mutate
#' @rdname list_models_openai
list_models_anthropic <- function(api_key = Sys.getenv("ANTHROPIC_API_KEY")) {
  # anthropic 엔드포인트 URL
  url <- "https://api.anthropic.com/v1/models"

  # API 요청
  response <- httr::GET(
    url,
    httr::add_headers(`x-api-key` = api_key,  # API 키 헤더 추가
                      `anthropic-version` = "2023-06-01" # API 버전 지정
                      ),
    httr::content_type_json())

  # 응답 확인
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")

    models <- jsonlite::fromJSON(content) |>
      purrr::pluck("data") |>
      as.data.frame()

    # API 목록 출력
    return(models)
  } else {
    cat(paste("Error:", httr::status_code(response), "\n"))
  }
}


#' List xAI models
#' @description 현재 사용 가능한 모델을 나열하고 소유자 및 생성일자 등,
#' 각 모델에 대한 기본 정보를 제공.
#' @param api_key character. xAI API key.
#' @return a data.frame.
#' Names of data.frame is as follows.
#' \itemize{
#'   \item id : 모델의 고유 식별자. 이 식별자는 모델을 호출하거나 다른 API 요청에서 참조할 때 사용됨.
#'   \item created : 모델 생성일자.
#'   \item object : 모델 객체의 종류를 나타내는 문자열.
#'   \item owned_by : 모델의 오너.
#' }
#' @examples
#' \dontrun{
#' list_models_xai()
#' }
#' @export
#' @importFrom httr GET add_headers content_type_json status_code
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom dplyr mutate
#' @rdname list_models_openai
list_models_xai <- function(api_key = Sys.getenv("XAI_API_KEY")) {
  # anthropic 엔드포인트 URL
  url <- "https://api.x.ai/v1/models"

  # API 요청
  response <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json())

  # 응답 확인
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")

    models <- jsonlite::fromJSON(content) |>
      purrr::pluck("data") |>
      as.data.frame() |>
      mutate(created = lubridate::as_datetime(created))

    # API 목록 출력
    return(models)
  } else {
    cat(paste("Error:", httr::status_code(response), "\n"))
  }
}


#' Chat with a xAI model
#' @description xAI의 Grok API를 사용하여 모델과 대화합니다.
#' @param system_prompt character. 시스템 프롬프트.
#' @param model character. 모델의 이름. 기본값은 "grok-2-latest".
#' @param api_key character. xAI API key.
#' @return A Chat object.
#' @examples
#' \dontrun{
#' chat <- chat_grok()
#' chat$chat("Tell me three jokes about statisticians")
#' }
#' @import ellmer
#' @export
chat_grok <- function(system_prompt = NULL,
                      model = "grok-2-latest",
                      api_key = Sys.getenv("XAI_API_KEY")) {
  # xAI 엔드포인트 URL
  url <- "https://api.x.ai/v1/"

  # https://github.com/tidyverse/ellmer/issues/216
  chat_openai(
    system_prompt = system_prompt,
    model = model,
    base_url = url,
    api_key = api_key
    )
}


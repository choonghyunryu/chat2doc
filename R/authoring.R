#' Convert Chat to Markdown
#' @description Chat 결과를 Markdown 형식으로 변환합니다.
#' @param x A Chat object.
#' @param title_levels logical. 제목 수준을 유지할지 여부를 결정합니다.
#' @return character.
#' @examples
#' \dontrun{
#' chat2markdown(x)
#' }
#' @export
#' @import ellmer
#' @import stringr
chat2markdown <- function(x, title_levels = TRUE) {
  chat_text <- contents_markdown(x) |>
    trim_before_string("Assistant\n\n")

  first_char <- str_sub(chat_text, 1, 1)
  match_result <- str_extract(chat_text, paste0("^", first_char, "+")) |>
    str_length()

  if (title_levels & match_result > 1) {
    chat_text <- contents_markdown(x) |>
      trim_before_string("Assistant\n\n") |>
      str_replace_all("# ", " ")
  }

  return(chat_text)
}


#' Outputs Chat to Markdown
#' @description Chat 결과를 Markdown 형식으로 출력합니다.
#' @param x A Chat object.
#' @param title_levels logical. 제목 수준을 유지할지 여부를 결정합니다.
#' @examples
#' \dontrun{
#' cat_chat(x)
#' }
#' @export
#' @import ellmer
#' @import stringr
cat_chat <- function(x, title_levels = TRUE) {
  chat2markdown(x, title_levels) |>
    cat()
}


#' List sub-titles in Chat
#' @description Chat 결과에서 서브 타이틀들을 추출
#' @param x A Chat object.
#' @return character.
#' @examples
#' \dontrun{
#' extract_subtitles(x)
#' }
#' @export
#' @import stringr
extract_subtitles <- function(x) {
  chat2markdown(x) |>
    str_extract_all("## (.+?)\n") |>
    unlist() |>
    str_remove("## ") |>
    str_remove("\n")
}


#' Edit text
#' @description Chat 결과를 요약하거나 편집
#' @param x A Chat object.
#' @param type character. 요약 또는 편집 중 하나를 선택. "summary", "edit" 중 하나.
#' @return character.
#' @examples
#' \dontrun{
#' list_models_openai()
#' }
#' @export
#' @import ellmer
edit_openai <- function(x, type = c("summary", "edit")) {
  type <- match.arg(type)

  type_summary <- type_object(
    "요약",
    summary = type_string("최대 3~4 문단으로 내용을 요약해 줘.")
  )

  type_edit <- type_object(
    "편집",
    edit    = type_string("문장이 지나치게 길거나 복잡한 경우 가독성을 높이도록 개선해 줘.")
  )

  .chat <- chat_openai()

  if (type == "summary") {
    data <- .chat$extract_data(contents_markdown(x), type = type_summary)
    return(data$summary)
  } else if (type == "edit") {
    data <- .chat$extract_data(contents_markdown(x), type = type_edit)
    return(data$edit)
  }
}

#' Trim before Text
#' @description 특정 문자열 및 이전의 모든 문자 제거
#' @param text character. original text.
#' @param pattern character. pattern to base.
#' @return character.
#' @examples
#' \dontrun{
#' text <- "이것은 예제입니다. 삭제할 부분을 포함한 문자열입니다."
#' pattern <- "니"
#' trim_before_string(text, pattern)
#' }
#' @export
trim_before_string <- function(text, pattern) {
  # 정규 표현식을 사용하여 특정 문자열 및 이전의 모든 문자 제거
  trimmed_vector <- sub(paste0(".*?", pattern), "", text)
  return(trimmed_vector)
}


#' Remove before Text
#' @description 모든 특정 문자열 앞의 문자열 제거
#' @param text character. original text.
#' @param pattern character. pattern to base.
#' @return character.
#' @examples
#' \dontrun{
#' text <- "이것은 예제입니다. 삭제할 부분을 포함한 문자열입니다."
#' pattern <- "니"
#' remove_before_string(text, pattern)
#' }
#' @export
#' @rdname trim_before_string
remove_before_string <- function(text, pattern) {
  sub(paste0(".*", pattern), pattern, text)
}


#' Remove before Text by First Pattern
#' @description 첫 특정 문자열 앞의 문자열을 제거
#' @param text character. original text.
#' @param pattern character. pattern to base.
#' @return character.
#' @examples
#' \dontrun{
#' text <- "이것은 예제입니다. 삭제할 부분을 포함한 문자열입니다."
#' pattern <- "니"
#' remove_before_first(text, pattern)
#' }
#' @export
#' @rdname trim_before_string
remove_before_first <- function(text, pattern) {
  match_pos <- regexpr(pattern, text)  # 첫 번째 패턴의 위치 찾기

  if (match_pos == -1) {
    return(text)  # 패턴이 없으면 원본 텍스트 반환
  }

  substr(text, match_pos, nchar(text))  # 첫 번째 패턴부터 끝까지 추출
}


#' Remove before Text by First Pattern
#' @description 두 문자열 패턴 사이의 문자열 추출
#' @param text character. original text.
#' @param start_pattern character. start pattern.
#' @param end_pattern character. end pattern.
#' @return character.
#' @examples
#' \dontrun{
#' text <- "이것은 예제입니다. 삭제할 부분을 포함한 문자열입니다."
#' spattern <- "니"
#' epattern <- "삭"
#' extract_between(text, spattern, epattern)
#' }
#' @export
#' @import stringr
#' @rdname trim_before_string
extract_between <- function(text, start_pattern, end_pattern) {
  # start_pattern에 괄호가 포함되어 있을 경우, 이를 이스케이프 처리
  start_pattern <- str_replace_all(start_pattern, "([()])", "\\\\\\1")

  match_result <- str_extract(text, paste0("(?<=", start_pattern, ").*?(?=", end_pattern, ")"))

  return(ifelse(is.na(match_result), "", match_result))
}

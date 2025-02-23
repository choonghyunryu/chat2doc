#' @export
# 특정 문자열을 포함한 앞의 문자 및 해당 문자열 제거하는 함수
trim_before_string <- function(text_vector, keyword) {
  # 정규 표현식을 사용하여 특정 문자열 및 이전의 모든 문자 제거
  trimmed_vector <- sub(paste0(".*?", keyword), "", text_vector)
  return(trimmed_vector)
}


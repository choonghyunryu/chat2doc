#' @export
#' @import ellmer
chat2markdown <- function() {
  contents_markdown(chat_from_claude) |>
    trim_before_string("Assistant\n\n") |>
    cat()
}

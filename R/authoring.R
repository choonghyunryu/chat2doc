#' @export
#' @import ellmer
chat2markdown <- function(x) {
  contents_markdown(x) |>
    trim_before_string("Assistant\n\n") |>
    cat()
}

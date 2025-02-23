.chat2docEnv <- new.env()

.onAttach <- function(libname, pkgname) {
  if (Sys.getenv("OPENAI_API_KEY") == "") {
    api_key <- get_api_key()

    # set openai API key
    if (!is.null(api_key$openai_api_key)) {
      set_openai_key(api_key$openai_api_key)
    }
  }

  if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
    api_key <- get_api_key()

    # set enthropic API key
    if (!is.null(api_key$anthropic_api_key)) {
      set_anthropic_key(api_key$anthropic_api_key)
    }
  }
}

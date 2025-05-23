.chat2docEnv <- new.env()

.onAttach <- function(libname, pkgname) {
  if (Sys.getenv("OPENAI_API_KEY") == "") {
    api_key <- get_api_key()

    # set openai API key
    if (!is.null(api_key$openai_api_key)) {
      set_api_key("openai", api_key$openai_api_key)
    }
  }

  if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
    api_key <- get_api_key()

    # set anthropic API key
    if (!is.null(api_key$anthropic_api_key)) {
      set_api_key("anthropic", api_key$anthropic_api_key)
    }
  }

  if (Sys.getenv("GOOGLE_API_KEY") == "") {
    api_key <- get_api_key()

    # set google API key
    if (!is.null(api_key$google_api_key)) {
      set_api_key("google", api_key$google_api_key)
    }
  }

  if (Sys.getenv("XAI_API_KEY") == "") {
    api_key <- get_api_key()

    # set google API key
    if (!is.null(api_key$xai_api_key)) {
      set_api_key("xai", api_key$xai_api_key)
    }
  }
}

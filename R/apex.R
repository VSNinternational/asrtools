apex <- function(
  x = NULL,
  np = NULL,
  default = NULL,
  message = TRUE
) {
  if (!is.null(np) && !is.null(default)) {
    stop("np and default should not be non-NULL in the same call.")
  }
  if (is.null(np)) {
    if (!is.null(default)) {
      options(asr.np = default)
      if (message) {
        message(
          "Default `np` changed to `c(",
          paste0(default, collapse = ", "),
          ")`."
        )
      }
      return(invisible())
    }
    if (is.null(default)) {
      if (!is.null(.Options$asr.np)) {
        np <- .Options$asr.np
      }
    }
  }
  if (is.null(np)) {
    stop("No `np` value available for indices.")
  }
  cat(nrow(x), "x", ncol(x), class(x)[1], "\n")
  if (length(np) == 1) {
    return(head(x, c(np[1], np[1])))
  } else {
    return(head(x, np))
  }
}
## Option with own environment (consider this if we have too many hidden options)

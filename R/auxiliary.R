#' @export
.silence <- function(.code) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(.code))
}
.catch_warnings <- function(.expr, .where = .GlobalEnv) {
  warning_handler <- function(wh) {
    if (exists("warning_message", envir = .where)) {
      obj <- get("warning_message", envir = .where)
      obj <- paste(obj, wh$message, sep = " / ")
      assign("warning_message", obj, envir = .where)
    } else {
      assign("warning_message", wh$message, envir = .where)
    }
    invokeRestart("muffleWarning")
  }
  withCallingHandlers(.expr, warning = warning_handler)
}
#' @export
.replace_operators <- function(
  .object = NULL,
  .replacement = "_",
  .message = TRUE
) {
  math.operators <- "\\-|\\*|\\/|\\%|\\+|\\:"
  obj_name <- deparse(substitute(.object))
  if (.message) {
    message(col_blue(
      "\nVerifying if names in `",
      obj_name,
      "` need to be modified."
    ))
  }
  if (any(grepl(pattern = math.operators, x = .object))) {
    if (.message) {
      message(
        "Names in `",
        obj_name,
        "` contain mathematical operators (e.g., + - * / % :), ",
        "which will be replaced by ",
        .replacement,
        "."
      )
    }
    new_names <-
      sapply(
        X = .object,
        FUN = gsub,
        pattern = math.operators,
        replacement = .replacement,
        USE.NAMES = FALSE
      )
    return(new_names)
  }
}
#' @export
.by.stats <- function(.data = NULL, .index = NULL, .var = NULL) {
  inner.by.stats_ <- function(.data = NULL, .var = NULL) {
    curvar <- .data[[.var]]
    na.pos <- is.na(curvar)
    curvar <- curvar[!na.pos]
    n <- length(curvar)
    if (length(curvar) > 0) {
      min <- min(curvar)
      mean <- mean(curvar)
      max <- max(curvar)
      sd <- sd(curvar)
      missing <- sum(na.pos)
      CVp <- 100 * sd / mean
    } else {
      min <- mean <- max <- sd <- CVp <- as.numeric(NA)
      missing <- sum(na.pos)
    }
    data.table(n, min, mean, max, sd, missing, CVp)
  }
  .data <- as.data.table(.data)
  var_ <- .var
  return(.data[, inner.by.stats_(.data = .SD, .var = ..var_), by = .index])
}

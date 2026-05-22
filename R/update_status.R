#' Update the status of a process
#'
#' This function sends a POST request to the endpoint specified in the
#' environment variable ENDPOINT. The request body is a JSON object with the
#' progress, context, and category
#'
#' @param progress An integer representing the progress of the process
#' (default = \code{NULL}).
#' @param context A character string describing the context of the process
#' (default = \code{NULL}).
#' @param category A character string describing the category of the process
#' (default = \code{NULL}).
#' @param verbose A logical value indicating whether to print
#' information (default = \code{FALSE}).
#'
#' @details A server must be running at the endpoint specified in the
#' environment variable ENDPOINT for this function to work. The server must
#' accept POST requests with a JSON body containing the aforementioned keys.
#' The older httr packages is used instead of the newer httr2 as it has
#' fewer dependencies and generates smaller Docker images.
#'
#' @return The response from the server.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(httr)
#' # Set the endpoint
#' Sys.setenv(ENDPOINT = "http://localhost:3000/...")
#' status <- status_update(50L, "Processing data", "Data processing")
#' }
.update_status <- function(
  progress = NULL,
  context = NULL,
  category = NULL,
  verbose = FALSE
) {
  if (typeof(progress) != "integer") {
    stop("progress must be an integer")
  }
  if (typeof(context) != "character") {
    stop("context must be a character")
  }
  if (typeof(category) != "character") {
    stop("category must be a character")
  }
  url <- Sys.getenv("ENDPOINT")
  if (url == "") {
    stop("environment variable ENDPOINT not set.\n", call. = FALSE)
  }
  body <- list(
    progress = progress,
    context = context,
    category = category
  )
  request_log <- tryCatch(
    expr = POST(
      url,
      body = body,
      encode = "json",
      content_type("application/json")
    ),
    error = function(e) conditionMessage(e)
  )
  if (verbose) {
    if (is(request_log, "response")) {
      content(request_log) |>
        toJSON(pretty = TRUE, auto_unbox = TRUE) |>
        print()
      cat("\n")
    } else {
      paste0("WARNING: ", request_log, " [", url, "]\n") |> cat()
      body |>
        toJSON(pretty = TRUE, auto_unbox = TRUE) |>
        print()
      cat("\n")
    }
  }
  invisible(request_log)
}
#' Same as \code{.update_status} but asynchronously
#'
#' This function sends a POST request to the endpoint specified in the
#' environment variable ENDPOINT. The request body is a JSON object with the
#' progress, context, and category. The request is sent asynchronously using
#' a system call to \code{curl}.
#'
#' @param progress An integer representing the progress of the process
#' (default = \code{NULL}).
#' @param context A character string describing the context of the process
#' (default = \code{NULL}).
#' @param category A character string describing the category of the process
#' (default = \code{NULL}).
#' @param verbose A logical value indicating whether to print
#' information (default = \code{FALSE}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv(ENDPOINT = "http://localhost:3000/...")
#' .async_update_status(10L, "loading data", "inProgress", verbose = TRUE)
#' }
.async_update_status <- function(
  progress = NULL,
  context = NULL,
  category = NULL,
  verbose = FALSE
) {
  if (typeof(progress) != "integer") {
    stop("progress must be an integer")
  }
  if (typeof(context) != "character") {
    stop("context must be a character")
  }
  if (typeof(category) != "character") {
    stop("category must be a character")
  }
  url <- Sys.getenv("ENDPOINT")
  timeout <- Sys.getenv("VSNI_TIMEOUT")
  if (url == "") {
    stop("ERROR: environment variable ENDPOINT not set.\n", call. = FALSE)
  }
  if (timeout == "") {
    cat("NOTE: environment variable VSNI_TIMEOUT not set; using 5s.\n")
    timeout <- 5
  } else {
    timeout <- as.numeric(timeout)
  }
  body <- paste0(
    '{"',
    'progress": ',
    progress,
    ', "context": "',
    context,
    '", "category": "',
    category,
    '"}'
  )
  if (verbose) {
    body |> cat("\n\n")
  }
  curling <- paste0(
    "curl -m ",
    timeout,
    " -X POST '",
    url,
    "' -H 'Content-Type: application/json' -d '",
    body,
    "'"
  )
  request_log <- tryCatch(
    expr = system(
      curling,
      wait = FALSE,
      ignore.stdout = !verbose,
      ignore.stderr = !verbose,
      intern = FALSE
    ),
    error = function(e) conditionMessage(e)
  )
  if (verbose) {
    if (is(request_log, "character")) {
      cat(
        "ERROR: failed to execure HTTP request via system call, check R code:\n"
      )
      cat(request_log)
      cat("\n")
    }
  }
  invisible()
}

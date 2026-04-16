#' @keywords internal
"_PACKAGE"
## usethis namespace: start
#' @import plyr
#' @import data.table
#' @import jsonlite
#' @import httr
#' @import cli
#' @importFrom methods  getFunction is
#' @importFrom stats cor lm sd
#' @importFrom utils capture.output head
## usethis namespace: end
NULL
utils::globalVariables(c("<<-", "..var_", "mod_train", "vpredict"))
.onLoad <- function(libname, pkgname) {
  options(asr.np = c(5))
}

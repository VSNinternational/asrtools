#' @export
.argument_class <- function(.data = NULL, .class = NULL) {
  data.name_ <- deparse(substitute(.data))
  if (!any(class(.data) %in% .class)) {
    stop(paste0(
      "The object passed to ",
      data.name_,
      " argument should be of class(es) ",
      paste0(.class, collapse = " or "),
      "."
    ))
  }
}
#' @export
.variable_class <- function(
  .data = NULL,
  .mandatory = FALSE,
  .variable = NULL,
  .class = "factor",
  .class.action = "stop",
  .mutate = FALSE,
  .message = message
) {
  data.name_ <- deparse(substitute(.data))
  var.name_ <- deparse(substitute(.variable))
  class.fun_ <- paste0("is.", .class)
  variable.name_ <- get(var.name_, envir = parent.frame())
  if (!is.null(variable.name_)) {
    for (cur.variable.name_ in variable.name_) {
      if (!cur.variable.name_ %in% names(.data)) {
        stop(paste0(
          "\'",
          cur.variable.name_,
          "' does not correspond to a variable name of '",
          data.name_,
          "'."
        ))
      }
      if (!getFunction(class.fun_)(.data[[cur.variable.name_]])) {
        if (!(.class.action == "message" && isFALSE(.message)) && !.mutate) {
          getFunction(.class.action)(
            paste0(
              "Variable \'",
              cur.variable.name_,
              "' must be of class \'",
              .class,
              "'."
            )
          )
        }
        if (.mutate) {
          .data[[cur.variable.name_]] <- getFunction(paste0(
            "as.",
            .class
          ))(.data[[cur.variable.name_]])
          assign(x = data.name_, value = .data, envir = parent.frame())
          if (.message) {
            message(
              "Coercing `",
              cur.variable.name_,
              "` to class `",
              .class,
              "`."
            )
          }
        }
      }
    }
  }
  if (is.null(variable.name_) && .mandatory) {
    stop(paste0("The variable \'", var.name_, "' is mandatory."))
  }
}

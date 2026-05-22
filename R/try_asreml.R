#' @title Error handling for ASReml-R
#'
#' @param call The asreml call to be evaluated provided as character.
#' @param updates The maximum number of tries to update the model if it
#' has not converged yet.
#'
#' @return A list with the exit status and the model object if it converged.
#'
#' @export
.try_asreml <-
  function(
    x = "mod",
    call = NULL,
    updates = 2,
    env = parent.frame(),
    message = TRUE,
    ...
  ) {
    debug_flag <- exists("DebugEnv")
    ErrorEnv <- new.env()
    if (debug_flag) {
      DebugEnv$time_delta <- proc.time()[3]
    }
    tryCatch(
      expr = .catch_warnings(
        .expr = eval(parse(text = paste0(x, "<-", call)), envir = env),
        .where = ErrorEnv
      ),
      error = function(e) {
        assign("error_message", conditionMessage(e), envir = ErrorEnv)
      }
    )
    if (debug_flag) {
      DebugEnv$time_delta <- proc.time()[3] - DebugEnv$time_delta
    }
    if (!exists(x, envir = env) || is.null(get(x, envir = env))) {
      if (message) {
        message("Error: see `$error` for more information.\n")
      }
      return(list(
        converged = FALSE,
        warning = ErrorEnv$warning_message,
        error = ErrorEnv$error_message
      ))
    }
    cur_try <- 1
    while (!get(x, envir = env)$converge) {
      ErrorEnv <- new.env()
      if (debug_flag) {
        cur_timing <- proc.time()[3]
      }
      if (message) {
        message("MESSAGE: base model has not converged. Updating.\n")
      }
      tryCatch(
        expr = .catch_warnings(
          .expr = eval(
            parse(text = paste0(x, "<-update(", x, ")")),
            envir = env
          ),
          .where = ErrorEnv
        ),
        error = function(e) {
          assign("error_message", conditionMessage(e), envir = ErrorEnv)
        }
      )
      if (debug_flag) {
        DebugEnv$time_delta <- append(
          DebugEnv$time_delta,
          proc.time()[3] - cur_timing
        )
      }
      if (exists("error_message", envir = ErrorEnv)) {
        if (message) {
          message("Error: see `$error` for more information.\n")
        }
        return(list(
          converged = FALSE,
          warning = ErrorEnv$warning_message,
          error = ErrorEnv$error_message
        ))
      }
      if (cur_try == updates) {
        return(list(
          converged = FALSE,
          warning = ErrorEnv$warning_message,
          error = paste0("Model did not converge after ", updates, " updates!")
        ))
      }
      cur_try <- cur_try + 1
    }
    if (message) {
      message("...model converged.\n")
    }
    return(list(
      converged = TRUE,
      warning = ErrorEnv$warning_message,
      error = ErrorEnv$error_message
    ))
  }

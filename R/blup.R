#' Gets EBLUPs from an \code{asreml} object.
#'
#' @param object An \code{asreml} object (default = \code{NULL}).
#' @param terms A character vector with the names of the terms to be extracted
#' from the \code{object} (default = \code{NULL}).
#' @param tidy If \code{TRUE}, the \code{terms} are presented in the style
#' of \code{predict.asreml} (default = \code{TRUE}).
#'
#' @details
#' This function will work for simple random terms. It does not currently
#' supports \code{at()} and other constructors from \code{asreml}.
#'
#' @return A list of class \code{blup.asreml} with the EBLUPs from
#' \code{object}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(asreml)
#'
#' oats.asr <- asreml(yield ~ Variety*Nitrogen,
#'   random = ~ Blocks/Wplots, data = oats)
#'
#' .blup(oats.asr, terms = c("Blocks:Wplots"))
#'
#' }
.blup <- function(object, terms = NULL, tidy = TRUE) {
  if (!all(terms %in% names(object$vparameters))) {
    stop(
      "At least one term is not present in the model. Check names(",
      deparse(substitute(object)),
      "$vparameters) for reference.",
      call. = FALSE
    )
  }
  blups <- summary(object, coef = TRUE)$coef.random
  blups <- lapply(terms, function(vc) {
    vc <- strsplit(x = vc, split = ":")[[1]]
    split_coef_names <- strsplit(x = rownames(blups), split = ":")
    match_term_number_index <-
      lapply(split_coef_names, function(x) length(x) == length(vc)) |> unlist()
    coef_idx <- lapply(split_coef_names, function(co) {
      c(sapply(vc, grepl, x = co, fixed = TRUE)) |> sum() == length(co)
    }) |>
      unlist()
    coef_idx <- match_term_number_index & coef_idx
    cur_blups <- blups[coef_idx, ]
    split_coef_names <- split_coef_names[coef_idx]
    if (tidy) {
      coef_levels <- lapply(split_coef_names, function(ci) {
        strsplit(x = ci, split = paste0(vc, "_"), fixed = TRUE) |>
          sapply(X = _, function(x) x[2])
      })
      coef_levels <- coef_levels |>
        do.call(what = cbind, args = _) |>
        t()
      colnames(coef_levels) <- vc
      final_frame <- cbind.data.frame(coef_levels, cur_blups, row.names = NULL)
      final_frame[vc] <- lapply(final_frame[vc], as.factor)
      final_frame
    } else {
      cur_blups
    }
  })
  names(blups) <- terms
  structure(
    blups,
    class = c("blup.asreml", "list")
  )
}

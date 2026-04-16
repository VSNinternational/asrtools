#' Creates a dummy genomic map
#'
#' @param marker_id vector with names of markers to compose dummy map.
#' @param message logical value indicating whether diagnostic messages should be printed on screen (default = \code{TRUE}).
#'
#' @return Data frame with dummy map. A single chromosome/linkage group is created and marker
#' distances are a sequence from one to the number of markers.
#'
#' @export
.dummy_map <- function(marker_id = NULL, message = TRUE) {
  if (message) {
    message("Creating dummy map.")
  }
  data.frame(
    marker = as.character(marker_id),
    chrom = 1L,
    pos = seq_along(marker_id)
  )
}

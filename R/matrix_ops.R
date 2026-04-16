#' Moore-Penrose pseudo-inverse
#'
#' @param G A matrix to be inverted (default = \code{NULL}).
#' @param eig.tol Defines relative relevance (\emph{i.e.}, non-zero) of eigenvalues
#' compared to the largest one. It determines which threshold of eigenvalues
#' will be treated as zero (default = \code{1e-06}).
#'
#' @return A Moore-Penrose pseudo-inverse of matrix G.
moore_penrose <- function(G = NULL, eig.tol = 1e-06) {
  G.dec <- svd(G)
  rel.thr <- max(0, eig.tol * G.dec$d[1])
  keep <- G.dec$d > rel.thr
  if (all(keep)) {
    return(
      G.dec$v %*% (1 / G.dec$d * t(G.dec$u))
    )
  } else {
    return(
      G.dec$v[, keep, drop = FALSE] %*%
        ((1 / G.dec$d[keep]) * t(G.dec$u[, keep, drop = FALSE]))
    )
  }
}
#' Performs Woodbury's inverted matrix update for row/column elimination
#'
#' @param X An inverted matrix to be updated (default = \code{NULL}).
#' @param na A vector indicating the indices of rows/columns to be dropped
#' (default = \code{NULL}).
#'
#' @return The updated matrix inverse without dropped rows/columns.
woodbury <- function(X = NULL, na = NULL) {
  A <- X[-na, -na, drop = FALSE]
  B <- X[-na, na, drop = FALSE]
  C <- X[na, -na, drop = FALSE]
  D <- X[na, na]
  return(A - B %*% solve(D) %*% C)
}
#' Performs Schulz-type inverted matrix update for row/column elimination
#'
#' @param X A matrix to be inverted (default = \code{NULL}).
#' @param Xinv.init An initial guess of the updated inverse (default =
#'   \code{NULL}).
#' @param na A vector indicating the indices of rows/columns to be dropped on
#'   \code{X} (default = \code{NULL}).
#' @param niter An integer indicating the number of iterations to carry out
#'   (theoretically, the more iterations, the closer the approximated inverse is
#'   to the true inverse) (default = \code{2}).
#'
#' @return The updated matrix inverse with 0 on dropped rows/columns.
#'
schulz <- function(X = NULL, Xinv.init = NULL, na = NULL, niter = 2) {
  update.range <- 1:niter
  I2 <- diag(x = 2, nrow = ncol(X))
  Xinv.init[na, ] <- 0
  Xinv.init[, na] <- 0
  for (i in update.range) {
    Xinv.init <- Xinv.init %*% (I2 - X %*% Xinv.init)
  }
  return(Xinv.init)
}

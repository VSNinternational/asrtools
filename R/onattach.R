.onAttach <- function(libname, pkgname) {
  ## This has been moved here so the environment is only set once.
  vsni.init()
  packageStartupMessage(pkgname, " attached from ", libname)
}
vsni.init <- function(askuser = FALSE, msgout = message) {
  return(TRUE)
}

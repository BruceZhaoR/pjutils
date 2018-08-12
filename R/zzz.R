
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the package, developing by Wei Zhao, Pingjia Technology. \nIf you have any question, please email `zhaowei@chinaubi.com`.")
}

.onUnload <- function(libpath) {
  library.dynam.unload("pjutils", libpath)
}

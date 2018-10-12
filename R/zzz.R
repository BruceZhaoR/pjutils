
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
  "Welcome to the package, developing by Wei Zhao,Pingjia Technology.
If you have any question, please email `zhaowei@chinaubi.com`."
  )
}

.onUnload <- function(libpath) {
  library.dynam.unload("pjutils", libpath)
}

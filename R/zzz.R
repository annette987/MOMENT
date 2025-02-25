.onAttach <- function(libname, pkgname) {
  # to show a startup message
  packageStartupMessage("Welcome to MOMENT - Multi-Omics Modelling by ENsemble Techniques")
}

.onLoad <- function(libname, pkgname) {
#  make_filters()
}

ignore_unused_imports <- function() {
  mlrCPO::cpoDropConstants
}



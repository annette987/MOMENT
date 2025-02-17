.onAttach <- function(libname, pkgname) {
  # to show a startup message
  packageStartupMessage("This is the MOMENT package")
	make_filters()
}

.onLoad <- function(libname, pkgname) {
  make_filters()
}



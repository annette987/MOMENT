#' @importFrom R6 R6Class
#' @importFrom future future
#' @importFrom mlrCPO makeCPOExtendedTrafo
#' @importFrom checkmate assert
#' @importFrom BBmisc makeS3Obj
#' @importFrom BBmisc vnapply
#' @importFrom dplyr bind_rows


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



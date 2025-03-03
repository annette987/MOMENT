expect_results <- function(object) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  if (!is.null(act$val) &&
			inherits(act$val, "MM_Results") &&
			!is.null(act$val$stab)
			) 
	{
    succeed()
    return(invisible(act$val))
  }

  message <- sprintf("%s has not returned any results")
  fail(message)
}
##################################
## tacmagic - PET Analysis in R ##
## loading.R                    ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################


#'@export
#'@noRd
print.tac <- function(x, ...) {

  if (!validate_tac(x)) {
	stop("Invalid tac object.")
  }

  else {
	cat("tac object\n", 
	    "Activity unit:          ", attributes(x)$activity_unit, "\n",
	    "Time unit:              ", attributes(x)$time_unit, "\n",
	    "Number of ROIs:         ", length(names(x)) - 2, "\n",
	    "Number of frames:       ", length(x$start), "\n",
	    "Time span:              ", x$start[1], "-", 
	                  x$end[length(x$end)], attributes(x)$time_unit, "\n",
	    "Unique frame durations: ", unique(x$end - x$start), 
	                  attributes(x)$time_unit, "\n"
	   )	
  }	
}

#'@export
#'@noRd
as.data.frame.tac <- function(x, ...) {
	class(x) <- "data.frame"
	return(x)
}

#'@noRd
is.tac <- function(x) {
	return (all(class(x) == c("tac", "data.frame"))) 
}

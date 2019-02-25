##################################
## tacmagic - PET Analysis in R ##
## tac_methods.R                ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#'@export
#'@noRd
summary.tac <- function(object, ...) {

  if (!validate_tac(object)) {
	stop("Invalid tac object.")
  }

  else {
	cat("tac object\n", 
	    "Activity unit:          ", attributes(object)$activity_unit, "\n",
	    "Time unit:              ", attributes(object)$time_unit, "\n",
	    "Number of ROIs:         ", length(names(object)) - 2, "\n",
	    "Number of frames:       ", length(object$start), "\n",
	    "Time span:              ", object$start[1], "-", 
	                                object$end[length(object$end)], 
	                                attributes(object)$time_unit, "\n",
	    "Unique frame durations: ", unique(object$end - object$start), 
	                  attributes(object)$time_unit, "\n"
	   )	
  }	
}

#' Creates a tac object from a data.frame
#'
#' tac objects can be created from data.frame objects with `as.tac()`. The time 
#' and activity units must be specified as arguments if not already set as 
#' attributes in the data.frame. The columns of the data frame are the regional
#' time activity curves, with the column names the names of the ROIs. 
#' 
#' If the time_unit and activity_unit attributes are already in the data.frame,
#' they do not need to be set again, but otherwise they will need to be
#' specified in the input parameters.
#' 
#'@export
#'@param x data.frame with start, end time and tac data
#'@param time_unit NULL if in data.frame or set to "seconds" or "minutes"
#'@param activity_unit NULL if in data.frame or set to "kBq/cc", "Bq/cc", 
#'                     "nCi/cc"
#'@return tac object
#'@family Loading functions 
#'@examples
#' manual <- data.frame(start=c(0:4), end=c(2:6), 
#'                      ROI1=c(10.1:14.2), ROI2=c(11:15))
#' manual_tac <- as.tac(manual, time_unit="minutes", activity_unit="kBq/cc")
as.tac <- function(x, time_unit=NULL, activity_unit=NULL) {

  if (!is.data.frame(x)) stop("Can only convert data.frames to tac object")
  if (is.null(time_unit) == is.null(x$time_unit)) stop("Time unit must be 
  	specified in exactly one of: supplied data.frame, or time_unit argument")
  if (is.null(activity_unit) == is.null(x$activity_unit)) stop("Activity unit 
  	must be in exactly one of: supplied data.frame or activity_unit argument")

  if (!is.null(time_unit)) attributes(x)$time_unit <- time_unit
  if (!is.null(activity_unit)) attributes(x)$activity_unit <- activity_unit
  class(x) <- c("tac", "data.frame")

  validate_tac(x)

  return(x)
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

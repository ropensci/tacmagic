##################################
## tacmagic - PET Analysis in R ##
## units.R                      ##
## (C) Eric E. Brown  2018-2019 ##
## Beta version--check all work ##
##################################


#' @noRd
get_activity_unit_index <- function() {
  # The equivalent to 1 kBq
  unit_index <- list(`Bq`  = 0.001,
                     `kBq` = 1,
                     `MBq` = 1000,
                     `nCi` = 0.037,
                     `uCi` = 37,
                     `mCi` = 37000,
                     `Ci`  = 37000000)
  return(unit_index)
}

#' Convert radioactivity units
#'
#' Change the radioactivity units of a tac or numeric object to the specified
#' desired units (e.g. Bq, kBq, MBq, nCi, uCi, mCi, Ci). For convenience, if the
#' unit is per volume ("x/cc" or "x/mL"), the "/cc" part is ignored for the 
#' conversion.
#'
#'@export 
#'@param x time-activity curve or numeric object
#'@param to_unit the desired unit (e.g. "kBq")
#'@param from_unit not used for tac object (it is in the tac object), but for 
#' numeric objects, must be specified (e.g. "nCi")
#'@return the converted object, same type as x
#'@family unit functions
#'@examples
#' f <- system.file("extdata", "AD06.tac", package="tacmagic")
#' AD06_tac <- load_tac(f, format="PMOD")
#' AD06_tac_nCicc <- change_units(AD06_tac, to_unit = "nCi/cc")
#'
#' change_units(5, to_unit = "kBq", from_unit = "nCi")
#' change_units(0.185, to_unit = "nCi", from_unit = "kBq")
change_units <- function(x, to_unit, from_unit) UseMethod("change_units")

#'@noRd
#'@export
change_units.tac <- function(x, to_unit, from_unit = NULL) {

  if(!validate_tac(x)) stop("Supplied tac file did not validate.")
  if(!is.null(from_unit)) stop("The from_unit is found within the tac object.")

  unit_index <- get_activity_unit_index()

  from <- sub("([A-Za-z]+).*", "\\1", attributes(x)$activity_unit)
  to <- sub("([A-Za-z]+).*", "\\1", to_unit)

  x[,-(1:2)] <- x[,-(1:2)] * ( unit_index[[from]] / unit_index[[to]] )

  attributes(x)$activity_unit <- to_unit

  if(!validate_tac(x)) stop("Converted tac file did not validate.") 

  return(x)
  
}

#'@noRd
#'@export
change_units.numeric <- function(x, to_unit, from_unit) {

  unit_index <- get_activity_unit_index()

  from <- sub("([A-Za-z]+).*", "\\1", from_unit)
  to <- sub("([A-Za-z]+).*", "\\1", to_unit)

  x <- x * ( unit_index[[from]] / unit_index[[to]] )

  return(x)

}


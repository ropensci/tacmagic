##################################
## tacmagic - PET Analysis in R ##
## utilities.R                  ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

# create an empty table for model calculation storage
#'@noRd
new_table <- function(tac, varname="VALUE") {
  VALUE <- rep(NA, (length(names(tac)) - 2))
  VALUEtable <- data.frame(row.names=names(tac)[-(1:2)], VALUE)	
  names(VALUEtable) <- varname
  return(VALUEtable)
}

#' Copies the time and activity unit attributes from origin to destination;
#' and assigns tm_type as "tac"
#'@noRd
copy_tac_attributes <- function(origin, destination) {
  attributes(destination)$time_unit <- attributes(origin)$time_unit
  attributes(destination)$activity_unit <- attributes(origin)$activity_unit
  attributes(destination)$tm_type <- "tac"
  return(attributes(destination))
}
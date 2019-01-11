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
    
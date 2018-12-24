#utilities.R


# create an empty table for model calculation storage
new_table <- function(tac, varname="VALUE") {
  VALUE <- rep(NA, (length(names(tac)) - 2))
  VALUEtable <- data.frame(row.names=names(tac)[-(1:2)], VALUE)	
  names(VALUEtable) <- varname
  return(VALUEtable)
}
    
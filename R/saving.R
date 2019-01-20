##################################
## tacmagic - PET Analysis in R ##
## saving.R                     ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################


#' Save a tac object as a .tac file
#'
#' Saves a tac object, created by load_tac(), tac_roi() or manually, and 
#' saves it as a PMOD-formatted tac file. Using the .tac extension in the 
#' file name is recommended.
#' 
#' 
#'@export
#'@param tac The time-activity curve data, e.g. from load_tac() or tac_roi()
#'@param outfile The output filename
#'@family tac functions 
#'@return Does not return an object, only saves a file
save_tac <- function(tac, outfile) {

	validate_tac(tac)

	if (!((class(outfile) == "character") & (length(outfile) == 1))) {
		stop("outfile must be a character string")
	}

    names(tac)[1] <- paste0("start[", attributes(tac)$time_unit, "]")
	names(tac)[2] <- paste0("end[", attributes(tac)$activity_unit, "]")

	write.table(tac, file = outfile, append = FALSE, quote = TRUE, sep = "\t",
                 eol = "\n", na = "NaN", dec = ".", row.names = FALSE,
                 col.names = TRUE, qmethod = c("escape", "double"),
                 fileEncoding = "")

}

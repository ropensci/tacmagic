##################################
## tacmagic - PET Analysis in R ##
## loading.R                    ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

# A group of functions for loading files for analysis. File loading should be
# done with these functions rather than in the other files to allow for new
# formats to be easily integrated.


# TAC files

#' Loads TAC from file for use by other functions (default is PMOD .tac format)
#'
#'@export
#'@param filename (e.g. participant.TAC)
#'@param format Options include "PMOD", "voistat" (also from PMOD), and "magia"
#'@param acqtimes File name for a .acqtimes file (as in PMOD), required for 
#' format="voistat"
#'@return data.frame with loaded TAC data
load_tac <- function(filename, format="PMOD", acqtimes=NULL) {
  if (format == "PMOD") {
      tac <- load_tac_PMOD(filename)
  } else if (format == "voistat") {
      tac <- load_tac_voistat(filename, acqtimes)
    } else if (format == "magia") {
      tac <- load_tac_magia(filename)
      } else stop("Speficied format for tac not supported.")

  validate_tac(tac)
  return(tac)
}


# ROI volume data

#' Loads ROI volumes from file for use by other functions
#'
#'@export
#'@param filename (e.g. participant.voistat)
#'@param format (default is the TAC .voistat format from PMOD)
#'@return data.frame with loaded TAC data
load_vol <- function(filename, format="voistat") {
  if (format == "voistat") {
      volumes <- volumesFromVoistatTAC(filename)
  } else if (format == "BPndPaste") {
      volumes <- volumesFromBPndPaste(filename)
  } else stop("Specified format for volume data not supported.")
    
  return(volumes)
}


# PET Model Data

#' Loads model data from file for use by other functions.
#'
#'@param voistat_file Filename (e.g. participant_logan.voistat)
#'@param ROI_def The definition of ROIs by combining smaller ROIs from TAC file
#'@param model A string to name the variable being extracted, e.g. "Logan_DVR"
#'@return data.frame with loaded model data in specified combined weighted ROIs
load_voistat <- function(voistat_file, ROI_def, model="VALUE") {
    
    voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T, 
                        stringsAsFactors=F)
    
    VALUE <- rep(NA, length(ROI_def))
    VALUEtable <- data.frame(row.names=names(ROI_def), VALUE)
    
    for (i in 1:length(ROI_def)) {
        m <- match(ROI_def[[i]], voistat$VoiName.Region...string.)
        VALUEtable[names(ROI_def)[i], "VALUE"] <- weighted.mean(
                                                      voistat$Averaged..1.1.[m], 
                                                      voistat$Volume..ccm.[m])
    }
    
    names(VALUEtable) <- model
    return(VALUEtable)
}

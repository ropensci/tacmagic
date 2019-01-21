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
#' This is the main function for loading an individual participant's TAC data.
#' The minimal required information within the supplied files is the start and 
#' stop times and a time unit (either seconds or minutes), as well as the 
#' activity values for 1 or more ROIs, and units for activity. The currently 
#' supported formats (with the corresponding format argument), include:
#' \itemize{
#'   \item "PMOD": PMOD .tac files
#'   \item "voistat": PMOD TAC .voistat files used in combination with PMOD 
#'          .acqtimes file for start/stop times.
#'   \item "magia": magia pipeline .mat tac file
#'   \item "DFF": Turku PET Centre's DFT format
#' }
#' 
#'@export
#'@param filename (e.g. "participant01.tac")
#'@param format A character string, with options listed above (e.g. "PMOD")
#'@param acqtimes Filename for a .acqtimes file (as in PMOD), required for 
#'                format="voistat"
#'@param time_unit NULL if in file (e.g. PMOD .tac), or set to "seconds" or 
#'                 "minutes" if not in file or to override file
#'@param activity_unit NULL if in file (e.g. PMOD .tac), or set to "kBq/cc", 
#'                     "Bq/cc", "nCi/cc"
#'@return data.frame with loaded TAC data
#'@family Loading functions 
#'@examples
#' f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic") 
#' tac <- load_tac(f_raw_tac)
#' 
load_tac <- function(filename, format="PMOD", acqtimes=NULL, time_unit=NULL, 
                     activity_unit=NULL) {
  
  if (format == "PMOD") {
      
      if (!(is.null(time_unit) & is.null(activity_unit))) {
        warning("Your specified units will override any data in the files.")
      }
      
      tac <- load_tac_PMOD(filename)

  } else if (format == "voistat") {  
      
      if (!(is.null(time_unit) & is.null(activity_unit))) {
        warning("Your specified units will override any data in the files.")
      }
      
      tac <- load_tac_voistat(filename, acqtimes)
    
  } else if (format == "magia") {
    
      if ((is.null(time_unit) | is.null(activity_unit))) {
        stop("You must specify both time and activity units.")
      }
    
      tac <- load_tac_magia(filename)
    
  } else if (format == "DFT") {

      tac <- load_tac_DFT(filename)

  } else stop("Specified format for tac not supported.")

  attributes(tac)$tm_type <- "tac"
  if (!is.null(time_unit)) attributes(tac)$time_unit <- time_unit
  if (!is.null(activity_unit)) attributes(tac)$activity_unit <- activity_unit
  if (!(validate_tac(tac))) stop("TAC object created by load_tac was invalid.")
  return(tac)
}


# ROI volume data

#' Loads ROI volumes from file for use by other functions
#'
#' 
#'@export
#'@examples
#' f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' 
#' vol <- load_vol(f_raw_vol)
#'@param filename (e.g. participant.voistat)
#'@param format (default is the TAC .voistat format from PMOD, also accepts 
#'              "DFT and "BPndPaste")
#'@return data.frame with loaded TAC data
#'@family Loading functions
load_vol <- function(filename, format="voistat") {
  if (format == "voistat") {
      volumes <- volumesFromVoistatTAC(filename)
  } else if (format == "BPndPaste") {
      volumes <- volumesFromBPndPaste(filename)
  } else if (format == "DFT") {
      volumes <- load_vol_DFT(filename)
  } else stop("Specified format for volume data not supported.")
    
  return(volumes)
}


# PET Model Data

#' Reads PMOD .voistat files and optionally merges volume-weighted ROIs
#'
#' PMOD can produce .voistat files with the average model values by ROI for 
#' its voxelwise binding potential (BPnd) models, such as Logan, SRTM, etc.
#' This function reads the .voistat file and returns a data.frame with the
#' ROI as rows and the model value as the column. Optionally, the ROIs can be
#' combined into larger ROIs if ROI_def is specified, just as with TAC loading.
#' 
#'@export
#'@param filename (e.g. participant_logan.voistat)
#'@param ROI_def Optional ROI definitions to combine ROIs (e.g. roi_ham_pib())
#'@param model A string to name the variable being extracted, e.g. "Logan_DVR"
#'@return data.frame with loaded model data in specified combined weighted ROIs
#'@family Loading functions
#' @examples
#' f <- system.file("extdata", "AD06_BPnd_BPnd_Logan.voistat", 
#'                  package="tacmagic")
#' vs <- load_voistat(f, ROI_def=roi_ham_pib(), model="Logan")
load_voistat <- function(filename, ROI_def=NULL, model="VALUE") {
    
  voistat <- read.csv(filename, sep="\t", skip=6, header=TRUE, 
                      stringsAsFactors=FALSE)

  # This still works if ROI_def is NULL
  ROIs <- c(voistat$VoiName.Region...string., names(ROI_def))
  values <- c(voistat$Averaged..1.1., rep(NA, length(ROI_def)))

  VALUEtable <- data.frame(row.names=ROIs, VALUE=values)

  if (!is.null(ROI_def)) {

    for (i in seq_along(ROI_def)) {
      m <- match(ROI_def[[i]], voistat$VoiName.Region...string.)
      VALUEtable[names(ROI_def)[i], "VALUE"] <- weighted.mean(
                                                    voistat$Averaged..1.1.[m], 
                                                    voistat$Volume..ccm.[m])
    }  
  }
    
    names(VALUEtable) <- model
    return(VALUEtable)
}

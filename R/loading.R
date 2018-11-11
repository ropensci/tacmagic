##################################
## PET Analysis in R            ##
## loading.R                    ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

# A group of functions for loading files for analysis. File loading should be
# done with these functions rather than in the other files to allow for new
# formats to be easily integrated.


# TAC files


#' Loads TAC from file for use by other functions. Default is PMOD .tac format.
#'
#'@param filename (e.g. participant.TAC)
#'@param format (default, and only option currently, is .tac as from PMOD.
#'@return data.frame with loaded TAC data
#'@examples loadTACfile("/dir/participant1.tac")
loadTACfile <- function(filename, format="PMOD") {
    if (format == "PMOD") {
        tac <- read.csv(filename, sep="")
    } else stop("Speficied format for tac not supported.")
    return(tac)
}


# ROI volume data

#' Loads ROI volumes from file for use by other functions.
#'
#'@param filename (e.g. participant.voistat)
#'@param format (default is the TAC .voistat format from PMOD).
#'@return data.frame with loaded TAC data
#'@examples loadVolumes("/dir/participant1_TAC.voistat")
loadVolumes <- function(filename, format="Voistat", commontime="30") {
  if (format == "Voistat") {
      volumes <- volumesFromVoistatTAC(filename, commontime)
  } else if (format == "BPndPaste") {
      volumes <- volumesFromBPndPaste(filename)
  } else stop("Specified format for volume data not supported.")
    
  return(volumes)
}


# PET Model Data

#' Loads model data from file for use by other functions.
#'
#'@param filename (e.g. participant_logan.voistat)  
#'@param format (default is the TAC .voistat format from PMOD).
#'@return data.frame with loaded model data in specified combined weighted ROIs.
#'@examples loadVolumes("/dir/participant1_TAC.voistat")
voistatScraper <- function(voistat_file, ROI_def=standardROIs(), model="VALUE") {
    
  voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T,
                      stringsAsFactors=F)
  ROIs <- voistat$VoiName.Region...string.
  Volume..ccm. <- voistat$Volume..ccm.
  Averaged..1.1. <- voistat$Averaged..1.1.
    
  rawvolumes <- data.frame(ROIs, Volume..ccm., row.names=1)
  storedvalues <- data.frame(ROIs, Averaged..1.1., row.names=1)
    
  # Calculate the relative volumes for the subROIs and ROIs
  proportiontable <- calcRelativeVolumes(rawvolumes, ROI_def)
    
  summarytable <- data.frame(
  row.names = c(names(ROI_def@hemilobe), names(ROI_def@lobe), "totalcortical"),
  VALUE = rep(0, length(c(names(ROI_def@hemilobe), names(ROI_def@lobe)))+1)
  )
    
  # Creates a mean table (data.frame to store the means and relative
  # volumes of each ROI), and fills it.
  means <- fill_table(mean_table(ROI_def), storedvalues, ROI_def,
  proportiontable, headername="Averaged..1.1.")
    
  VALUEtable <- create_final_table(ROI_def, model)
    
  VALUEtable <- weighted_average(ROI_def@hemilobe, names(ROI_def@hemilobe),
  			     means, VALUEtable, model, "proportion_of_hemilobe")
  VALUEtable <- weighted_average(ROI_def@lobe, names(ROI_def@lobe), means,
  				                       VALUEtable, model, "proportion_of_lobe")
  VALUEtable <- weighted_average(ROI_def@totalcortical, "totalcortical",
                                means, VALUEtable, model, "proportion_of_total")
    
    return(VALUEtable)
}







##################################
## PET Analysis in R            ##
## loading_utilities.R          ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################


## VOLUME INFORMATION


# TAC .voistat files contain volume information for each ROI. This extracts it. 
volumesFromVoistatTAC <- function(voistat_file) {
    voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T,
    stringsAsFactors=F)
    # create a list of each unique ROI name
    ROIs <- unique(voistat[,"VoiName.Region...string."])
    # subset the voistat data to include just a single time frame, since
    # volume is same for each time
    u <- voistat$Time..seconds.==voistat$Time..seconds.[1]
    # create a list of the volumes for each ROI.
    Volume..ccm. <- voistat[, "Volume..ccm."][u]
    return(data.frame(ROIs, Volume..ccm., row.names=1))
}

# BPnd data can be copied from PNEURO and saved as a CSV. It contains ROI volume
# information. This extracts that. Not needed unless volume information is
# otherwise unavailable.
volumesFromBPndPaste <- function(BPnd_file) {
    BPnd <- read.csv(BPnd_file, header=TRUE, row.names=1)
    return(BPnd["Volume..ccm."])
}


## TAC INFORMATION

#' @noRd
load_tac_PMOD <- function(tac_file) {

  tac <- read.csv(tac_file, sep="")

  if (all(c(
            startsWith(names(tac)[1], "start"), 
            startsWith(names(tac)[2], "end")
           ))) {
    names(tac)[1:2] <- c("start", "end")
  } else stop("The first 2 columns should have start and end times.")

  return(tac)
}

#' @noRd
load_tac_voistat <- function(voistat_file, acqtimes) {
  voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T,
  stringsAsFactors=F)

  voistat_type <- validateTACvoistat(voistat)
  
  if (voistat_type == "invalid") stop("Invalid voistat TAC file.")

  ROIs <- unique(voistat$VoiName.Region...string.)

  variables <- c("time", ROIs)
  if (voistat_type == "C") variables <- c(variables, paste(ROIs, "_C", sep=""))

  frames <- length(voistat[voistat$VoiName.Region...string. == ROIs[3], ][,1])

  tac <- as.data.frame(matrix(ncol=length(variables), nrow=frames))
  names(tac) <- variables

  tac$time <- voistat[voistat$VoiName.Region...string. == ROIs[3], ][, "Time..seconds."]

  for (i in 1:length(ROIs)) {
    tac[,ROIs[i]] <- voistat[voistat$VoiName.Region...string. == ROIs[i], ][, "Averaged..kBq.cc."]

    if (voistat_type == "C") {
      tac[,paste(ROIs[i], "_C", sep="")] <- voistat[voistat$VoiName.Region...string. == ROIs[i], ][, "PVC..kBq.cc."]
    }
  }

  startend <- load_acq(acqtimes)
  if (checkACQtimes(startend$start, startend$end, tac$time)) {
	tac <- data.frame(startend, tac) 
  } else stop("Supplied acqtimes do not match midframe time data.")

  tac$time <- NULL

  return(tac)
}

# Loads tac data from a .mat file, the output of the magia pipelines
# magia information is found at references()$magia
#' @noRd
load_tac_magia <- function(filename) {
  matlab <- R.matlab::readMat(filename)
  tacs <- as.data.frame(t(matlab$tacs))
  names(tacs) <- as.vector(unlist(matlab$roi.info[[1]]))
  frames <- as.data.frame(matlab$frames) * 60
  names(frames) <- c("start", "end")
  return(data.frame(frames, tacs))
}

# Checks to ensure there are start and end times in the first 2 columns.
#' @noRd
validate_tac <- function(tac) {
  if (FALSE == (startsWith(names(tac)[1], "start") && 
                startsWith(names(tac)[2], "end"))) {
    stop("The first two columns of the TAC file should be start and end times, 
          with headers starting with 'start' and 'end'.")
  }
  return(TRUE)
}

# Checks the headers of the TAC voistat file to ensure they are as expected,
# and returns C if there is a PVC value, NC if not, and invalid if headers
# are not as expected.
#' @noRd
validateTACvoistat <- function(voistat) {

  PVC <- c("X...Component..string.", "File..string.", "PatientName..string.", 
           "PatientID..string.", "PatientInfo..string.", 
           "StudyDescription..string.", "SeriesDescription..string.", 
           "StudyDate..date_time.", "Color..0xARGB.", 
           "VoiName.Region...string.", "Time..seconds.", "Averaged..kBq.cc.", 
           "PVC..kBq.cc.", "Volume..ccm.")

  if (all(names(voistat) == PVC)) type <- "C" 
  else if (all(names(voistat) == PVC[-13])) type <- "NC" 
  else type <- "invalid"

  return(type)
}

# A .acqtimes file can be saved from PMOD, or created, and contains start and 
# end times of each frame. Returns a data frame with 2 columns with start, end
# in seconds.
#' @noRd
load_acq <- function(acqtimes_file) {
  aq <- read.csv(acqtimes_file, sep="\t", skip=2, header=F)
  names(aq) <- c("start", "end")
  return(aq)
}

# Ensures consistency between start/end and mid-frame times.
#' @noRd
checkACQtimes <- function(start, end, mid) {
  return(all(mid == ((start + end) / 2)))
}

# To convert the voistat TAC file to a .tac file, check the header names to
# ensure it matches.
#' @noRd
voistat_to_TAC <- function(voistat_file, acqtimes_file, output_file,
                           header_names=c("start[seconds]", "end[kBq/cc]")) {
  tac <- load_tac_voistat(voistat_file, acqtimes_file)
  tac <- tac[-3]
  names(tac)[1:2] <- header_names
  write.table(x=tac, file=output_file, quote=F, sep="\t", row.names=F)
}

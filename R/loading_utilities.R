##################################
## tacmagic - PET Analysis in R ##
## loading_utilities.R          ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################



# General tac file validation
#' @noRd
validate_tac <- function(tac) {
  # are first 2 variables "start" and "end"

  status <- TRUE

  if (FALSE == (startsWith(names(tac)[1], "start") && 
                startsWith(names(tac)[2], "end"))) {
    message("The first two columns of the TAC file should be start and end 
             times, with headers starting with 'start' and 'end'.")
    status <- FALSE
  }

  # Are the correct attributes set
  if (!(attributes(tac)$tm_type == "tac")) {
    message("TAC data should have attribute tm_type=tac")
    status <- FALSE
  }

  if (!(attributes(tac)$time_unit %in% c("seconds", "minutes"))) {
    message("TAC data missing attribute time_unit")
    status <- FALSE
  }

  if (!(attributes(tac)$activity_unit %in% c("kBq/cc", "nCi/cc", "Bq/cc"))) {
    message("TAC data missing attribute activity_unit")
    status <- FALSE
  }

  return(status)
}

#### PMOD file types ----------------------------------------------------------

  ## TAC INFORMATION

# PMOD .tac file
#' @noRd
load_tac_PMOD <- function(tac_file) {

  tac <- read.csv(tac_file, sep="")

  if (names(tac)[1] == "start.seconds.") {
    attributes(tac)$time_unit <- "seconds"
  } else if (names(tac)[1] == "start.minutes.") { 
      attributes(tac)$time_unit <- "minutes"  
      } else {
          stop("First column should be start[seconds] or start[minutes]")
        }
  names(tac)[1] <- "start"

  if (names(tac)[2] %in% c("end.kBq.cc.", "end.KBq.ml.")) {
    attributes(tac)$activity_unit <- "kBq/cc"
  } else {
     stop("Second column should be end[kBq/cc] or end[kBq/ml]")
    }
  names(tac)[2] <- "end"

  return(tac)
}

# PMOD .voistat TAC file, needs .acqtimes file as well
#' @noRd
load_tac_voistat <- function(voistat_file, acqtimes) {
  voistat <- read.csv(voistat_file, sep="\t", skip=6, header=TRUE,
                      stringsAsFactors=FALSE)

  voistat_type <- validateTACvoistat(voistat)
  
  if (voistat_type == "invalid") stop("Invalid voistat TAC file.")

  ROIs <- unique(voistat$VoiName.Region...string.)

  variables <- c("time", ROIs)
  if (voistat_type == "C") variables <- c(variables, paste(ROIs, "_C", sep=""))

  frames <- length(voistat[voistat$VoiName.Region...string. == ROIs[3], ][,1])

  tac <- as.data.frame(matrix(ncol=length(variables), nrow=frames))
  names(tac) <- variables

  tac$time <- voistat[voistat$VoiName.Region...string. == ROIs[3], ][, 
                                                              "Time..seconds."]

  for (i in seq_along(ROIs)) {
    tac[,ROIs[i]] <- voistat[voistat$VoiName.Region...string. == 
                             ROIs[i], ][, "Averaged..kBq.cc."]

    if (voistat_type == "C") {
      tac[,paste0(ROIs[i], "_C")] <- voistat[voistat$VoiName.Region...string. 
                                             == ROIs[i], ][, "PVC..kBq.cc."]
    }
  }

  startend <- load_acq(acqtimes)
  if (checkACQtimes(startend$start, startend$end, tac$time)) {
  tac <- data.frame(startend, tac) 
  } else stop("Supplied acqtimes do not match midframe time data.")

  tac$time <- NULL
  attributes(tac)$time_unit <- "seconds"
  attributes(tac)$activity_unit <- "kBq/cc"

  return(tac)
}


# A .acqtimes file can be saved from PMOD, or created, and contains start and 
# end times of each frame. Returns a data frame with 2 columns with start, end
# in seconds.
#' @noRd
load_acq <- function(acqtimes_file) {
  aq <- read.csv(acqtimes_file, sep="\t", skip=2, header=FALSE)
  names(aq) <- c("start", "end")
  return(aq)
}


  ## VOLUME INFORMATION

# TAC .voistat files contain volume information for each ROI. This extracts it
#' @noRd
volumesFromVoistatTAC <- function(voistat_file) {
    voistat <- read.csv(voistat_file, sep="\t", skip=6, header=TRUE,
                        stringsAsFactors=FALSE)
    # create a list of each unique ROI name
    ROIs <- unique(voistat[,"VoiName.Region...string."])
    # subset the voistat data to include just a single time frame, since
    # volume is same for each time
    u <- voistat$Time..seconds.==voistat$Time..seconds.[1]
    # create a list of the volumes for each ROI.
    Volume..ccm. <- voistat[, "Volume..ccm."][u]
    return(data.frame(ROIs, Volume..ccm., row.names=1))
}

# BPnd data can be copied from PNEURO and saved as a CSV. It contains ROI
# volume information. This extracts that. Not needed unless volume information
# is otherwise unavailable.
volumesFromBPndPaste <- function(BPnd_file) {
    BPnd <- read.csv(BPnd_file, header=TRUE, row.names=1)
    return(BPnd["Volume..ccm."])
}



  ## PMOD validation

# Ensures consistency between start/end and mid-frame times.
#' @noRd
checkACQtimes <- function(start, end, mid) {
  return(all(mid == ((start + end) / 2)))
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



#### Magia file types ---------------------------------------------------------

# Loads tac data from a .mat file, the output of the magia pipelines
# magia information is found at references()$magia
#' @noRd
load_tac_magia <- function(filename) {
  matlab <- R.matlab::readMat(filename)
  tacs <- as.data.frame(t(matlab$tacs))
  names(tacs) <- as.vector(unlist(matlab$roi.info[[1]]))
  frames <- as.data.frame(matlab$frames) * 60
  names(frames) <- c("start", "end")
  tac <- data.frame(frames, tacs)
  return(tac)
}




#### Turku PET Centre DFT file type

  # File type specifications: 
  # http://www.turkupetcentre.net/formats/format_dft_1_0_0.pdf

#'@noRd
load_tac_DFT <- function(f) {

  header <- load_header_DFT(f) 
  ROIs <- load_ROIs_DFT(header)

  if (header[3,1] == "kBq/ml") {
    activity_unit <- "kBq/cc"
  } else {
    stop(paste("Was expecting activity units kBq/ml but got", header[3,1]))
  }

  if (header[4,2] %in% c("(min)")) {
    time_unit <- "minutes"
  } else if (header[4,2] %in% c("(sec)", "(s)")) {
    time_unit <- "seconds"
  } else {
    stop("Invalid time units detected.")
  }

  tac <- read.table(f, skip=4, stringsAsFactors=FALSE)
  # "." is the missing data convention in "DFT"; here we replace . with NA
  # so that when we coerce to numeric, the warnings are meaningful.
  repl <- function(v) replace(v, which(v=="."), NA) 
  tac <- apply(tac, 2, repl)
  tac <- as.data.frame(apply(tac, 2, as.numeric))

  if (length(tac) - 2 != length(ROIs)) stop("ROIs and columns don't match")

  colnames(tac) <- c("start", "end", ROIs)
  
  attributes(tac)$time_unit <- time_unit
  attributes(tac)$activity_unit <- activity_unit
  
  return(tac)
} 

#'@noRd  
load_header_DFT <- function(f) {
  
  header <- read.delim(f, nrows=4, header=FALSE, sep="", stringsAsFactors=FALSE)

  if (header[1,1] != "DFT") stop("Bad DFT file: no \"DFT\" string")

  # This is expected format when there are start and stop times
  if (header[4,1] != "Times") stop("Bad DFT file: expected \"Times\" at 4,1")

  time_options <- c("(min)", "(sec)", "(s)")
  if (!(header[4,2] %in% time_options)) stop("Bad DFT file: bad time units")
  
  return(header)  
}

#'@noRd
load_vol_DFT <- function(f) {

  header <- load_header_DFT(f)
  ROIs <- load_ROIs_DFT(header)
  weights <-  as.numeric(header[4, seq_along(header)[-(1:2)]])   

  return(data.frame(row.names=ROIs, volume=weights))
}

#'@noRd
load_ROIs_DFT <- function(header) {

  vars <- as.character(header[1, head(seq_along(header)[-1], -1)])
  secvars <- as.character(header[2, head(seq_along(header)[-1], -1)])

  return(paste(vars, secvars, sep="_"))

}

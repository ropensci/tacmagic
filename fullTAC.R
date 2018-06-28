##################################
## PET Analysis in R            ##
## fullTAC.R                    ##
## Eric E. Brown                ##
## v 0.1.7--in progress         ##
##################################

source("ROI_definitions.R")
source("calculateSUVR.R")
source("utilities.R")


# This creates a data.frame to hold the mean TAC from the ROIs specified in 
# ROI_def. 
emptyTACtable <- function(tac_file, sep="\t", ROI_def=standardROIs(), do_total_cortical=TRUE) {
  tac <- read.csv(tac_file, sep=sep)

  #Warning: ensure the tac file has first 2 columns = start and end
  TACtable <- tac[1:2]
  names(TACtable) <- c("start", "end")
  frames <- length(TACtable$start)

  ROIs <- c(ROI_def@hemilobenames, ROI_def@lobenames)
  for (ROI in ROIs) {
    TACtable <- data.frame(TACtable, rep(0, frames))
    names(TACtable)[ncol(TACtable)] <- ROI
    }

  if (do_total_cortical) {
    totalcortical <- rep(0, frames)
    TACtable <- data.frame(TACtable, totalcortical)
    }
  
  return(TACtable)
  }

# This creates TACs for ROIs as specified in ROI_def, i.e. takes the weighted
# average of TACs of each region that makes up the ROI.
weighted_TAC <- function(ROI_def_val, ROI_def_names, tac, TACtable,
                             proportion_of_text, vols) {

  frames <- length(TACtable$start)
    
  counter <- 1
  temp <- 0

  for (ROI in ROI_def_val) {
    for (subROI in ROI) {
      temp <- temp + (tac[subROI] * vols[subROI, proportion_of_text])
    }
  
    names(temp) <- ROI_def_names[counter]
    TACtable[, ROI_def_names[counter]] <- temp
    temp <- rep(0, frames)
    counter <- counter + 1
  }

  return(TACtable)
}

#This uses weighted_TAC 3 times for hemilobes, lobes and totalcortical ROIs.
calcTAC <- function(tac_file, voistat_file, ROI_def=standardROIs()) {
  tac <- read.csv(tac_file, sep="\t")
  vols <- calcRelativeVolumes(volumesFromVoistatTAC(voistat_file), ROI_def)
  TACtable <- emptyTACtable(tac_file)


  TACtable <- weighted_TAC(ROI_def@hemilobe, ROI_def@hemilobenames, tac, TACtable, "proportion_of_hemilobe", vols)
  TACtable <- weighted_TAC(ROI_def@lobe, ROI_def@lobenames, tac, TACtable, "proportion_of_lobe", vols)
  TACtable <- weighted_TAC(ROI_def@totalcortical, "totalcortical", tac, TACtable, "proportion_of_total", vols)

  return(TACtable)
}

#A function to get average TAC for a list of participants in weighted average ROIs.
groupTAC <- function(participantlist, directory="", ROI_def=standardROIs()) {
  groupTACtable <- emptyTACtable(paste(directory, participantlist[1], ".tac", sep=""), sep="", ROI_def)
  print("Working on files:")
  for (participant in participantlist) {
    tac_file <- paste(directory, participant, ".tac", sep="")
    voistat_file <- paste(directory, participant, ".voistat", sep="")
    print(tac_file)
    print(voistat_file)
    TACtable <- calcTAC(tac_file, voistat_file, ROI_def)
    groupTACtable <- groupTACtable + TACtable
  }
  groupTACtable <- groupTACtable / length(participantlist)
  print(paste("Divided group table by", length(participantlist), "to arrive at mean."))
  return(groupTACtable)
}

# Simple plot of a single ROI TAC.
plotTAC <- function(TACtable, ROIs=c("totalcortical", "cerebellum")) {
  plot(1,type='n',xlim=c(1,35),ylim=c(0,25),xlab='Time', ylab='Activity')
  colour <- rainbow(length(ROIs))
  index <- c(1:length(ROIs))
  for (ROI in index) {
    lines(TACtable[,ROIs[ROI]], type='o', col=colour[ROI], lwd=2)
  }
  legend("topright", legend = ROIs , col=colour, pch=1)
}

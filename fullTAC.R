##################################
## PET Analysis in R            ##
## fullTAC.R                    ##
## Eric E. Brown                ##
## PEAR v 0.1.8                 ##
## Beta version--check all work ##
##################################

source("utilities.R")


# This function calculates the weighted time-activity curves for ROIs by 
# combining smaller ROIs into larger ones as specified in ROI_def, by getting
# the weighted average.
# This uses weighted_TAC (from utilities.R) 3 times for hemilobes, lobes and 
# totalcortical ROIs. The main work is done by weighted_TAC, and this simply 
# runs it 3 times. The user would only have to use this function.
calcTAC <- function(
    tac_file,   #filename of the tac file ("subject.tac")
    voistat_file, #filename of the voistat file ("subject.voistat") 
    ROI_def=standardROIs(), #see ROI_definitions.R
    merge=F # combined table of the merged ROIs and the old individual ROIs
    ) {
  tac <- read.csv(tac_file, sep="")
  vols <- calcRelativeVolumes(volumesFromVoistatTAC(voistat_file), ROI_def)
  TACtable <- emptyTACtable(tac_file)

  TACtable <- weighted_TAC(ROI_def@hemilobe, names(ROI_def@hemilobe), tac,
                          TACtable, "proportion_of_hemilobe", vols)
  TACtable <- weighted_TAC(ROI_def@lobe, names(ROI_def@lobe), tac,
                          TACtable, "proportion_of_lobe", vols)
  TACtable <- weighted_TAC(ROI_def@totalcortical, "totalcortical", tac, 
                          TACtable, "proportion_of_total", vols)

  if (merge) {
    TACtable <- data.frame(TACtable, tac)
  }

  return(TACtable)
}


# A function to get average TAC for a list of participants in weighted average 
# ROIs.
groupTAC <- function(participantlist, directory="", ROI_def=standardROIs(), 
                     merge=F) {
  groupTACtable <- emptyTACtable(paste(directory, participantlist[1], ".tac", 
                                 sep=""), sep="", ROI_def, merge=merge)
  print("Working on files:")
  for (participant in participantlist) {
    tac_file <- paste(directory, participant, ".tac", sep="")
    voistat_file <- paste(directory, participant, ".voistat", sep="")
    print(tac_file)
    print(voistat_file)
    TACtable <- calcTAC(tac_file, voistat_file, ROI_def, merge)
    print(all(names(TACtable)==names(groupTACtable)))
    if (all(names(TACtable)==names(groupTACtable))==F) {
        stop("Columns don't match. Please check your files.")
    }
    groupTACtable <- groupTACtable + TACtable
  }
  groupTACtable <- groupTACtable / length(participantlist)
  print(paste("Divided group table by", length(participantlist), 
        "to arrive at mean."))
  return(groupTACtable)
}

# Plots time activity curves from 1 or 2 participants or group means.
# Note this works with the output of groupTAC and calcTAC, as well as simply 
# .tac files. Further, you can add together the original TAC file and the 
# weighted TACs as they are simple data frames.
# e.g. merged <- data.frame(individualtac, weightedTACtable)
plotTAC2 <- function(TACtable1, TACtable2=NULL, ROIs=c("totalcortical", 
  "cerebellum"), ymax=25, seconds_to_mins=FALSE) {
  
  # If the seconds_to_mins argument is TRUE, this converts the time from 
  # seconds to minutes (by dividing the $start column by 60)
  if (seconds_to_mins) {
    time_conversion <- 60
    time_units <- "Time (minutes)"
  } else { 
      time_conversion <- 1
      time_units <- "Time (seconds)"
    }

  # Sets up the plot using the frame start from the TAC file for the x axis
  # and converting to minutes if chosen. 
  plot(1,type='n',xlim=c(TACtable1$start[1],
                      TACtable1$start[length(TACtable1$start)]/time_conversion),
                        ylim=c(0,ymax),xlab=time_units, ylab='Activity')
  
  # Separate colour ranges for each group of TACs.
  colour1 <- rainbow(length(ROIs), start=0, end=0.25)
  colour2 <- rainbow(length(ROIs), start=0.5, end=0.8)
  
  # Plots the ROIs as specified in the ROIs argument.
  index <- c(1:length(ROIs))
  for (ROI in index) {

    lines(x=TACtable1$start/time_conversion, 
          y=TACtable1[,ROIs[ROI]], 
          type='o', 
          col=colour1[ROI], 
          lwd=2)
    
    # Only if a 2nd TAC table is provided, plots a second participant/group on 
    # the same plot.
    if (is.data.frame(TACtable2)) {
      lines(x=TACtable2$start/time_conversion, 
            y=TACtable2[,ROIs[ROI]], 
            type='o', 
            col=colour2[ROI], 
            lwd=2)
    }
    
  }
  
  legend("topright", legend = ROIs , col=colour1, pch=1)
  if (is.data.frame(TACtable2)) {
    legend("bottomright", legend = ROIs, col=colour2, pch=1)
  }
} 

##################################
## PET Analysis in R            ##
## fullTAC.R                    ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

#' Calculate weighted time-activity curves for specified regions of interest
#'
#'@param tac The time-activity curve data from loading function.
#'@param volumes The ROI volume data from loading function
#'@param ROI_def The definition of ROIs by combining smaller ROIs from TAC file.
#'@param merge If true, includes the original ROIs in the output data.
#'@return Time-activity curves for the specified ROIs
#'@examples calcTAC(p1tac, p1vol, standardROIs(), merge=T)
calcTAC <- function(tac, volumes, ROI_def, merge=F, PVC=F) {
    
    ROI_PVC <- ROI_def
    
    if (PVC) {
        for (i in 1:length(ROI_PVC)) ROI_PVC[i] <- lapply(ROI_PVC[i], paste, "_C", sep="")
    }
    
    # Setup the output data.frame
    m <- matrix(nrow=length(tac[,1]), ncol=length(ROI_def))
    calculated_TACs <- as.data.frame(m)
    names(calculated_TACs) <- names(ROI_def)
    # Calculate the weighted mean TACs for each ROI in the definition list.
    for (i in 1:length(ROI_def)) {
        calculated_TACs[i] <- apply(tac[,ROI_PVC[[i]]], 1,  weighted.mean, volumes[ROI_def[[i]],])
    }
    
    # Prepare the output data frame.
    if (merge) {
      calculated_TACs <- data.frame(tac, calculated_TACs)
    } else {
        calculated_TACs <- data.frame(tac[1:2], calculated_TACs)
    }
    
    return(calculated_TACs)
}

#' Calculate group mean TAC for a list of participants in weighted average ROIs.
#'
#'@param participantlist
#'@param tac The time-activity curve data from loading function.
#'@param raw_volumes The ROI volume data from loading function
#'@param ROI_def The definition of ROIs by combining smaller ROIs from TAC file.
#'@param merge If true, includes the original ROIs in the output data.
#'@return Time-activity curves for the specified ROIs
#'@examples calcTAC(p1tac, p1vol, standardROIs(), merge=T)
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

#' Plots time activity curves from 1 or 2 participants or groups.
#'
#'@param TACtable1 (e.g. from calcTAC() or groupTAC(), or simply loadTACfile())
#'@param TACtable2 An optional, second TAC, to plot for comparison.
#'@param ROIs A vector of ROIs to plot, names matching the TAC headers.
#'@param ymax The maximum value on the y-axis.
#'@param seconds_to_mins If true, converts time from TAC from sec to min.
#'@return Creates a plot.
#'@examples plotTAC2(controls_tac, treatment_tac)
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

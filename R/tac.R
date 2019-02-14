##################################
## tacmagic - PET Analysis in R ##
## tac.R                        ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' Calculate weighted time-activity curves for specified regions of interest
#'
#'@export
#'@param tac The time-activity curve data from loading function
#'@param volumes The ROI volume data from loading function
#'@param ROI_def The definition of ROIs by combining smaller ROIs from TAC file
#'@param merge If TRUE, includes the original ROIs in the output data
#'@param PVC If TRUE, appends "_C" to ROI name header (as in PMOD TAC files)
#'@family tac functions 
#'@return Time-activity curves for the specified ROIs
#'@examples 
#' # f_raw_tac and f_raw_vol are the filenames of PMOD-generated files
#' f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic") 
#' f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' 
#' tac <- load_tac(f_raw_tac)
#' vol <- load_vol(f_raw_vol)
#' AD06_tac_nc <- tac_roi(tac, vol, roi_ham_full(), merge=FALSE, PVC=FALSE)
tac_roi <- function(tac, volumes, ROI_def, merge, PVC) {
    
  if(!validate_tac(tac)) stop("Supplied tac file did not validate.")

  ROI_PVC <- ROI_def
    
  if (PVC) {
      for (i in seq_along(ROI_PVC)) {
        ROI_PVC[i] <- lapply(ROI_PVC[i], paste, "_C", sep="")
      }
  }
    
  # Setup the output data.frame
  m <- matrix(nrow=length(tac[,1]), ncol=length(ROI_def))
  calculated_TACs <- as.data.frame(m)
  names(calculated_TACs) <- names(ROI_def)

  # Calculate the weighted mean TACs for each ROI in the definition list
  for (i in seq_along(ROI_def)) {
      calculated_TACs[i] <- apply(tac[,ROI_PVC[[i]]], 1, weighted.mean,
                                  volumes[ROI_def[[i]],])
  }
    
  if (merge) {
    calculated_TACs <- data.frame(tac, calculated_TACs)
  } else {
      calculated_TACs <- data.frame(tac[1:2], calculated_TACs)
  }
  
  attributes(calculated_TACs) <- copy_tac_attributes(tac, calculated_TACs)

  if(!validate_tac(calculated_TACs)) stop("Merged ROI tac file did not 
                                           validate.")

  return(calculated_TACs)
}

#' Plots time activity curves from 1 or 2 participants or groups.
#'
#'@export
#'@param TACtable1 (e.g. from tac_roi() or load_tac())
#'@param TACtable2 An optional, second TAC, to plot for comparison
#'@param ROIs A vector of ROIs to plot, names matching the TAC headers
#'@param ymax The maximum value on the y-axis
#'@param time "seconds" or "minutes" depending on desired x-axis, converts tac
#'@param title A title for the plot
#'@return Creates a plot
#'@family tac functions 
#'@examples
#' # f_raw_tac and f_raw_vol are the filenames of PMOD-generated files
#' f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic") 
#' f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' 
#' tac <- load_tac(f_raw_tac)
#' vol <- load_vol(f_raw_vol)
#' AD06_tac_nc <- tac_roi(tac, vol, roi_ham_full(), merge=FALSE, PVC=FALSE)
#' plot_tac(AD06_tac_nc, ROIs=c("frontal", "cerebellum"), title="Example Plot")
plot_tac <- function(TACtable1, TACtable2=NULL, ROIs, ymax=25, 
                     time="minutes", title="") {
  
  if (!all(ROIs %in% names(TACtable1))) stop("ROIs are not in TACtable1")

  if (!validate_tac(TACtable1)) stop("The 1st tac object did not validate.")
  if (!is.null(TACtable2)) {
    if (!validate_tac(TACtable2)) stop("The 2nd tac object did not validate.")
  }
  
  if (!(time %in% c("seconds", "minutes"))) stop("Time must be \"seconds\" or 
                                                 \"minutes\"")

  if (time == "minutes") {
    if (attributes(TACtable1)$time_unit == "minutes") time_conversion <- 1
    if (attributes(TACtable1)$time_unit == "seconds") time_conversion <- 60
  }

  if (time == "seconds") {
    if (attributes(TACtable1)$time_unit == "seconds") time_conversion <- 1
    if (attributes(TACtable1)$time_unit == "minutes") time_conversion <- 1/60
  }

  # Sets up the plot using the frame start from the TAC file for the x axis
  # and converting to minutes if chosen. 
   
  plot(1,type='n',
       xlim=c(TACtable1$start[1],
              TACtable1$start[length(TACtable1$start)]/time_conversion),
              ylim=c(0,ymax),
              xlab=paste0("Time (", time, ")"),
              ylab=paste0("Activity (", 
                          attributes(TACtable1)$activity_unit, ")"),
              main=title)
  
  # Separate colour ranges for each group of TACs
  colour1 <- rainbow(length(ROIs), start=0, end=0.25)
  colour2 <- rainbow(length(ROIs), start=0.5, end=0.8)
  
  # Plots the ROIs as specified in the ROIs argument
  for (ROI in seq_along(ROIs)) {

    lines(x=TACtable1$start/time_conversion, 
          y=TACtable1[,ROIs[ROI]], 
          type='o', 
          col=colour1[ROI], 
          lwd=2)
    
    # Only if 2nd TAC table is provided, plots second on same plot
    if (is.data.frame(TACtable2)) {
      if (!all(ROIs %in% names(TACtable2))) stop("ROIs are not in TACtable2")
      compare_tac_form(TACtable1, TACtable2)
      lines(x=TACtable2$start/time_conversion, 
            y=TACtable2[,ROIs[ROI]], 
            type='o', 
            col=colour2[ROI], 
            lwd=2)
    }
  }
  
  legend("topright", legend=ROIs, col=colour1, pch=1)
  if (is.data.frame(TACtable2)) {
    legend("bottomright", legend=ROIs, col=colour2, pch=1)
  }
} 

# Utility functions -----------------------------------------------------------

# Used by the plot, or any function that needs 2 tacs, to ensure their overall
# form and attributes are equal (except the ROIs)
compare_tac_form <- function(tac, tac2) {
  if (!all.equal(tac$start, tac2$start)) stop("tac start times not equal")
  if (!all.equal(tac$end, tac2$end)) stop("tac end times not equal")
  a1 <- attributes(tac)
  a2 <- attributes(tac2)
  if (!all.equal(a1$time_unit, a2$time_unit)) stop("tac time units not equal")
  if (!all.equal(a1$activity_unit, a2$activity_unit)) stop("tac start times 
                                                            not equal")
  return(TRUE) 
}
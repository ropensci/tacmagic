##################################
## PET Analysis in R            ##
## calculateSUVR.R              ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

source("utilities.R")
source("loading.R")

# calcSUVR
# Use relative volumes to calculate weighted SUVRs
# tac is the tac data from loading function
# volumes is the ROI volume data from loading function
# SUVR_def is a vector of the start times for the TACs to be used in SUVR,
# for example: c("3000", "3300", "3600", "3900")
# corrected is TRUE for partial volume correction, where it is stored as _C in
# the same tac file provided.
calcSUVR <- function(TAC_file, volumes, ROI_def, SUVR_def, corrected=TRUE) {
  
  tac <- data.frame(TAC_file, row.names=1)
  proportiontable <- calcRelativeVolumes(volumes, ROI_def)
  denominator <- length(SUVR_def)

  # Creates a data.frame to store the means (over the SUVR window) and relative 
  # volumes of each ROI
  means <- mean_table(ROI_def)

  # This fills in the mean table. Note the _C is added to get the PVC-corrected 
  # values by default, unless corrected=FALSE
  for (subROI in ROI_def@all) {
    single_mean <- sum(tac[SUVR_def, correct(corrected, subROI)])/denominator
    means <- fill_means_table(single_mean, subROI, means, proportiontable)
  }

  # Data frame to store the calculated SUVRs, which will be returned.
  SUVRtable <- create_final_table(ROI_def, "SUVR")

  # This step calculates the SUVR for each hemilobe by iterating through each 
  # ROI name (from hemilobe names) and ROI in ROI_def@hemilobe. This speaks to 
  # the critical importance of both sources having the same order, so be 
  # cautious if changing the standardROIs() function.

  SUVRtable <- weighted_average(ROI_def@hemilobe, names(ROI_def@hemilobe),
    means, SUVRtable, "SUVR", "proportion_of_hemilobe")
  SUVRtable <- weighted_average(ROI_def@lobe, names(ROI_def@lobe), means,
    SUVRtable, "SUVR", "proportion_of_lobe")
  SUVRtable <- weighted_average(ROI_def@totalcortical, "totalcortical", 
    means, SUVRtable, "SUVR", "proportion_of_total")
  # Gets the cerebellum value to use as reference and calculate SUVR with
  cerebellumreference <- (means["Cerebellum_l", "mean"] * 
                          means["Cerebellum_l", "proportion_of_lobe"]) + 
                         (means["Cerebellum_r", "mean"] * 
                          means["Cerebellum_r", "proportion_of_lobe"])
  SUVRtable <- SUVRtable/cerebellumreference

  return(SUVRtable)
}

# A function to find the upslope of the TAC, from start to peak.
peakSlope <- function(TAC_file) {

  TAC <- read.csv(TAC_file, sep="\t")
  # create a new data frame to hold the calculated slopes 
  holder <- TAC[1,]
  # remove any data  
  holder[1,] <- NA
  # get rid of the first 2 columns, i.e. the frame times
  holder <- holder[,3:length(holder)]

  # make a list of all the ROIs
  subROIs <- names(holder)

  # loop through each of the subROIs from the TAC file.
  for (each in subROIs) {

    #find the highest value in the TAC
    TAC_maximum <- max(TAC[,each])
    
    #proceed only if the highest value was found
    if (!is.na(TAC_maximum)) {
  
      # get TRUE for each of the TAC values that match the max value 
      # (There should be just one, and only the first is needed.)
      TAC_maximum_bool <- TAC[,each] == TAC_maximum
      # returns the value for the end of the time frame of that maximum value
      frame_of_maximum <- TAC[TAC_maximum_bool,]$end.kBq.cc.

      #slope = rise over run = TAC_maximum - TAC_first / frame_of_maximum
      slope <- (TAC_maximum - TAC[1, each]) / frame_of_maximum[1]

      # fill in the calculated slope into the holder data.frame
      holder[,each] <- slope 
    }
  }

  return(holder)  
}


# Calculates weighted average slopes for ROIs, using slopes from peakSlope
peakSlopeROI <- function(slopes, ROI_def, proportiontable, corrected=TRUE) {

  # data.frame to store the slopes and relative volumes of each ROI
  means <- mean_table(ROI_def)

  # Fill the means table -- note this is where the corrected or 
  # uncorrected values come from
  for (subROI in ROI_def@all) {
    single_mean <- slopes[, correct(corrected, subROI)]    
    means <- fill_means_table(single_mean, subROI, means, 
    proportiontable)
  }

  # Data frame for final weighted slopes, which will be returned.
  slope_table <- create_final_table(ROI_def, "slope")

  slope_table <- weighted_average(ROI_def@hemilobe, names(ROI_def@hemilobe),
    means, slope_table, "slope", "proportion_of_hemilobe")
  slope_table <- weighted_average(ROI_def@lobe, names(ROI_def@lobe), means,
    slope_table, "slope", "proportion_of_lobe")
  slope_table <- weighted_average(ROI_def@totalcortical, "totalcortical", 
    means, slope_table, "slope", "proportion_of_total")

  return(slope_table)
}
  

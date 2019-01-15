##################################
## tacmagic - PET Analysis in R ##
## cutoff.R                     ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################


#' Cutoff value caluclation using method described in Aizenstein et al. 2008
#' 
#' See references()$Aizenstein. The authors proposed a standardized method of 
#' calculating PIB+ cutoff values to classify participants as PIB+ or PIB-. They
#' used the DVR from several ROIs associated with amyloid deposition. The steps 
#' are summarized below. cutoff_aiz() implements 1-3, returning cutoff values
#' for each ROI. It can be used to dichotomize participants, with pos_anyroi().
#' 
#' 1. Remove outliers from a group of cognitively normal individuals. An outlier
#' is defined as having any ROI with DVR > upper inner fence of that ROI (= 3rd 
#' quartile + (1.5 * IQR).
#' 2. Iterate step 1 as needed until there are no more outlying participants.
#' 3. From this subset of the group with outliers removed, the cutoff value for 
#' each ROI is set as the upper inner fence. 
#' 4. For all participants, if there is any ROI above the cutoff for that 
#' region, then the participant is deemed to be PIB+.
#' 
#' @export
#' @param modelstats SUVR or DVR data for group of participants from batch_tm()
#' @param ROIs list of variables (ROIs) to use for cutoff detection
#' @return Cutoff values for each ROI based on the above method
cutoff_aiz <- function(modelstats, ROIs) {

  if (length(ROIs) < 2) stop("You must specify at least 2 ROIs.")

  modelstats <- modelstats[,ROIs]
  outliers <- modelstats
  outliers[,] <- FALSE

  i <- 1 
  num_removed <- 9999

  while (num_removed > 0) {

    cut <- apply(modelstats, 2, upper_inner_fence)
    threshold <- outliers
    for (j in 1:length(cut)) threshold[,j] <- cut[j]
    outliers <- modelstats > threshold

    num_removed <- sum(apply(outliers, 1, any))
    message(paste("Iteration:", i, "Removed:", num_removed))

    modelstats <- modelstats[!apply(outliers, 1, any),]
    outliers <- outliers[!apply(outliers, 1, any),]

    i <- i + 1
  }

  cut <- apply(modelstats, 2, upper_inner_fence)
  return(cut)
}

#' Dichotomize participants based on ROI cutoff values
#' 
#' See references()$Aizenstein. The authors proposed a standardized method of 
#' calculating PIB+ cutoff values to classify participants as PIB+ or PIB-. They
#' used the DVR from 7 ROIs associated with amyloid deposition. This function 
#' takes the ROI-based cutoff-values, e.g. from cutoff_aiz(), and returns a 
#' table specifying which participants are positive, i.e. which have at least
#' one ROI greater than the cutoff.
#' 
#' @export
#' @param modelstats SUVR or DVR data for group of participants from batch_tm()
#' @param cutoff cutoffs for ROIs as from cutoff_aiz()
#' @return data.frame of participants and positive/negative status
pos_anyroi <- function(modelstats, cutoff) {
  pos_tab <- modelstats
  pos_tab[,] <- NA 
  for (j in 1:length(cutoff)) {
    pos_tab[,names(cutoff)[j]] <- modelstats[,names(cutoff[j])] > cutoff[j]
  } 
  pos <- apply(pos_tab, 1, any, na.rm=T)
  return(pos)
}

# Helper functions--------------------------------------------------------------

#' @noRd
upper_inner_fence <- function(vector) {
  uif <- quantile(vector, 0.75, type=7) + (1.5*IQR(vector, type=7))
  return(as.numeric(uif)) 
}
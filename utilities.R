##################################
## PET Analysis in R            ##
## utilities.R                  ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

source("ROI_definitions.R")


# Append text to headers in a data.frame
# Useful, for example, before merging 2 data frames that have the same header.
addColumnHeader <- function(data, headersuffix) {
  data <- as.data.frame(data)
  names(data) <- paste(names(data), headersuffix, sep="")
  return(data)
}


# Adds a column to a data frame from another CSV file, toaddfile.
# Adds the columns with the names from the vector varnames.
growDF <- function(master, toaddfile, varnames) {
  addfrom <- read.table(toaddfile, header=TRUE, sep=",")
  masterdf <- as.data.frame(master)
  for (varname in varnames) {
    masterdf <- data.frame(masterdf, addfrom[, varname])
    colnames(masterdf)[ncol(masterdf)] <- varname
  }
  return(masterdf)
}


mean_table <- function(ROI_def) {
  means <- data.frame(
    row.names = ROI_def@all,
    mean = rep(0, length(ROI_def@all)),
    proportion_of_hemilobe = rep(0, length(ROI_def@all)),
    proportion_of_lobe = rep(0, length(ROI_def@all)),
    proprtion_of_total = rep(0, length(ROI_def@all))
  )
  return(means)
}


fill_table <- function(means, storedvalues, ROI_def, proportiontable,
                       headername) {
  for (subROI in ROI_def@all) {
    means[subROI, "mean"] <- storedvalues[subROI, headername]
    means[subROI, "proportion_of_hemilobe"] <- proportiontable[subROI,
                                                       "proportion_of_hemilobe"]
    means[subROI, "proportion_of_lobe"] <- proportiontable[subROI,
                                                           "proportion_of_lobe"]
    means[subROI, "proportion_of_total"] <- proportiontable[subROI,
                                                          "proportion_of_total"]
  }
  return(means)
}


# BPnd data can be copied from PNEURO and saved as a CSV. It contains ROI volume 
# information. This extracts that.
volumesFromBPndPaste <- function(BPnd_file) {
  BPnd <- read.csv(BPnd_file, header=TRUE, row.names=1)
  return(BPnd["Volume..ccm."])
}

# TAC .voistat files contain volume information for each ROI. 
# This extracts that. Common time is simply a frame time that appears once in 
# each ROI.
volumesFromVoistatTAC <- function(voistat_file, commontime="30") {
  voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T,
    stringsAsFactors=F)
  # create a list of each unique ROI name
  ROIs <- unique(voistat[,"VoiName.Region...string."])
  # subset the voistat data to include just a single time frame, since
  # volume is same for each time
  u <- voistat$Time..seconds.==commontime
  # create a list of the volumes for each ROI.
  Volume..ccm. <- voistat[, "Volume..ccm."][u]
  return(data.frame(ROIs, Volume..ccm., row.names=1))
}


#Simple function that adds "_C" to a string if PVC is true
correct <- function(PVC, subROI) {
  if (PVC) {
    subROI <- paste(subROI, "_C", sep="")
  }
  return(subROI)
}


create_final_table <- function(ROI_def, header) {
  final_table <- data.frame(
    row.names = c(names(ROI_def@hemilobe), names(ROI_def@lobe), "totalcortical"),
    data_name = rep(0, length(c(names(ROI_def@hemilobe), names(ROI_def@lobe)))+1))
  names(final_table) <- header
  return(final_table)
}


fill_means_table <- function(single_mean, subROI, means, proportiontable) {
  means[subROI, "mean"] <- single_mean
  means[subROI, "proportion_of_hemilobe"] <- proportiontable[subROI,
                                                       "proportion_of_hemilobe"]
  means[subROI, "proportion_of_lobe"] <- proportiontable[subROI,
                                                         "proportion_of_lobe"]
  means[subROI, "proportion_of_total"] <- proportiontable[subROI,
                                                          "proportion_of_total"]
  return(means)
}


# These steps calculate weighted avg for each hemilobe/lobe by iterating through 
# each ROI name (from hemilobe names) and ROI in ROI_def@hemilobe/lobe. This 
# speaks to the critical importance of both sources having the same order, so be 
# cautious if changing the standardROIs()/fullROIs() function.
weighted_average <- function(ROI_def_val, ROI_def_names, means, finaltable,
                             headername, proportion_of_text) {
  counter <- 1
  temp <- 0

  for (ROI in ROI_def_val) {
    for (subROI in ROI) {
      temp <- temp + (means[subROI, "mean"] * means[subROI, proportion_of_text])
    }
  
    finaltable[ROI_def_names[counter], headername] <- temp
    temp <- 0
    counter <- counter + 1
  }

  return(finaltable)
}


### Utilities for TAC calculation.

# This creates a data.frame to hold the mean TAC from the ROIs specified in 
# ROI_def. 
emptyTACtable <- function(tac_file, sep="", ROI_def=standardROIs(), 
  do_total_cortical=TRUE, merge=F) {
  
  tac <- read.csv(tac_file, sep=sep)

  #Warning: ensure the tac file has first 2 columns = start and end
  TACtable <- tac[1:2]
  names(TACtable) <- c("start", "end")
  frames <- length(TACtable$start)

  ROIs <- c(names(ROI_def@hemilobe), names(ROI_def@lobe))
  for (ROI in ROIs) {
    TACtable <- data.frame(TACtable, rep(0, frames))
    names(TACtable)[ncol(TACtable)] <- ROI
    }

  if (do_total_cortical) {
    totalcortical <- rep(0, frames)
    TACtable <- data.frame(TACtable, totalcortical)
    }

  if (merge) {
    TACtable <- data.frame(TACtable, (tac*0))
  }
  
  return(TACtable)
  }


# This creates TACs for ROIs as specified in ROI_def, i.e. takes the weighted
# average of TACs of each region that makes up the ROI.
weighted_TAC <- function(ROI_def_val, ROI_def_names, tac, TACtable,
                             proportion_of_text, vols) {
  # number of time points in the TAC
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



## SUVR and related functions.

# In order to find the average SUVR, BPnd, or other value for an ROI, the 
# relative size of each region must be calculated so that the average is
# weighted appropriately. This function calculates the weigtings.
# For ROI definitions, use ROI_definitions.R
# For rawvolumes, use utilities.R (volumesFromBPndPaste, volumesFromVoistatTAC)
calcRelativeVolumes <- function(rawvolumes, ROI_def) {

  # Prepare the output table.
  proportion_of_hemilobe <- rep(NA, length(rownames(rawvolumes)))
  proportion_of_lobe <- rep(NA, length(rownames(rawvolumes)))
  proportion_of_total <- rep(NA, length(rownames(rawvolumes)))
  proportiontable <- data.frame(row.names=rownames(rawvolumes), 
                proportion_of_lobe, proportion_of_hemilobe, proportion_of_total)
 
  # first iterates through each ROI in hemilobe, for example "leftfrontal"
  
  for (ROI in ROI_def@hemilobe) {
    # total is the sum of the ROI within hemilobe
    # e.g. total for "leftfrontal"
    total <- sum(rawvolumes[ROI, "Volume..ccm."])
    # now going within e.g. leftfrontal, to get the proportion
    # each atlas ROI makes of e.g. leftfrontal
    for (subROI in ROI) {
      proportiontable[subROI, "proportion_of_hemilobe"] <- rawvolumes[subROI, 
                                                         "Volume..ccm."] / total
    }
  }
  
  for (ROI in ROI_def@lobe) {
    lobetotal <- sum(rawvolumes[ROI, "Volume..ccm."])
    for (subROI in ROI) {
      proportiontable[subROI, "proportion_of_lobe"] <- rawvolumes[subROI, 
                                                     "Volume..ccm."] / lobetotal
    }
  }

  totalcort <- sum(rawvolumes[unlist(ROI_def@totalcortical), "Volume..ccm."])

  for (subROI in ROI_def@totalcortical) {
    proportiontable[subROI, "proportion_of_total"] <- rawvolumes[subROI, 
                                                     "Volume..ccm."] / totalcort
  }

  return(proportiontable)
}

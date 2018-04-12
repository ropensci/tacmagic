##################################
## PMOD Output Analysis in R	##
## calculateSUVR.R		##
## Eric E. Brown		##
## v 0.1.7--in progress		##
##################################

# Also see PIB_pos.R for calculating PIB+ cutoff.
# Also see ROI_definitions.R for standardROIs() and fullROIs()

source("ROI_definitions.R")
source("utilities.R")


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
			proportiontable[subROI, "proportion_of_hemilobe"] <- rawvolumes[subROI, "Volume..ccm."] / total
		}
	}
	
	for (ROI in ROI_def@lobe) {
		lobetotal <- sum(rawvolumes[ROI, "Volume..ccm."])
		for (subROI in ROI) {
			proportiontable[subROI, "proportion_of_lobe"] <- rawvolumes[subROI, "Volume..ccm."] / lobetotal
		}
	}

	totalcort <- sum(rawvolumes[unlist(ROI_def@totalcortical), "Volume..ccm."])
	for (subROI in ROI_def@totalcortical) {
		proportiontable[subROI, "proportion_of_total"] <- rawvolumes[subROI, "Volume..ccm."] / totalcort

	}

	return(proportiontable)
}

# calcSUVR
# Use relative volumes to calculate weighted SUVRs
# proportiontable is the output of calcRelativeVolumes()
# TAC_file is the name and path of the TAC_file
# SUVR_def is a vector of the start times for the TACs to be used 
# for example: c("3000", "3300", "3600", "3900")
calcSUVR <- function(TAC_file, ROI_def, proportiontable, SUVR_def, corrected=TRUE) {
	# Open the TAC file and get the number of TAC values in the SUVR definition (e.g. SUVR40-60)
	tac <- read.table(TAC_file, header=TRUE, row.names=1)
	denominator <- length(SUVR_def)

	# Creates a data.frame to store the means (over the SUVR window) and relative volumes of each ROI
	means <- mean_table(ROI_def)

	# This fills in the mean table. Note the _C is added to get the PVC-corrected values by default, unless corrected=FALSE
	for (subROI in ROI_def@all) {
		if (corrected) {
			subROI_C <- paste(subROI, "_C", sep="")
		}	else {
			subROI_C <- paste(subROI)
			}
		means[subROI, "mean"] <- sum(tac[SUVR_def, subROI_C])/denominator
		means[subROI, "proportion_of_hemilobe"] <- proportiontable[subROI, "proportion_of_hemilobe"]
		means[subROI, "proportion_of_lobe"] <- proportiontable[subROI, "proportion_of_lobe"]
		means[subROI, "proportion_of_total"] <- proportiontable[subROI, "proportion_of_total"]
	}

	# Creating another data frame to store the calculated SUVRs, which will be returned.
	SUVRtable <- data.frame(
		row.names = c(ROI_def@hemilobenames, ROI_def@lobenames, "totalcortical"),
		SUVR = rep(0, length(c(ROI_def@hemilobenames, ROI_def@lobenames))+1)
		)

	# Gets the cerebellum value to use as reference and calculate SUVR with
	cerebellumreference <- (means["Cerebellum_l", "mean"] * means["Cerebellum_l", "proportion_of_lobe"]) + 
		(means["Cerebellum_r", "mean"] * means["Cerebellum_r", "proportion_of_lobe"])

	# This step calculates the SUVR for each hemilobe by iterating through each ROI name (from hemilobe names)
	# and ROI in ROI_def@hemilobe. This speaks to the critical importance of both sources having the same 
	# order, so be cautious if changing the standardROIs() function.
	counter <- 1
	SUVRtemp <- 0
	for (ROI in ROI_def@hemilobe) {
		for (subROI in ROI) {
			SUVRtemp <- SUVRtemp + (means[subROI, "mean"] * means[subROI, "proportion_of_hemilobe"])
		}
		SUVRtable[ROI_def@hemilobenames[counter], "SUVR"] <- SUVRtemp/cerebellumreference
		SUVRtemp <- 0
		counter <- counter + 1
	}

	# As above, this step does the same for the full lobes.
	counter <- 1
	SUVRtemp <- 0
	for (ROI in ROI_def@lobe) {
		for (subROI in ROI) {
			SUVRtemp <- SUVRtemp + (means[subROI, "mean"] * means[subROI, "proportion_of_lobe"])
		}
		SUVRtable[ROI_def@lobenames[counter], "SUVR"] <- SUVRtemp/cerebellumreference
		SUVRtemp <- 0
		counter <- counter + 1
	}

	# This step is similar as above but for the total cortical ROI. The main difference is that it is only
	# one ROI, so there is only one for loop.
	SUVRtemp <- 0
	for (subROI in ROI_def@totalcortical) {
		SUVRtemp <- sum(SUVRtemp, ((means[subROI, "mean"] * means[subROI, "proportion_of_total"])))
	}
		SUVRtable["totalcortical", "SUVR"] <- SUVRtemp/cerebellumreference

	return(SUVRtable)
}


# .voistat files for each model contain data for each subROI, this 
# extracts mean data.
voistatScraper <- function(voistat_file, ROI_def=standardROIs()) {

	voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T, stringsAsFactors=F)
	ROIs <- voistat$VoiName.Region...string.
	Volume..ccm. <- voistat$Volume..ccm.
	Averaged..1.1. <- voistat$Averaged..1.1.	

	rawvolumes <- data.frame(ROIs, Volume..ccm., row.names=1)
	storedvalues <- data.frame(ROIs, Averaged..1.1., row.names=1)
	# Calculate the relative volumes for the subROIs and ROIs
	proportiontable <- calcRelativeVolumes(rawvolumes, ROI_def)
	
	summarytable <- data.frame(
		row.names = c(ROI_def@hemilobenames, ROI_def@lobenames, "totalcortical"),
		VALUE = rep(0, length(c(ROI_def@hemilobenames, ROI_def@lobenames))+1)
		)
	
	# Creates a mean table (data.frame to store the means and relative 
	# volumes of each ROI), and fills it.
	means <- fill_table(mean_table(ROI_def), storedvalues, ROI_def, 
		proportiontable, headername="Averaged..1.1.")
	
	# Creating another data frame to store the calculated SUVRs, which will be returned.
	VALUEtable <- data.frame(
		row.names = c(ROI_def@hemilobenames, ROI_def@lobenames, "totalcortical"),
		VALUE = rep(0, length(c(ROI_def@hemilobenames, ROI_def@lobenames))+1)
		)

	# This step calculates the value for each hemilobe by iterating through each ROI name (from hemilobe names)
	# and ROI in ROI_def@hemilobe. This speaks to the critical importance of both sources having the same 
	# order, so be cautious if changing the standardROIs() function.
	counter <- 1	
	VALUEtemp <- 0
	for (ROI in ROI_def@hemilobe) {
		for (subROI in ROI) {
			VALUEtemp <- VALUEtemp + (means[subROI, "mean"] * means[subROI, "proportion_of_hemilobe"])
		}
		VALUEtable[ROI_def@hemilobenames[counter], "VALUE"] <- VALUEtemp
		VALUEtemp <- 0
		counter <- counter + 1
	}

	# As above, this step does the same for the full lobes.
	counter <- 1		
	VALUEtemp <- 0
	for (ROI in ROI_def@lobe) {
		for (subROI in ROI) {
			VALUEtemp <- VALUEtemp + (means[subROI, "mean"] * means[subROI, "proportion_of_lobe"])
		}
		VALUEtable[ROI_def@lobenames[counter], "VALUE"] <- VALUEtemp
		VALUEtemp <- 0
		counter <- counter + 1
	}

	# This step is similar as above but for the total cortical ROI. The main difference is that it is only
	# one ROI, so there is only one for loop.
	VALUEtemp <- 0
	for (subROI in ROI_def@totalcortical) {
		VALUEtemp <- sum(VALUEtemp, ((means[subROI, "mean"] * means[subROI, "proportion_of_total"])))
	}
		VALUEtable["totalcortical", "VALUE"] <- VALUEtemp

	return(VALUEtable)
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

# Takes the slopes calculated in peakSlope, and merges them into ROIs as specified
peakSlopeROI <- function(slopes, ROI_def, proportiontable, corrected=TRUE) {

	# Creates a data.frame to store the slopes and relative volumes of each ROI
	
	means <- mean_table(ROI_def)

	for (subROI in ROI_def@all) {
		means[subROI, "mean"] <- slopes[, correct(corrected, subROI)]
		means[subROI, "proportion_of_hemilobe"] <- proportiontable[subROI, "proportion_of_hemilobe"]
		means[subROI, "proportion_of_lobe"] <- proportiontable[subROI, "proportion_of_lobe"]
		means[subROI, "proportion_of_total"] <- proportiontable[subROI, "proportion_of_total"]
	}

	# Creating another data frame to store the calculated SUVRs, which will be returned.
	slope_table <- data.frame(
		row.names = c(ROI_def@hemilobenames, ROI_def@lobenames, "totalcortical"),
		slope = rep(0, length(c(ROI_def@hemilobenames, ROI_def@lobenames))+1)
		)

	# This step calculates the slope for each hemilobe by iterating through each ROI name (from hemilobe names)
	# and ROI in ROI_def@hemilobe. This speaks to the critical importance of both sources having the same 
	# order, so be cautious if changing the standardROIs() function.
	counter <- 1
	temp <- 0
	for (ROI in ROI_def@hemilobe) {
		for (subROI in ROI) {
			temp <- temp + (means[subROI, "mean"] * means[subROI, "proportion_of_hemilobe"])
		}
		slope_table[ROI_def@hemilobenames[counter], "slope"] <- temp
		temp <- 0
		counter <- counter + 1
	}

	# As above, this step does the same for the full lobes.
	counter <- 1
	temp <- 0
	for (ROI in ROI_def@lobe) {
		for (subROI in ROI) {
			temp <- temp + (means[subROI, "mean"] * means[subROI, "proportion_of_lobe"])
		}
		slope_table[ROI_def@lobenames[counter], "slope"] <- temp
		temp <- 0
		counter <- counter + 1
	}

	# This step is similar as above but for the total cortical ROI. The main difference is that it is only
	# one ROI, so there is only one for loop.
	temp <- 0
	for (subROI in ROI_def@totalcortical) {
		temp <- sum(temp, ((means[subROI, "mean"] * means[subROI, "proportion_of_total"])))
	}
		slope_table["totalcortical", "slope"] <- temp

	return(slope_table)
}



	

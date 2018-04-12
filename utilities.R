##################################
## PMOD Output Analysis in R	##
## utilities.R			##
## Eric E. Brown		##
## v 0.1.7--in progress		##
##################################


##################################
##	GENERIC UTILITIES	##
##################################


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



fill_table <- function(means, storedvalues, ROI_def, proportiontable, headername) {

	for (subROI in ROI_def@all) {
		means[subROI, "mean"] <- storedvalues[subROI, headername]
		means[subROI, "proportion_of_hemilobe"] <- proportiontable[subROI, "proportion_of_hemilobe"]
		means[subROI, "proportion_of_lobe"] <- proportiontable[subROI, "proportion_of_lobe"]
		means[subROI, "proportion_of_total"] <- proportiontable[subROI, "proportion_of_total"]
	}
	
	return(means)
}

#sum(tac[SUVR_def, subROI_C])/denominator



##################################
##    PMOD-RELATED UTILITIES	##
##################################

	
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


fill_means_table <- function(single_mean, subROI, means, proportiontable) {
	means[subROI, "mean"] <- single_mean
	means[subROI, "proportion_of_hemilobe"] <- proportiontable[subROI, "proportion_of_hemilobe"]
	means[subROI, "proportion_of_lobe"] <- proportiontable[subROI, "proportion_of_lobe"]
	means[subROI, "proportion_of_total"] <- proportiontable[subROI, "proportion_of_total"]

	return(means)
}

weighted_average <- function(ROI_def_val, ROI_def_names, means, finaltable, headername, proportion_of_text) {
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




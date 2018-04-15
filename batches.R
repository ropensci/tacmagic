##################################
## PMOD Output Analysis in R	##
## calculateSUVR.R		##
## Eric E. Brown		##
## v 0.1.7--in progress		##
##################################

source("calculateSUVR.R")
source("utilities.R")
source("ROI_definitions.R")

# This function runs the SUVR calculation on a list of participants specified in a vector of participant
# IDs, where there are corresponding files called ID_BPnd.csv and ID_TAC.tac. It returns the data frame
# and also saves a CSV file (name in arguments)
batchSUVR <- function(participants, ROI_def, SUVR_def, outputfilename, corrected=TRUE, volfromBPnd=FALSE, tacfilesuffix=".tac") {

	if (volfromBPnd) {
		BPnd_file = paste(participants[1], "_BPnd.csv", sep="")
		vols <- calcRelativeVolumes(volumesFromBPndPaste(BPnd_file), ROI_def)
	}	else {
		voistat_file = paste(participants[1], ".voistat", sep="")
		vols <- calcRelativeVolumes(volumesFromVoistatTAC(voistat_file), ROI_def)
		}
	TAC_file = paste(participants[1], tacfilesuffix, sep="")
	
	first <- calcSUVR(TAC_file, ROI_def, vols, SUVR_def, corrected)
	master <- t(first)
	master <- master[-1,]

	for (each in participants) {
		print(paste("Working on...", each))
		TAC_file = paste(each, tacfilesuffix, sep="")
	
		if (volfromBPnd) {
		BPnd_file = paste(each, "_BPnd.csv", sep="")
		vols <- calcRelativeVolumes(volumesFromBPndPaste(BPnd_file), ROI_def)
		}	else {
			voistat_file = paste(each, ".voistat", sep="")
			vols <- calcRelativeVolumes(volumesFromVoistatTAC(voistat_file), ROI_def)
			}
		
		BPnd_file = paste(each, "_BPnd.csv", sep="")
		
		SUVR <- calcSUVR(TAC_file, ROI_def, vols, SUVR_def, corrected)
		trans <- t(SUVR)
		row.names(trans) <- each
		master <- rbind(master,trans)
	}	
	write.csv(master, file = outputfilename)

	return(master)
}

#Batch slope
batchSlope <- function(participants, ROI_def, outputfilename, corrected=TRUE, volfromBPnd=FALSE, tacfilesuffix=".tac") {

	if (volfromBPnd) {
		BPnd_file = paste(participants[1], "_BPnd.csv", sep="")
		vols <- calcRelativeVolumes(volumesFromBPndPaste(BPnd_file), ROI_def)
	}	else {
		voistat_file = paste(participants[1], ".voistat", sep="")
		vols <- calcRelativeVolumes(volumesFromVoistatTAC(voistat_file), ROI_def)
		}
	TAC_file = paste(participants[1], tacfilesuffix, sep="")
	
	cat("Current TAC file:", TAC_file)	

	firstslope <- peakSlope(TAC_file)
	first <- peakSlopeROI(firstslope, ROI_def, vols, corrected)
	master <- t(first)
	master <- master[-1,]
	
	for (each in participants) {
		print(paste("Working on...", each))
		TAC_file = paste(each, tacfilesuffix, sep="")
	
		if (volfromBPnd) {
		BPnd_file = paste(each, "_BPnd.csv", sep="")
		vols <- calcRelativeVolumes(volumesFromBPndPaste(BPnd_file), ROI_def)
		}	else {
			voistat_file = paste(each, ".voistat", sep="")
			vols <- calcRelativeVolumes(volumesFromVoistatTAC(voistat_file), ROI_def)
			}
		
		BPnd_file = paste(each, "_BPnd.csv", sep="")
		
		slope <- peakSlope(TAC_file)
		slopeROI <- peakSlopeROI(slope, ROI_def, vols, corrected)

		trans <- t(slopeROI)
		row.names(trans) <- each
		master <- rbind(master,trans)
	}	
	write.csv(master, file = outputfilename)
	return(master)
}

#A function to run voistatScraper on a list of participants
batchVoistat <- function(participants, ROI_def=standardROIs(), outputfilename, filesuffix) {

	voistat_file = paste(participants[1], filesuffix, ".voistat", sep="")

	first <- voistatScraper(voistat_file, ROI_def)
	master <- t(first)
	master <- master[-1,]

	for (each in participants) {
		print(paste("Working on...", each))
		voistat_file = paste(each, filesuffix, ".voistat", sep="")
		VALUE <- voistatScraper(voistat_file, ROI_def)
		trans <- t(VALUE)
		row.names(trans) <- each
		master <- rbind(master,trans)
	}	
	write.csv(master, file = outputfilename)
	return(master)
}

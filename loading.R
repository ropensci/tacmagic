##################################
## PET Analysis in R            ##
## loading.R                    ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################



# PMOD Files

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


# .voistat files for each model (e.g Logan, not TAC) contain data for each
# subROI, this extracts mean data for aggregate ROIs based on ROI_def.
voistatScraper <- function(voistat_file, ROI_def=standardROIs(), model="VALUE") {
    
    voistat <- read.csv(voistat_file, sep="\t", skip=6, header=T,
    stringsAsFactors=F)
    ROIs <- voistat$VoiName.Region...string.
    Volume..ccm. <- voistat$Volume..ccm.
    Averaged..1.1. <- voistat$Averaged..1.1.
    
    rawvolumes <- data.frame(ROIs, Volume..ccm., row.names=1)
    storedvalues <- data.frame(ROIs, Averaged..1.1., row.names=1)
    # Calculate the relative volumes for the subROIs and ROIs
    proportiontable <- calcRelativeVolumes(rawvolumes, ROI_def)
    
    summarytable <- data.frame(
    row.names = c(names(ROI_def@hemilobe), names(ROI_def@lobe), "totalcortical"),
    VALUE = rep(0, length(c(names(ROI_def@hemilobe), names(ROI_def@lobe)))+1)
    )
    
    # Creates a mean table (data.frame to store the means and relative
    # volumes of each ROI), and fills it.
    means <- fill_table(mean_table(ROI_def), storedvalues, ROI_def,
    proportiontable, headername="Averaged..1.1.")
    
    VALUEtable <- create_final_table(ROI_def, model)
    
    VALUEtable <- weighted_average(ROI_def@hemilobe, names(ROI_def@hemilobe),
    means, VALUEtable, model, "proportion_of_hemilobe")
    VALUEtable <- weighted_average(ROI_def@lobe, names(ROI_def@lobe), means,
    VALUEtable, model, "proportion_of_lobe")
    VALUEtable <- weighted_average(ROI_def@totalcortical, "totalcortical",
    means, VALUEtable, model, "proportion_of_total")
    
    return(VALUEtable)
}

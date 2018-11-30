


legacy_calcTAC <- function(tac, raw_volumes, ROI_def, merge=F) {
    vols <- calcRelativeVolumes(raw_volumes, ROI_def)
    TACtable <- emptyTACtable(tac, ROI_def)
    
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


#' Loads model data from file for use by other functions.
#'
#'@param filename (e.g. participant_logan.voistat)
#'@param format (default is the TAC .voistat format from PMOD).
#'@return data.frame with loaded model data in specified combined weighted ROIs.
#'@examples loadVolumes("/dir/participant1_TAC.voistat")
legacy_voistatScraper <- function(voistat_file, ROI_def=standardROIs(), model="VALUE") {
    
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

#' Calculate weighted SUVRs for specified regions of interest
#'
#' When smaller ROIs need to be combined into larger ROIs, e.g. when TACs have
#' been calculated for components of the frontal lobe, but an SUVR is desired
#' for the entire frontal lobe, the TACs need to be combined, and the relative
#' volumes of each needs to be taken into account. This function calculates
#' those weighted means.
#'
#'@param tac The time-activity curve data from loading function.
#'@param volumes The ROI volume data from loading function
#'@param SUVR_def is a vector of the start times for window to be used in SUVR
#'@param corrected For PVC, true where the data is stored as _C in same tac file
#'@return A table of SUVR values for the specified ROIs
#'@examples
#' calcSUVR(p1tac, p1vol, standardROIs(), c("3000", "3300", "3600", "3900"))
legacy_calcSUVR <- function(tac, volumes, ROI_def, SUVR_def, corrected=TRUE) {
    
    if (FALSE == verify_window_durations(tac, SUVR_def)) {
        warning("Frames durations are of unequal length.")
    }
    
    tac <- data.frame(tac, row.names=1)
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


# create a class ROIset with two slots, one for each list of ROIs, so
# that it can be returned as a single object
ROIset <- setClass("ROIset", slots = c(
hemilobe = "list", lobe = "list",
totalcortical = "list", all = "vector"
))


# Functions to generate an object of class ROIset, containing lists that define
# your ROIs. Use these directly or as templates to define your ROIs (as made up
# of header names of columns of your TAC files).

# There must be 2 sets of ROIs: one that is specicific to L/R
# ("hemilobe" e.g. L frontal) and one that is whole-brain (e.g. frontal)



# standardROIs() includes all major cortical lobes
legacy_standardROIs <- function() {
    
    frontal_def <- c("FL_mid_fr_G", "FL_precen_G", "FL_strai_G", "FL_OFC_AOG",
    "FL_inf_fr_G", "FL_sup_fr_G", "FL_OFC_MOG", "FL_OFC_LOG", "FL_OFC_POG",
    "Subgen_antCing", "Subcall_area", "Presubgen_antCing")
    temporal_def <- c("Hippocampus", "Amygdala", "Ant_TL_med", "Ant_TL_inf_lat",
    "G_paraH_amb", "G_sup_temp_post", "G_tem_midin", "G_fus", "Post_TL",
    "G_sup_temp_ant")
    parietal_def <- c("PL_postce_G", "PL_sup_pa_G", "PL_rest")
    occipital_def <- c("OL_rest_lat", "OL_ling_G", "OL_cuneus")
    cingulate_def <- c("G_cing_ant", "G_cing_post")
    
    leftfrontal <- paste(frontal_def, "_l", sep="")
    rightfrontal <- paste(frontal_def, "_r", sep="")
    frontal <- c(leftfrontal, rightfrontal)
    
    lefttemporal <- paste(temporal_def, "_l", sep="")
    righttemporal <- paste(temporal_def, "_r", sep="")
    temporal <- c(lefttemporal, righttemporal)
    
    leftparietal <- paste(parietal_def, "_l", sep="")
    rightparietal <- paste(parietal_def, "_r", sep="")
    parietal <- c(leftparietal, rightparietal)
    
    leftoccipital <- paste(occipital_def, "_l", sep="")
    rightoccipital <- paste(occipital_def, "_r", sep="")
    occipital <- c(leftoccipital, rightoccipital)
    
    leftcingulate <- paste(cingulate_def, "_l", sep="")
    rightcingulate <- paste(cingulate_def, "_r", sep="")
    cingulate <- c(leftcingulate, rightcingulate)
    
    cerebellum <- c("Cerebellum_l", "Cerebellum_r")
    
    hemilobeROIs <- list(leftfrontal = leftfrontal, rightfrontal=rightfrontal,
    lefttemporal=lefttemporal, righttemporal=righttemporal,
    leftparietal=leftparietal, rightparietal=rightparietal,
    leftoccipital=leftoccipital, rightoccipital=rightoccipital,
    leftcingulate=leftcingulate, rightcingulate=rightcingulate)
    
    lobeROIs <- list(frontal=frontal, temporal=temporal, parietal=parietal,
    occipital=occipital, cingulate=cingulate,
    cerebellum=cerebellum)
    
    totalcortical <- list(c(frontal, temporal, parietal, occipital, cingulate))
    
    allsubROIs <- c(frontal, temporal, parietal, occipital, cingulate, cerebellum)
    
    standardROIs <- ROIset()
    slot(standardROIs, "hemilobe") <- hemilobeROIs
    slot(standardROIs, "lobe") <- lobeROIs
    slot(standardROIs, "totalcortical") <- totalcortical
    slot(standardROIs, "all") <- allsubROIs
    
    return(standardROIs)
}



legacy_fullROIs <- function() {
    #hemilobe (first list)
    
    frontal_def <- c("FL_mid_fr_G", "FL_precen_G", "FL_strai_G", "FL_OFC_AOG",
    "FL_inf_fr_G", "FL_sup_fr_G", "FL_OFC_MOG", "FL_OFC_LOG", "FL_OFC_POG",
    "Subgen_antCing", "Subcall_area", "Presubgen_antCing")
    temporal_def <- c("Hippocampus", "Amygdala", "Ant_TL_med", "Ant_TL_inf_lat",
    "G_paraH_amb", "G_sup_temp_post", "G_tem_midin", "G_fus", "Post_TL",
    "G_sup_temp_ant")
    parietal_def <- c("PL_postce_G", "PL_sup_pa_G", "PL_rest")
    occipital_def <- c("OL_rest_lat", "OL_ling_G", "OL_cuneus")
    cingulate_def <- c("G_cing_ant", "G_cing_post")
    deep_def <- c("CaudateNucl", "NuclAccumb", "Putamen", "Thalamus", "Pallidum")
    
    leftfrontal <- paste(frontal_def, "_l", sep="")
    rightfrontal <- paste(frontal_def, "_r", sep="")
    frontal <- c(leftfrontal, rightfrontal)
    
    lefttemporal <- paste(temporal_def, "_l", sep="")
    righttemporal <- paste(temporal_def, "_r", sep="")
    temporal <- c(lefttemporal, righttemporal)
    
    leftparietal <- paste(parietal_def, "_l", sep="")
    rightparietal <- paste(parietal_def, "_r", sep="")
    parietal <- c(leftparietal, rightparietal)
    
    leftoccipital <- paste(occipital_def, "_l", sep="")
    rightoccipital <- paste(occipital_def, "_r", sep="")
    occipital <- c(leftoccipital, rightoccipital)
    
    leftcingulate <- paste(cingulate_def, "_l", sep="")
    rightcingulate <- paste(cingulate_def, "_r", sep="")
    cingulate <- c(leftcingulate, rightcingulate)
    
    leftdeep <- paste(deep_def, "_l", sep="")
    rightdeep <- paste(deep_def, "_r", sep="")
    deep <- c(leftdeep, rightdeep)
    
    corpus <- c("Corp_Callosum")
    cerebellum <- c("Cerebellum_l", "Cerebellum_r")
    brainstem <- c("Brainstem")
    ventricles <- c("FrontalHorn_l", "FrontalHorn_r", "TemporaHorn_r",
    "TemporaHorn_l", "ThirdVentricl")
    whitematter <- c("White_matter_l", "White_matter_r")
    
    hemilobeROIs <- list(leftfrontal=leftfrontal, rightfrontal=rightfrontal,
    lefttemporal=lefttemporal, righttemporal=righttemporal,
    leftparietal=leftparietal, rightparietal=rightparietal,
    leftoccipital=leftoccipital, rightoccipital=rightoccipital,
    leftcingulate=leftcingulate, rightcingulate=rightcingulate,
    leftdeep=leftdeep, rightdeep=rightdeep)
    
    lobeROIs <- list(frontal=frontal, temporal=temporal, parietal=parietal,
    occipital=occipital, cingulate=cingulate,deep=deep,
    corpus=corpus, cerebellum=cerebellum, brainstem=brainstem,
    ventricles=ventricles, whitematter=whitematter)
    
    totalcortical <- list(c(frontal, temporal, parietal, occipital, cingulate))
    
    allsubROIs <- c(frontal, temporal, parietal, occipital, cingulate, deep,
    corpus, cerebellum, brainstem, ventricles, whitematter)
    
    
    fullROIs <- ROIset()
    slot(fullROIs, "hemilobe") <- hemilobeROIs
    slot(fullROIs, "lobe") <- lobeROIs
    slot(fullROIs, "totalcortical") <- totalcortical
    slot(fullROIs, "all") <- allsubROIs
    
    return(fullROIs)
}


# These steps calculate weighted avg for each hemilobe/lobe by iterating through
# each ROI name (from hemilobe names) and ROI in ROI_def@hemilobe/lobe. This
# speaks to the critical importance of both sources having the same order, so be
# cautious if changing the standardROIs()/fullROIs() function.
legacy_weighted_average <- function(ROI_def_val, ROI_def_names, means, finaltable,
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

verify_window_durations <- function(tac, window) {
    
    frame_durations <- tac$end - tac$start
    window_durations <- frame_durations[tac$start.seconds. %in% window]
    if (var(window_durations) == 0) { # 0 if all lengths are equal.
        all_equal <- TRUE
    } else {
        all_equal <- FALSE
    }
    return (all_equal)
}


### Utilities for TAC calculation.

# This creates a data.frame to hold the mean TAC from the ROIs specified in
# ROI_def.
legacy_emptyTACtable <- function(tac, ROI_def, do_total_cortical=TRUE, merge=F) {
    
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
legacy_weighted_TAC <- function(ROI_def_val, ROI_def_names, tac, TACtable,
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
legacy_calcRelativeVolumes <- function(rawvolumes, ROI_def) {
    
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


## Legacy utilities

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

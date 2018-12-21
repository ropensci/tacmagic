##################################
## PET Analysis in R            ##
## ROI_definitions.R            ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

# ROI definitions file.

#' Return a list of larger ROIs made up of the ROIs in the Hammer's atlas (see 
#' references()).
#'
#'@return A list of lists, where each list is an ROI (e.g.) frontal lobe that 
#' specifies the atomic ROIs from the atlas that make it up.
#'@examples standardROIs()
standardROIs <- function() {
    
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
    
  totalcortical <- c(frontal, temporal, parietal, occipital, cingulate)
    
  ROIs <- list(
  leftfrontal=leftfrontal, rightfrontal=rightfrontal,
  lefttemporal=lefttemporal, righttemporal=righttemporal,
  leftparietal=leftparietal, rightparietal=rightparietal,
  leftoccipital=leftoccipital, rightoccipital=rightoccipital,
  leftcingulate=leftcingulate, rightcingulate=rightcingulate,
    
  frontal=frontal, temporal=temporal, parietal=parietal,
  occipital=occipital, cingulate=cingulate,
    
  cerebellum=cerebellum, totalcortical=totalcortical)
  
  return(ROIs)
}

#' Return a list of larger ROIs made up of the ROIs in the Hammer's atlas.
#'
#' This includes the cortical regions of standardROIs() but also other regions.
#' It can be modified to suit the user's needs.
#'
#'@return A list of lists, where each list is an ROI (e.g.) frontal lobe that 
#' specifies the atomic ROIs from the atlas that make it up.
#'@examples standardROIs()
fullROIs <- function() {

    deep_def <- c("CaudateNucl", "NuclAccumb",  "Putamen", "Thalamus", 
                  "Pallidum")
    
    leftdeep <- paste(deep_def, "_l", sep="")
    rightdeep <- paste(deep_def, "_r", sep="")
    deep <- c(leftdeep, rightdeep)
    
    ventricles <- c("FrontalHorn_l", "FrontalHorn_r", "TemporaHorn_r",
    "TemporaHorn_l", "ThirdVentricl")
    whitematter <- c("White_matter_l", "White_matter_r")
    
    ROIs <- c(standardROIs(), list(leftdeep=leftdeep, rightdeep=rightdeep,
              deep=deep, ventricles=ventricles, whitematter=whitematter))
    
    return(ROIs)
}

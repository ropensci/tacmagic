##################################
## PET Analysis in R            ##
## ROI_definitions.R            ##
## Eric E. Brown                ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

# ROI definitions file.


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



fullROIs <- function() {
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

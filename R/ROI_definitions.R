##################################
## tacmagic - PET Analysis in R ##
## ROI_definitions.R            ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

# ROI definitions file.

#' Return a list of merged ROIs made up of the atomic ROIs in the Hammer's 
#' atlas.
#'
#'@export
#'@return A list of lists, where each list is an ROI (e.g.) frontal lobe that 
#' specifies the atomic ROIs from the atlas that make it up.
#'@family ROI definitions
#'@references Hammers, Alexander, Richard Allom, Matthias J. Koepp, Samantha L. Free, 
#' Ralph Myers, Louis Lemieux, Tejal N. Mitchell, David J. Brooks, and John S. 
#' Duncan. 2003. Three-dimensional Maximum Probability Atlas of the Human 
#' Brain, with Particular Reference to the Temporal Lobe. Human Brain Mapping 
#' 19 (4): 224-247. doi:10.1002/hbm.10123
#'@examples 
#' roi_ham_stand()
roi_ham_stand <- function() {
    
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
#' This includes the cortical regions of roi_ham_stand() but also other regions.
#' It can be modified to suit the user's needs.
#'
#'@export
#'@return A list of lists, where each list is an ROI (e.g.) frontal lobe that 
#' specifies the atomic ROIs from the atlas that make it up.
#'@family ROI definitions
#'@references Hammers, Alexander, Richard Allom, Matthias J. Koepp, Samantha L. Free, 
#' Ralph Myers, Louis Lemieux, Tejal N. Mitchell, David J. Brooks, and John S. 
#' Duncan. 2003. Three-dimensional Maximum Probability Atlas of the Human 
#' Brain, with Particular Reference to the Temporal Lobe. Human Brain Mapping 
#' 19 (4): 224-247. doi:10.1002/hbm.10123
#'@examples roi_ham_full()
roi_ham_full <- function() {

  deep_def <- c("CaudateNucl", "NuclAccumb",  "Putamen", "Thalamus", 
                "Pallidum")
    
  leftdeep <- paste(deep_def, "_l", sep="")
  rightdeep <- paste(deep_def, "_r", sep="")
  deep <- c(leftdeep, rightdeep)
    
  ventricles <- c("FrontalHorn_l", "FrontalHorn_r", "TemporaHorn_r",
                  "TemporaHorn_l", "ThirdVentricl")
  whitematter <- c("White_matter_l", "White_matter_r")
    
  ROIs <- c(roi_ham_stand(), list(leftdeep=leftdeep, rightdeep=rightdeep,
            deep=deep, ventricles=ventricles, whitematter=whitematter))
    
  return(ROIs)
}

#' Return a list of merged ROIs made up of atomic ROIs in the Hammer's atlas.
#'
#' This includes the ROIs from roi_ham_full and also the PIB cortical composite
#' ROI as defined in the PMOD documentation and as widely used in PIB studies.
#' See PMOD Neuro Tool (PNEURO) (Version 4.0) documentation.
#' 
#'@export
#'@return A list of lists, where each list is an ROI (e.g.) frontal lobe that 
#' specifies the atomic ROIs from the atlas that make it up.
#'@family ROI definitions
#'@references Hammers, Alexander, Richard Allom, Matthias J. Koepp, Samantha L. Free, 
#' Ralph Myers, Louis Lemieux, Tejal N. Mitchell, David J. Brooks, and John S. 
#' Duncan. 2003. Three-dimensional Maximum Probability Atlas of the Human 
#' Brain, with Particular Reference to the Temporal Lobe. Human Brain Mapping 
#' 19 (4): 224-247. doi:10.1002/hbm.10123
#'@examples 
#' roi_ham_pib()
roi_ham_pib <- function() {

  amyloidcompdef <- c("FL_mid_fr_G", "FL_strai_G", "FL_sup_fr_G", "FL_OFC_MOG", 
                    "FL_OFC_LOG", "FL_OFC_POG", "Subgen_antCing", 
                    "Subcall_area", 

                    "G_sup_temp_post", "G_tem_midin", "G_sup_temp_ant",
                    
                    "PL_sup_pa_G", "PL_rest",
                    
                    "G_cing_ant", "G_cing_post")

  leftamyloidcomp <- paste0(amyloidcompdef, "_l")
  rightamyloidcomp <- paste0(amyloidcompdef, "_r")
  amyloidcomp <- c(leftamyloidcomp, rightamyloidcomp)

  ROIs <- c(roi_ham_full(), list(amyloidcomp=amyloidcomp))

  return(ROIs)
}


## AAL Atlas

#' Return a list of merged ROIs made up of the atomic ROIs in the AAL atlas.
#' The groupings for major lobes are as described in the PMOD documentation.
#'
#'@export
#'@return A list of lists, where each list is an ROI (e.g.) frontal lobe that
#' specifies the atomic ROIs from the atlas that make it up.
#'@family ROI definitions
#'@references PMOD Groupings of the AAL Atlas.
#'
#' Automated Anatomical Labeling of Activations in SPM Using a Macroscopic Anatomical Parcellation of the MNI MRI Single-Subject Brain. N. Tzourio-Mazoyer, B. Landeau, D. Papathanassiou, F. Crivello, O. Étard, N. Delcroix, B. Mazoyer, and M. Joliot. NeuroImage 2002, 15 :273-289 <doi:10.1006/nimg.2001.0978>
#'@examples
#' roi_AAL_full()
roi_AAL_full <- function() {
   
   # Precentral_l  Precentral_r  Frontal_Sup_l  Frontal_Sup_r  Frontal_Sup_Orb_l  Frontal_Sup_Orb_r  Frontal_Mid_l  Frontal_Mid_r  Frontal_Mid_Orb_l  Frontal_Mid_Orb_r  Frontal_Inf_Oper_l  Frontal_Inf_Oper_r  Frontal_Inf_Tri_l  Frontal_Inf_Tri_r  Frontal_Inf_Orb_l  Frontal_Inf_Orb_r  Rolandic_Oper_l  Rolandic_Oper_r  Supp_Motor_Area_l  Supp_Motor_Area_r  Olfactory_l  Olfactory_r  Frontal_Sup_Medial_l  Frontal_Sup_Medial_r  Frontal_Med_Orb_l  Frontal_Med_Orb_r  Rectus_l  Rectus_r  Insula_l  Insula_r  Cingulum_Ant_l  Cingulum_Ant_r  Cingulum_Mid_l  Cingulum_Mid_r  Cingulum_Post_l  Cingulum_Post_r  Hippocampus_l  Hippocampus_r  Parahippocampus_l  Parahippocampus_r  Amygdala_l  Amygdala_r  Calcarine_l  Calcarine_r  Cuneus_l  Cuneus_r  Lingual_l  Lingual_r  Occipital_Sup_l  Occipital_Sup_r  Occipital_Mid_l  Occipital_Mid_r  Occipital_Inf_l  Occipital_Inf_r  Fusiform_l  Fusiform_r  Postcentral_l  Postcentral_r  Parietal_Sup_l  Parietal_Sup_r  Parietal_Inf_l  Parietal_Inf_r  Supra_Marginal_l  Supra_Marginal_r  Angular_l  Angular_r  Precuneus_l  Precuneus_r  Paracentral_Lobule_l  Paracentral_Lobule_r  CaudateNucl_l  CaudateNucl_r  Putamen_l  Putamen_r  Pallidum_l  Pallidum_r  Thalamus_l  Thalamus_r  Heschl_l  Heschl_r  Temporal_Sup_l  Temporal_Sup_r  Temporal_Pole_Sup_l  Temporal_Pole_Sup_r  Temporal_Mid_l  Temporal_Mid_r  Temporal_Pole_Mid_l  Temporal_Pole_Mid_r  Temporal_Inf_l  Temporal_Inf_r  Cerebellum_Crus1_l  Cerebellum_Crus1_r  Cerebellum_Crus2_l  Cerebellum_Crus2_r  Cerebellum3_l  Cerebellum3_r  Cerebellum45_l  Cerebellum45_r  Cerebellum6_l  Cerebellum6_r  Cerebellum7_l  Cerebellum7_r  Cerebellum8_l  Cerebellum8_r  Cerebellum9_l  Cerebellum9_r  Cerebellum10_l  Cerebellum10_r  Vermis12  Vermis3  Vermis45  Vermis6  Vermis7  Vermis8  Vermis9  Vermis10  Medulla  Midbrain  Pons  White_matter_l  White_matter_r  CBW
   
  frontal_def <- c("Precentral", "Frontal_Sup", "Frontal_Sup_Orb",
                   "Frontal_Mid", "Frontal_Mid_Orb", "Frontal_Inf_Oper",
                   "Frontal_Inf_Tri", "Frontal_Inf_Orb", "Rolandic_Oper",
                   "Supp_Motor_Area", "Olfactory", "Frontal_Sup_Medial",
                   "Frontal_Med_Orb", "Rectus", "Paracentral_Lobule",
                   "Paracentral_Lobule")
  
  temporal_def <- c("Hippocampus", "Parahippocampus", "Amygdala", "Fusiform",
                    "Heschl", "Temporal_Sup", "Temporal_Pole_Sup",
                    "Temporal_Mid", "Temporal_Pole_Mid", "Temporal_Inf")
                    
  parietal_def <- c("Postcentral", "Parietal_Sup", "Parietal_Inf",
                    "Supra_Marginal", "Angular", "Precuneus")
  
  insulacingulate_def <- c("Insula", "Cingulum_Ant", "Cingulum_Mid",
                            "Cingulum_Post")
   
  occipital_def <- c("Calcarine", "Cuneus", "Lingual", "Occipital_Sup",
                     "Occipital_Mid", "Occipital_Inf")
  
  central_def <- c("CaudateNucl", "Putamen", "Pallidum", "Thalamus")
  
  cerebellum_def <- c("Cerebellum_Crus1", "Cerebellum_Crus2", "Cerebellum3",
                     "Cerebellum45", "Cerebellum6", "Cerebellum7",
                     "Cerebellum8", "Cerebellum9", "Cerebellum10")
  
  vermis <- c("Vermis12", "Vermis3", "Vermis45", "Vermis6", "Vermis7",
              "Vermis8", "Vermis9", "Vermis10")
  
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
    
  leftinsulacingulate <- paste(insulacingulate_def, "_l", sep="")
  rightinsulacingulate <- paste(insulacingulate_def, "_r", sep="")
  insulacingulate <- c(leftinsulacingulate, rightinsulacingulate)
    
  cerebellum <- c(paste(cerebellum_def, "_l", sep=""),
                  paste(cerebellum_def, "_r", sep=""), vermis)
  
  central <- c(paste(central_def, "_l", sep=""),
               paste(central_def, "_r", sep=""))
  
  #medulla <- c("Medulla")
  #midbrain <- c("Midbrain")
  #pons <- c("Pons")
  wm <- c("White_matter_l", "White_matter_r")
  #CBW <- c("CBW")
  
  totalcortical <- c(frontal, temporal, parietal, occipital, insulacingulate)
    
  ROIs <- list(leftfrontal=leftfrontal, rightfrontal=rightfrontal,
               lefttemporal=lefttemporal, righttemporal=righttemporal,
               leftparietal=leftparietal, rightparietal=rightparietal,
               leftoccipital=leftoccipital, rightoccipital=rightoccipital,
               leftinsulacingulate=leftinsulacingulate,
               rightinsulacingulate=rightinsulacingulate,
    
               frontal=frontal, temporal=temporal, parietal=parietal,
               occipital=occipital, insulacingulate=insulacingulate,
  
               totalcortical=totalcortical,
  
               cerebellum=cerebellum, central=central, wm=wm)
               #medulla=medulla, midbrain=midbrain, pons=pons, CBW=CBW)
  
  return(ROIs)
}


# ROI definitions file.

# Function to generate an object containing lists that define your ROIs
standardROIs <- function() {
	# Define your ROIs here (header names of columns of your TAC/BPnd files)
	# There must be 2 sets of ROIs: one that is specicific to L/R 
	# ("hemilobe" e.g. L frontal) and one that is whole-brain (e.g. frontal)
	# Additional ROIs can be defined, but each subROI only occur once per 
	# set (once in hemilobe and once in lobe)

	#hemilobe (first list)
	leftfrontal <- c("FL_mid_fr_G_l", "FL_precen_G_l", "FL_strai_G_l", 
		"FL_OFC_AOG_l", "FL_inf_fr_G_l", "FL_sup_fr_G_l", 
		"FL_OFC_MOG_l", "FL_OFC_LOG_l", "FL_OFC_POG_l", 
		"Subgen_antCing_l", "Subcall_area_l", "Presubgen_antCing_l")
	rightfrontal <- c("FL_mid_fr_G_r", "FL_precen_G_r", "FL_strai_G_r", 
		"FL_OFC_AOG_r", "FL_inf_fr_G_r", "FL_sup_fr_G_r", 
		"FL_OFC_MOG_r", "FL_OFC_LOG_r", "FL_OFC_POG_r", 
		"Subgen_antCing_r", "Subcall_area_r", "Presubgen_antCing_r")
	lefttemporal <- c("Hippocampus_l", "Amygdala_l", "Ant_TL_med_l", 
		"Ant_TL_inf_lat_l", "G_paraH_amb_l", "G_sup_temp_post_l", 
		"G_tem_midin_l", "G_fus_l", "Post_TL_l", "G_sup_temp_ant_l")
	righttemporal <- c("Hippocampus_r", "Amygdala_r", "Ant_TL_med_r", 
		"Ant_TL_inf_lat_r", "G_paraH_amb_r", "G_sup_temp_post_r", 
		"G_tem_midin_r", "G_fus_r", "Post_TL_r", "G_sup_temp_ant_r")
	leftparietal <- c("PL_postce_G_l", "PL_sup_pa_G_l", "PL_rest_l")
	rightparietal <- c("PL_postce_G_r", "PL_sup_pa_G_r", "PL_rest_r")
	leftoccipital <- c("OL_rest_lat_l", "OL_ling_G_l", "OL_cuneus_l")
	rightoccipital <- c("OL_rest_lat_r", "OL_ling_G_r", "OL_cuneus_r")
	leftcingulate <- c("G_cing_ant_l", "G_cing_post_l")
	rightcingulate <- c("G_cing_ant_r", "G_cing_post_r")
	hemilobeROIs <- list(leftfrontal, rightfrontal, lefttemporal, 
		righttemporal, leftparietal, rightparietal, 
		leftoccipital, rightoccipital, leftcingulate, rightcingulate)
	
	# vector with the names of your ROIs, in the same order as the list 
	# hemilobeROIs
	hemilobenames <- c("leftfrontal", "rightfrontal", "lefttemporal", 
		"righttemporal", "leftparietal", "rightparietal", 
		"leftoccipital", "rightoccipital", "leftcingulate", 
		"rightcingulate")

	#full lobe (second list)
	frontal <- c(leftfrontal, rightfrontal)
	temporal <- c(lefttemporal, righttemporal)
	parietal <- c(leftparietal, rightparietal)
	occipital <- c(leftoccipital, rightoccipital)
	cingulate <- c(leftcingulate, rightcingulate)
	cerebellum <- c("Cerebellum_l", "Cerebellum_r")	
	lobeROIs <- list(frontal, temporal, parietal, occipital, cingulate, 
		cerebellum)
	# a vector with the names of your ROIs, in the same order as the list 
	# lobeROIs
	lobenames <- c("frontal", "temporal", "parietal", "occipital", 
		"cingulate", "cerebellum")

	# totalcortical
	totalcortical <- list(c(frontal, temporal, parietal, occipital, 
		cingulate))

	#allsubROIs
	allsubROIs <- c(frontal, temporal, parietal, occipital, cingulate, 
		cerebellum)

	# create a class ROIset with two slots, one for each list of ROIs, so 
	# that it can be returned as a single object
	ROIset <- setClass("ROIset", 
		slots = c(hemilobe = "list", 
			lobe = "list", 
			totalcortical = "list",
			all = "vector", 
			hemilobenames = "vector", 
			lobenames = "vector"
		)
	)
	
	# an object to be returned containing all of the needed lists and 
	# vectors
	standardROIs <- ROIset()
	slot(standardROIs, "hemilobe") <- hemilobeROIs
	slot(standardROIs, "lobe") <- lobeROIs
	slot(standardROIs, "totalcortical") <- totalcortical
	slot(standardROIs, "all") <- allsubROIs
	slot(standardROIs, "hemilobenames") <- hemilobenames
	slot(standardROIs, "lobenames") <- lobenames

	return(standardROIs)
}



# Define your ROIs here (header names of columns of your TAC/BPnd files)
# There must be 2 sets of ROIs: one that is specicific to L/R 
# ("hemilobe" e.g. L frontal) and one that is whole-brain (e.g. frontal)
# Additional ROIs can be defined, but each subROI only occur once per 
# set (once in hemilobe and once in lobe)

fullROIs <- function() {
	#hemilobe (first list)
	leftfrontal <- c("FL_mid_fr_G_l", "FL_precen_G_l", "FL_strai_G_l", 
		"FL_OFC_AOG_l", "FL_inf_fr_G_l", "FL_sup_fr_G_l", 
		"FL_OFC_MOG_l", "FL_OFC_LOG_l", "FL_OFC_POG_l", 
		"Subgen_antCing_l", "Subcall_area_l", "Presubgen_antCing_l")
	rightfrontal <- c("FL_mid_fr_G_r", "FL_precen_G_r", "FL_strai_G_r", 
		"FL_OFC_AOG_r", "FL_inf_fr_G_r", "FL_sup_fr_G_r", 
		"FL_OFC_MOG_r", "FL_OFC_LOG_r", "FL_OFC_POG_r", 
		"Subgen_antCing_r", "Subcall_area_r", "Presubgen_antCing_r")
	lefttemporal <- c("Hippocampus_l", "Amygdala_l", "Ant_TL_med_l", 
		"Ant_TL_inf_lat_l", "G_paraH_amb_l", "G_sup_temp_post_l", 
		"G_tem_midin_l", "G_fus_l", "Post_TL_l", "G_sup_temp_ant_l")
	righttemporal <- c("Hippocampus_r", "Amygdala_r", "Ant_TL_med_r", 
		"Ant_TL_inf_lat_r", "G_paraH_amb_r", "G_sup_temp_post_r", 
		"G_tem_midin_r", "G_fus_r", "Post_TL_r", "G_sup_temp_ant_r")
	leftparietal <- c("PL_postce_G_l", "PL_sup_pa_G_l", "PL_rest_l")
	rightparietal <- c("PL_postce_G_r", "PL_sup_pa_G_r", "PL_rest_r")
	leftoccipital <- c("OL_rest_lat_l", "OL_ling_G_l", "OL_cuneus_l")
	rightoccipital <- c("OL_rest_lat_r", "OL_ling_G_r", "OL_cuneus_r")
	leftcingulate <- c("G_cing_ant_l", "G_cing_post_l")
	rightcingulate <- c("G_cing_ant_r", "G_cing_post_r")
	leftdeep <- c("CaudateNucl_l", "NuclAccumb_l", "Putamen_l", 
		"Thalamus_l", "Pallidum_l")
	rightdeep <- c("CaudateNucl_r", "NuclAccumb_r", "Putamen_r", 
		"Thalamus_r", "Pallidum_r")
	hemilobeROIs <- list(leftfrontal, rightfrontal, lefttemporal, 
		righttemporal, leftparietal, rightparietal, leftoccipital, 
		rightoccipital, leftcingulate, rightcingulate, leftdeep, 
		rightdeep)
	# a vector with the names of your ROIs, in the same order as the list 
	# hemilobeROIs
	hemilobenames <- c("leftfrontal", "rightfrontal", "lefttemporal", 
		"righttemporal", "leftparietal", "rightparietal", 
		"leftoccipital", "rightoccipital", "leftcingulate", 
		"rightcingulate", "leftdeep", "rightdeep")

	#full lobe (second list)
	frontal <- c(leftfrontal, rightfrontal)
	temporal <- c(lefttemporal, righttemporal)
	parietal <- c(leftparietal, rightparietal)
	occipital <- c(leftoccipital, rightoccipital)
	cingulate <- c(leftcingulate, rightcingulate)
	deep <- c(leftdeep, rightdeep)
	corpus <- c("Corp_Callosum")
	cerebellum <- c("Cerebellum_l", "Cerebellum_r")	
	brainstem <- c("Brainstem")
	ventricles <- c("FrontalHorn_l", "FrontalHorn_r", "TemporaHorn_r", "TemporaHorn_l", "ThirdVentricl") 
	whitematter <- c("White_matter")
	lobeROIs <- list(frontal, temporal, parietal, occipital, cingulate, 
		deep, corpus, cerebellum, brainstem, ventricles, whitematter)
	# a vector with the names of your ROIs, in the same order as the list 
	# lobeROIs
	lobenames <- c("frontal", "temporal", "parietal", "occipital", 
		"cingulate", "deep", "corpus", "cerebellum", "brainstem", "ventricles", "whitematter")

	# totalcortical
	totalcortical <- list(c(frontal, temporal, parietal, occipital, 
		cingulate))

	#allsubROIs
	allsubROIs <- c(frontal, temporal, parietal, occipital, cingulate, deep, 
		corpus, cerebellum, brainstem, ventricles, whitematter)

	# create a class ROIset with two slots, one for each list of ROIs, so 
	# that it can be returned as a single object
	ROIset <- setClass("ROIset", 
		slots = c(hemilobe = "list", 
			lobe = "list", 
			totalcortical = "list",
			all = "vector", 
			hemilobenames = "vector", 
			lobenames = "vector"
		)
	)
	
	# an object to be returned containing all of the needed lists and 
	# vectors
	standardROIs <- ROIset()
	slot(standardROIs, "hemilobe") <- hemilobeROIs
	slot(standardROIs, "lobe") <- lobeROIs
	slot(standardROIs, "totalcortical") <- totalcortical
	slot(standardROIs, "all") <- allsubROIs
	slot(standardROIs, "hemilobenames") <- hemilobenames
	slot(standardROIs, "lobenames") <- lobenames

	return(standardROIs)
}

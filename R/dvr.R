##################################
## tacmagic - PET Analysis in R ##
## dvr.R                        ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' Distribution volume ratio (DVR) for one or more ROIs
#'
#' This calculates the DVR using the non-invasive reference Logan method for
#' all TACs in a supplied tac file. It uses DVR_ref_Logan if a target ROI is 
#' specified, otherwise will calculate DVR for all ROIs with DVR_ref_all_Logan()
#' 
#' For other model paramters, directly call DVR_ref_Logan().
#'
#'@export
#'@param tac The time-activity curve data from load_tac() or tac_roi()
#'@param model Only model currently available is "logan"
#'@param target Optional - otherwise will calculate DVR for all regions
#'@param ref Required -- The reference region, e.g. "cerebellum"
#'@param k2prime Required -- A fixed value for k2' must be specified (e.g. 0.2)
#'@param t_star Required -- If 0, t* will be calculated using find_t_star()
#'@param error For find_t_star()
#'@param method Method of integration, "trapz" or "integrate"
#'@return Data frame with calculated DVRs
#'@family Logan plot functions
#'@references Logan, J., Fowler, J. S., Volkow, N. D., Wang, G.-J., 
#' Ding, Y.-S., & Alexoff, D. L. (1996). Distribution Volume Ratios without 
#' Blood Sampling from Graphical Analysis of PET Data. Journal of Cerebral 
#' Blood Flow & Metabolism, 16(5), 834-840. 
#' https://doi.org/10.1097/00004647-199609000-00008
#'@examples
#' f <- system.file("extdata", "AD06.tac", package="tacmagic")
#' fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' AD06_tac <- load_tac(f, format="PMOD")
#' AD06_volume <- load_vol(fv, format="voistat")
#' AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(),  
#'                 merge=FALSE, PVC=FALSE)  
#' 
#' AD06_DVRs <- dvr(AD06, ref="cerebellum", k2prime=0.2, t_star=23)
#' 
#' AD06_DVR <- dvr(AD06, target="frontal", ref="cerebellum", 
#'              k2prime=0.2, t_star=23)
dvr <- function(tac, model="logan", target=NULL, ref, k2prime, t_star, 
             	error=0.10, method="trapz") {

  if (model != "logan") stop("Only the Logan reference model is available")

  if (is.null(target)) {
  	dvrs <- DVR_all_ref_Logan(tac_data=tac, ref=ref, k2prime=k2prime, 
  		                      t_star=t_star, error=error, method=method)
  } else {
      ref_logan <- DVR_ref_Logan(tac_data=tac, target=target, ref=ref, 
      	                    k2prime=k2prime, t_star=t_star, error=error, 
                            method=method)
      dvrs <- ref_logan$DVR
  }
  return(dvrs)
}

##################################
## tacmagic - PET Analysis in R ##
## logan.R                      ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################


# On testing, produces results equivalent to using Turku PET Centre's
# logan 0.6.17 with the settings -C -mid=y (traditional regression model and 
# mid-frame times)

#' Non-invasive reference Logan method
#'
#' This calculates the coefficient from the non-invasive Logan method, which
#' is equal to DVR. Works for a single tac (target).
#'
#'@export
#'@param tac_data The time-activity curve data from tac_roi()
#'@param target The name of the target ROI, e.g. "frontal"
#'@param ref The reference region, e.g. "cerebellum"
#'@param k2prime A fixed value for k2' must be specified (e.g. 0.2)
#'@param t_star If 0, t* will be calculated using find_t_star()
#'@param error For find_t_star()
#'@param method Method of integration, "trapz" or "integrate"
#'@return Data frame with calculate DVRs for all ROIs
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
#' AD06_DVR_fr <- DVR_ref_Logan(AD06, target="frontal", ref="cerebellum",
#'                              k2prime=0.2, t_star=0) 
#'                             
DVR_ref_Logan <- function(tac_data, target, ref, k2prime, t_star, error=0.10, 
                          method="trapz") {
    logan_model <- ref_Logan_lm(tac_data=tac_data, target=target, ref=ref, 
                                k2prime=k2prime, t_star=t_star, error=error, 
                                method=method)
    tac <- as.data.frame(tac_data)[,c("start", "end", target, ref)]
    attributes(tac) <- copy_tac_attributes(tac_data, tac)
    out <- list(DVR=logan_model$model$coefficients[[2]], 
                model=logan_model$model,
                xy=logan_model$xy,
                t_star=logan_model$t_star,
                tac=tac)
    class(out) <- "ref_Logan"
    return(out)
}


#' Non-invasive reference Logan method for all ROIs in tac data
#'
#' This calculates the DVR using the non-invasive reference Logan method for
#' all TACs in a supplied tac file. It uses DVR_ref_Logan.
#'
#'@export
#'@param tac_data The time-activity curve data from tac_roi()
#'@param ref Required -- The reference region, e.g. "cerebellum"
#'@param k2prime Required -- A fixed value for k2' must be specified (e.g. 0.2)
#'@param t_star Required -- If 0, t* will be calculated using find_t_star()
#'@param error For find_t_star()
#'@param method Method of integration, "trapz" or "integrate"
#'@param params Used by batch_tm (not for calling individually) to pass model
#'              parameters
#'@return Data frame with calculate DVRs for all ROIs
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
#' AD06_DVR <- DVR_all_ref_Logan(AD06, ref="cerebellum", k2prime=0.2, t_star=23)
#' 
DVR_all_ref_Logan <- function(tac_data, 
                              ref=NULL, k2prime=NULL, t_star=NULL, 
                              error=0.10, method="trapz", params=NULL) {
    
  if (!is.null(params)) {
  if(!is.null(c(ref, k2prime, t_star))) {
    stop("Provide EITHER params or ALL of ref, k2prime and t_star")
  } 
  if (any(is.null(params$ref), is.null(params$k2prime), 
      is.null(params$t_star))) {
        stop("Provide ALL of ref, k2prime and t_star")
      }
      ref <- params$ref
      k2prime <- params$k2prime
      t_star <- params$t_star
    }

  DVRtable <- new_table(tac_data, "DVR")
    
  ROIs <- names(tac_data)[3:length(names(tac_data))]
  for (ROI in ROIs) {
    message(paste("Trying", ROI))
    if (any(is.na(tac_data[,ROI]))) {
      attempt <- NA
    } else {
        attempt <- try(DVR_ref_Logan(tac_data, target=ROI, ref=ref,
                                     k2prime=k2prime, t_star=t_star, 
                                     error=error, method=method))
        if (class(attempt) == "try-error") {
          attempt <- NA
        } else attempt <- attempt$DVR 
      }
        
    DVRtable[ROI, "DVR"] <- attempt
  }

  return(DVRtable)
}

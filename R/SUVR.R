##################################
## tacmagic - PET Analysis in R ##
## SUVR.R                       ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' Calculate weighted SUVRs for specified regions of interest
#'
#' Calculate the standardized uptake value ratio (SUVR) for all ROIs in the
#' provided tac data, using the specified reference region.
#'
#'@export 
#'@param tac The time-activity curve data from tac_roi()
#'@param SUVR_def a vector of start times for window to be used in SUVR
#'@param ref a string, e.g. "cerbellum", to specify reference region
#'@param params a list of paramters passed from the batch_tm function and is
#'              not needed when calling for individual participants.
#'@return A data.frame of SUVR values for the specified ROIs
#'@family SUVR functions
#'@examples
#' f <- system.file("extdata", "AD06.tac", package="tacmagic")
#' fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' AD06_tac <- load_tac(f, format="PMOD")
#' AD06_volume <- load_vol(fv, format="voistat")
#' AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(),  
#'                 merge=FALSE, PVC=FALSE)
#' 
#' AD06_SUVR <- suvr(AD06, SUVR_def=c(3000,3300,3600), ref="cerebellum")
#' 
suvr <- function(tac, SUVR_def=NULL, ref=NULL, params=NULL) {

    if (!(is.null(params))) {
      if(!is.null(c(SUVR_def, ref))) {
        stop("Only provide either params argument or both SUVR_def and ref.")
      } 
      if ( is.null(params$SUVR_def) | is.null(params$ref) ) {
        stop("Both SUVR_def and ref are needed to calculate SUVR.")
      }
      
      ref <- params$ref
      SUVR_def <- params$SUVR
    }

    validate_suvr_params(tac, SUVR_def, ref)

    SUVRtable <- new_table(tac, "SUVR")
    
    frames <- match(SUVR_def, tac$start)
    frame_weights <- tac$end[frames] - tac$start[frames]
    
    for (ROI in names(tac)[-(1:2)]) {
        rich <- weighted.mean(tac[frames,][,ROI], frame_weights)
        poor <- weighted.mean(tac[frames,][,ref], frame_weights)
        SUVRtable[ROI, "SUVR"] <-  rich/poor
    }
    return(SUVRtable)
}

#' Calculate SUVRs for regions of interest with AUC from mid-frame times
#'
#' Calculate the standardized uptake value ratio (SUVR) for all ROIs in the
#' provided tac data, using the specified reference region. This is an 
#' alternate to suvr() which should provide very similar values.
#'
#'@export 
#'@param tac The time-activity curve data from tac_roi()
#'@param SUVR_def a vector of start times for window to be used in SUVR
#'@param ref is a string, e.g. "cerbellum", to specify reference region
#'@param params a list of paramters passed from the batch_tm function and is
#'              not needed when calling for individual participants.
#'@family SUVR functions
#'@return A data.frame of SUVR values for the specified ROIs
#' #' f <- system.file("extdata", "AD06.tac", package="tacmagic")
#' fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' AD06_tac <- load_tac(f, format="PMOD")
#' AD06_volume <- load_vol(fv, format="voistat")
#' AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(),  
#'                 merge=FALSE, PVC=FALSE)
#' 
#' AD06_SUVR <- suvr_auc(AD06, SUVR_def=c(3000,3300,3600), ref="cerebellum")
#' 
suvr_auc <- function(tac, SUVR_def=NULL, ref=NULL, params=NULL) {

  if (!(is.null(params))) {
    if(!is.null(c(SUVR_def, ref))) {
      stop("Only provide either params argument or both SUVR_def and ref.")
    } 
    if ( is.null(params$SUVR_def) | is.null(params$ref) ) {
      stop("Both SUVR_def and ref are needed to calculate SUVR.")
    }  
    ref <- params$ref
    SUVR_def <- params$SUVR
  }

  validate_suvr_params(tac, SUVR_def, ref)

  SUVRtable <- new_table(tac, "SUVR")
    
  tac$mid <- (tac$start + tac$end) / 2

  for (ROI in names(tac)[-c(1:2, length(tac))]) {    
    rich <- pracma::trapz(tac[(tac$start %in% SUVR_def),][,"mid"], 
                          tac[(tac$start %in% SUVR_def),][,ROI])
    poor <- pracma::trapz(tac[(tac$start %in% SUVR_def),][,"mid"], 
                          tac[(tac$start %in% SUVR_def),][,ref])
    SUVRtable[ROI, "SUVR"] <-  rich/poor
  }    

    return(SUVRtable)
}


# Checks to ensure SUVR parameters are appropriate and throws error if not.
#' @noRd
validate_suvr_params <- function(tac, SUVR_def, ref) {

  if(!validate_tac(tac)) stop("Supplied tac file did not validate.")

  if (!all(SUVR_def %in% tac$start)) {
    stop("The SUVR definition must refer to valid start times in the tac")
  }

  if(!(ref %in% names(tac))) {
    stop("The reference region (ref) must be in the supplied tac.")
  }  
}

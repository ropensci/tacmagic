##################################
## tacmagic - PET Analysis in R ##
## SUV.R                        ##
## (C) Eric E. Brown  2018-2019 ##
## Beta version--check all work ##
##################################

#' Calculate average SUV over time window, or maximum SUV
#'
#' Calculate the standardized uptake value (SUV) from a tac object, the
#' participant's weight, and the tracer dose. These values may be in the tac
#' object or manually supplied. The weight must be in kg, and the tracer units
#' must be specified. The dose is converted to MBq, the tac is converted to
#' kBq/cc, and the final SUV units are thus in g/cc. Aside from the tac object,
#' the remaining parameters should be left NULL if the required data is in the
#' tac object attributes (as can be done with batch_load()).
#'
#'@export
#'@param tac time-activity curve object (decay-corrected)
#'@param SUV_def vector of start times for window for SUV weighted average, or
#'       alternatively, "max" for the maximum ROI SUV value
#'@param dose the injected tracer dose
#'@param dose_unit unit of tracer dose (e.g. "MBq", "kBq", "mCi"...)
#'@param weight_kg the participant's weight in kg
#'@param ... When called from tm_batch, unused parameters may be supplied
#'@return table of SUV values
#'@family SUV functions
#'@examples
#' f <- system.file("extdata", "AD06.tac", package="tacmagic")
#' fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' AD06_tac <- load_tac(f, format="PMOD")
#' AD06_volume <- load_vol(fv, format="voistat")
#' AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(),
#'                 merge=FALSE, PVC=FALSE)
#' # dose and weight are fabricated for the example
#' AD06_suvmax <- suv(AD06, "max", dose = 9.0, dose_unit = "mCi",
#'                      weight_kg = 70)
#' AD06_suv <- suv(AD06, c(3000, 3300, 3600), dose = 9.0, dose_unit = "mCi",
#'                   weight_kg = 70)
suv <- function(tac, SUV_def, dose=NULL, dose_unit=NULL, weight_kg=NULL, ...) {

  if (!(any(is.null(attributes(tac)$tracer_dose),
           is.null(attributes(tac)$weight_kg),
           is.null(attributes(tac)$dose_unit)))) {

    dose <- attributes(tac)$tracer_dose
    weight_kg <- attributes(tac)$weight_kg
    dose_unit <- attributes(tac)$dose_unit
  }

  validate_suv_params(tac=tac, dose=dose, dose_unit=dose_unit,
                      weight_kg=weight_kg)

  if ( (SUV_def[1] != "max") && (!all(SUV_def %in% tac$start)) ) {
    stop("The SUV definition must be valid tac start times or \"max\"")
  }

  tac <- tac_suv(tac=tac, dose=dose, dose_unit=dose_unit, weight_kg=weight_kg)

  SUVtable <- new_table(tac, "SUV")

  if (SUV_def[1] == "max") {
    for (ROI in names(tac)[-(1:2)]) {
      SUVtable[ROI, "SUV"] <- max(tac[,ROI])
    }
  } else {

    frames <- match(SUV_def, tac$start)
    frame_weights <- tac$end[frames] - tac$start[frames]

    for (ROI in names(tac)[-(1:2)]) {
      SUVtable[ROI, "SUV"] <- weighted.mean(tac[frames,][,ROI], frame_weights)
    }

  }

 return(SUVtable)
}

#' Calculate SUV from TAC
#'
#' Calculate the standardized uptake value (SUV) time-activity curve from a tac
#' object, the participant's weight, and the tracer dose. The weight must be in
#' kg, and the tracer dose must be specified. The dose is converted to MBq, the
#' tac is converted to kBq/cc, and the final SUV units are thus in g/cc. Aside
#' from the tac object, the remaining parameters should be left NULL if the
#' required data is in the tac object attributes (as can be done with
#' batch_load().
#'
#'@export
#'@param tac time-activity curve object (decay-corrected)
#'@param dose the injected tracer dose
#'@param dose_unit unit of tracer dose (e.g. "MBq", "kBq", "mCi"...)
#'@param weight_kg the participant's weight in kg
#'@return tac object with SUV values
#'@family SUV functions
#'@examples
#' f <- system.file("extdata", "AD06.tac", package="tacmagic")
#' fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' AD06_tac <- load_tac(f, format="PMOD")
#' AD06_volume <- load_vol(fv, format="voistat")
#' AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(),
#'                 merge=FALSE, PVC=FALSE)
#' # dose and weight are fabricated for the example
#' AD06_suv <- tac_suv(AD06, dose = 9.0, dose_unit = "mCi", weight_kg = 70)
tac_suv <- function(tac, dose=NULL, dose_unit=NULL, weight_kg=NULL) {

  if (!(any(is.null(attributes(tac)$tracer_dose),
           is.null(attributes(tac)$weight_kg),
           is.null(attributes(tac)$dose_unit)))) {

    dose <- attributes(tac)$tracer_dose
    weight_kg <- attributes(tac)$weight_kg
    dose_unit <- attributes(tac)$dose_unit
  }

  validate_suv_params(tac, dose, dose_unit, weight_kg)

  if (attributes(tac)$activity_unit != "kBq/cc") {
    tac <- change_units(tac, to_unit = "kBq/cc")
  }

  if (dose_unit != "MBq") dose <- change_units(dose, to_unit = "MBq", dose_unit)

  tac[,-(1:2)] <- tac[,-(1:2)] / (dose / weight_kg)

  attributes(tac)$activity_unit <- "g/mL"

  if(!validate_tac(tac)) stop("The resulting tac object did not validate.")

  return(tac)
}

#'@noRd
validate_suv_params <- function(tac, dose, dose_unit, weight_kg) {

  if (!validate_tac(tac)) stop("Supplied tac file did not validate.")
  if (!is.numeric(dose)) stop("Dose must be numeric.")
  if (!(dose_unit %in% names(get_activity_unit_index()))) stop("Bad dose unit.")
  if (!is.numeric(weight_kg)) stop("Weight must be numeric.")

  return(TRUE)
}

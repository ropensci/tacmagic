##################################
## tacmagic - PET Analysis in R ##
## tac.R                        ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################


#' Plots time activity curves from 1 or 2 participants or groups.
#'
#'@export
#'@param x A tac object containing time-activity curves to plot, e.g. from 
#' tac_roi() or load_tac()
#'@param tac2 An optional, second TAC, to plot for comparison
#'@param ROIs A vector of ROIs to plot, names matching the TAC headers
#'@param ymax The maximum value on the y-axis
#'@param time "seconds" or "minutes" depending on desired x-axis, converts tac
#'@param title A title for the plot
#'@param colors If null, rainbow palette is used, otherwise another palette can
#' be specified (heat.colors, terrain.colors, topo.colors, cm.colors
#'@param ... Additional arguments
#'@return Creates a plot
#'@family tac functions 
#'@examples
#' # f_raw_tac and f_raw_vol are the filenames of PMOD-generated files
#' f_raw_tac <- system.file("extdata", "AD06.tac", package="tacmagic") 
#' f_raw_vol <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' 
#' tac <- load_tac(f_raw_tac)
#' vol <- load_vol(f_raw_vol)
#' AD06_tac_nc <- tac_roi(tac, vol, roi_ham_full(), merge=FALSE, PVC=FALSE)
#' plot(AD06_tac_nc, ROIs=c("frontal", "cerebellum"), title="Example Plot")
plot.tac <- function(x, tac2=NULL, ROIs, ymax=25, 
                     time="minutes", title="", colors=NULL, ...) {
  
  tac1 <- x

  if (!all(ROIs %in% names(tac1))) stop("ROIs are not in TACtable1")

  if (!validate_tac(tac1)) stop("The 1st tac object did not validate.")
  if (!is.null(tac2)) {
    if (!validate_tac(tac2)) stop("The 2nd tac object did not validate.")
  }
  
  if (!(time %in% c("seconds", "minutes"))) stop("Time must be \"seconds\" or 
                                                 \"minutes\"")

  if (time == "minutes") {
    if (attributes(tac1)$time_unit == "minutes") time_conversion <- 1
    if (attributes(tac1)$time_unit == "seconds") time_conversion <- 60
  }

  if (time == "seconds") {
    if (attributes(tac1)$time_unit == "seconds") time_conversion <- 1
    if (attributes(tac1)$time_unit == "minutes") time_conversion <- 1/60
  }

  # Sets up the plot using the frame start from the TAC file for the x axis
  # and converting to minutes if chosen. 
   
  plot(1,type='n',
       xlim=c(tac1$start[1],
              tac1$start[length(tac1$start)]/time_conversion),
              ylim=c(0,ymax),
              xlab=paste0("Time (", time, ")"),
              ylab=paste0("Activity (", 
                          attributes(tac1)$activity_unit, ")"),
              main=title)
  
  # Separate colour ranges for each group of TACs
  if (is.null(colors)) {
    colour1 <- rainbow(length(ROIs), start=0, end=0.25)
    colour2 <- rainbow(length(ROIs), start=0.5, end=0.8)	
  } else {
    colour1 <- colors(length(ROIs)*2)[1:length(ROIs)]
    colour2 <- colors(length(ROIs)*2)[length(ROIs)+1:2*length(ROIs)]
  }
  
  
  # Plots the ROIs as specified in the ROIs argument
  for (ROI in seq_along(ROIs)) {

    lines(x=tac1$start/time_conversion, 
          y=tac1[,ROIs[ROI]], 
          type='o', 
          col=colour1[ROI], 
          lwd=2)
    
    # Only if 2nd TAC table is provided, plots second on same plot
    if (is.tac(tac2)) {
      if (!all(ROIs %in% names(tac2))) stop("ROIs are not in tac2")
      compare_tac_form(tac1, tac2)
      lines(x=tac2$start/time_conversion, 
            y=tac2[,ROIs[ROI]], 
            type='o', 
            col=colour2[ROI], 
            lwd=2)
    }
  }
  
  legend("topright", legend=ROIs, col=colour1, pch=1)
  if (is.tac(tac2)) {
    legend("bottomright", legend=ROIs, col=colour2, pch=1)
  }
} 


#' Non-invasive reference Logan plot
#'
#' This plots the non-invasive Logan plot.
#'
#'@export
#'@param tac_data The time-activity curve data from tac_roi()
#'@param target The name of the receptor-rich region, e.g. "frontal"
#'@param ref The reference region, e.g. "cerebellum"
#'@param k2prime A fixed value for k2' must be specified (e.g. 0.2)
#'@param t_star If 0, t* will be calculated using find_t_star()
#'@param error For find_t_star()
#'@param method Method of integration, "trapz" or "integrate"
#'@family Logan plot functions
#'@return No return
#' f <- system.file("extdata", "AD06.tac", package="tacmagic")
#' fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
#' AD06_tac <- load_tac(f, format="PMOD")
#' AD06_volume <- load_vol(fv, format="voistat")
#' AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(),  
#'                 merge=FALSE, PVC=FALSE)  
#' 
#' plot_ref_Logan(AD06, target="frontal", ref="cerebellum", 
#'                k2prime=0.2, t_star=0)
#' 
plot_ref_Logan <- function(tac_data, target, ref, k2prime, t_star=0, error=0.1,
                           method="trapz") {
    model <- ref_Logan_lm(tac_data=tac_data, target=target, ref=ref, 
                          k2prime=k2prime, t_star=t_star, error=error, 
                          method=method)
    xy <- ref_Logan_xy(tac=tac_data, target=target, ref=ref, 
                       k2prime=k2prime, method=method)
    x <- xy$x
    y <- xy$y
    if (t_star == 0) t_star <- find_t_star(x, y, error=error)
    
    par(mfrow=c(1,2))
    
    plot(tac_data, ROIs=c(target,ref))
    
    plot(y~x, main="Logan plot")
    abline(model)
    abline(v=x[t_star])
}


# Utility functions -----------------------------------------------------------

# Used by the plot, or any function that needs 2 tacs, to ensure their overall
# form and attributes are equal (except the ROIs)
compare_tac_form <- function(tac, tac2) {
  if (!all.equal(tac$start, tac2$start)) stop("tac start times not equal")
  if (!all.equal(tac$end, tac2$end)) stop("tac end times not equal")
  a1 <- attributes(tac)
  a2 <- attributes(tac2)
  if (!all.equal(a1$time_unit, a2$time_unit)) stop("tac time units not equal")
  if (!all.equal(a1$activity_unit, a2$activity_unit)) stop("tac start times 
                                                            not equal")
  return(TRUE) 
}

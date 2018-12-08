##################################
## PET Analysis in R            ##
## reference_Logan.R            ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

# See Logan 1996 in references().

# Helper function to use integrate() with sapply(), simply re-arranging the
# arguments of integrate() so that upper is the first argument, and returns
# just the integrate() value.
vintegrate <- function(upper, lower, fn) {
    v <- integrate(fn, lower=lower, upper=upper, stop.on.error=F)
    return(v$value)
}


vAUC <- function(frames,x,y) {
    AUC <- pracma::trapz(x[1:frames], y[1:frames])
    return(AUC)
}

# The non-invasive reference Logan method
reference_Logan_xy <- function(tac_data, target, ref, k2prime) {
    
  mid_time <- (tac$start + tac$end) / 2
  
  # Derive functions for TACs by interpolation.
  target_tac <- approxfun(x=mid_time, y=tac[,target], method = "constant", rule=2)
  ref_tac <- approxfun(x=mid_time, y=tac[,ref], method = "constant", rule=2)
  
  #  yA <- sapply(mid_time, FUN=vintegrate, lower=mid_time[1], f=target_tac)
  #  xA <- sapply(mid_time, FUN=vintegrate, lower=mid_time[1], f=ref_tac) + (tac[,ref] / k2prime)
  
  frames <- 1:length(mid_time)
  yA <- sapply(frames, FUN=vAUC, x=mid_time, y=tac[,target])
  xA <- sapply(frames, FUN=vAUC, x=mid_time, y=tac[,ref])
  
  yB <- tac[,target]
  y <- yA / yB
  xB <- yB
  x <- xA / xB
  output <- data.frame(x,y)

  return(output)
}

# The non-invasive reference Logan method -- linear model starting from t*
reference_Logan_lm <- function(tac_data, target, ref, k2prime, t_star=0) {
    
  xy <- reference_Logan_xy(tac_data, target, ref, k2prime)
  
  x <- xy$x
  y <- xy$y
  
  if (t_star == 0) t_star <- find_t_star(x, y)
  message(paste("t* is", t_star))
  
  model <- lm(y[t_star:length(y)] ~ x[t_star:length(x)])
  return(model)
}

# The coefficient from the non-invasive Logan method, equal to DVR
DVR_reference_Logan <- function(tac_data, target, ref, k2prime, t_star=0) {
    model <- reference_Logan_lm(tac_data, target, ref, k2prime, t_star)
    DVR <- model$coefficients[[2]]
    return(DVR)
}

# The non-invasive reference Logan method for all ROIs in tac_data.
DVR_all_reference_Logan <- function(tac_data, ref, k2prime, t_star=0) {
    
    ROIs <- names(tac_data)[3:length(names(tac_data))]
    
    DVR <- rep(NA, length(ROIs))
    DVRtable <- data.frame(row.names=ROIs, DVR)
    
    for (ROI in ROIs) {
        message(paste("Trying", ROI))
        attempt <- try(DVR_reference_Logan(tac_data, target=ROI, ref, k2prime, t_star))
        if (class(attempt) == "try-error") {
            attempt <- NA
        }
        DVRtable[ROI, "DVR"] <- attempt
    }
    return(DVRtable)
}

# 2 plots to visualize the non-invasive Logan graphical analysis model.
plot_reference_Logan <- function(tac_data, target, ref, k2prime, t_star=0) {
    model <- reference_Logan_lm(tac_data, target, ref, k2prime, t_star)
    xy <- reference_Logan_xy(tac_data, target, ref, k2prime)
    x <- xy$x
    y <- xy$y
    if (t_star == 0) t_star <- find_t_star(x, y)
    
    par(mfrow=c(1,2))
    
    plotTAC2(tac_data, ROIs=c(target,ref))
    
    plot(y~x)
    abline(model)
    abline(v=x[t_star])

}

# The Logan graphical analysis method finds the slope after a point t*; this
# function calculates t* as the earliest time in which all residuals are less
# than the specified proportion (default 0.10 or 10%) of the actual value.
find_t_star <- function(x, y, error=0.10) {
    
    for (i in 1:length(y)) {
        linear_model <- lm(y[i:34]~x[i:34])
        if (all((linear_model$residuals / y[i:34]) < error )) {
            t_star <- i
            break
        }
    }
    if (t_star == 0) {
        stop("No suitable t* found.")
    }
    return(t_star)
}

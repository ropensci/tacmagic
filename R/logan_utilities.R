##################################
## tacmagic - PET Analysis in R ##
## logan.R                      ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

# The non-invasive reference Logan method
#' @noRd
ref_Logan_xy <- function(tac, target, ref, k2prime, method) {
  
  if (!(validate_tac(tac))) stop("Invalid tac object provided.")

  mid_time <- (tac$start + tac$end) / 2
  if (attributes(tac)$time_unit == "seconds") {
    mid_time <- mid_time / 60 # needed because k2' is units 1/min (not seconds)
  }
  
  # Derive functions for TACs by interpolation.
  target_tac <- approxfun(x=mid_time, y=tac[,target], method="linear", rule=2)
  ref_tac <- approxfun(x=mid_time, y=tac[,ref], method = "linear", rule=2)

  if (!is.null(k2prime)) k2r <- (tac[,ref] / k2prime) else k2r <- 0

  if (method == "trapz") {
    frames <- seq_along(mid_time)
    yA <- vapply(frames, FUN=wrap_auc, FUN.VALUE=0, x=mid_time, y=tac[,target])
    xA <- vapply(frames, FUN=wrap_auc, FUN.VALUE=0, x=mid_time, y=tac[,ref]) + 
                         k2r
  } else if (method == "integrate") {
    yA <- vapply(mid_time, FUN=wrap_integrate, FUN.VALUE=0, lower=mid_time[1], 
                 f=target_tac)
    xA <- vapply(mid_time, FUN=wrap_integrate, FUN.VALUE=0, lower=mid_time[1], 
                 f=ref_tac) + k2r
  }

  y <- yA / tac[,target]
  x <- xA / tac[,target]

  output <- data.frame(x,y)

  return(output)
}


# The non-invasive reference Logan method -- linear model starting from t*
#' @noRd
ref_Logan_lm <- function(tac_data, target, ref, k2prime, t_star, error, 
                         method) {
    
  xy <- ref_Logan_xy(tac_data, target=target, ref=ref, k2prime=k2prime,
                     method=method)
  
  x <- xy$x
  y <- xy$y
  
  if (t_star == 0) {
    t_star <- find_t_star(x, y, error=error)
    message(paste("t* is", t_star))
  }

  model <- lm(y[t_star:length(y)] ~ x[t_star:length(x)])

  ref_logan <- list(model=model, xy=xy, t_star=t_star)

  return(ref_logan)
}


# The Logan graphical analysis method finds the slope after a point t*; this
# function calculates t* as the earliest time in which all residuals are less
# than the specified proportion (default 0.1 or 10%) of the actual value.
#' @noRd
find_t_star <- function(x, y, error=0.1) {
    
    frames <- length(y)
    t_star <- 0

    for (i in 1:(frames - 2)) {
        linear_model <- lm(y[i:frames]~x[i:frames])
        if (all(abs(linear_model$residuals / y[i:frames]) < error )) {
            t_star <- i
            break
        }
    }

    if (t_star == 0) stop("No suitable t* found.")
    
    return(t_star)
}


## Helper functions------------------------------------------------------------

# Helper function to use integrate() with vapply(), simply re-arranging the
# arguments of integrate() so that upper is the first argument, and returns
# just the integrate() value.
#' @noRd
wrap_integrate <- function(upper, lower, fn) {
    v <- integrate(fn, lower=lower, upper=upper, stop.on.error=FALSE, 
                   subdivisions=10000L)
    return(v$value)
}

#' @noRd
wrap_auc <- function(frames,x,y) {
    AUC <- pracma::trapz(x[1:frames], y[1:frames])
    return(AUC)
}

## Utilities ------------------------------------------------------------------

is.ref_logan <- function(x) {
		return (class(x) == "ref_Logan") 
}

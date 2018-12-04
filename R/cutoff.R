##################################
## PET Analysis in R            ##
## PIB_pos.R                    ##
## Eric E. Brown                ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

# Based on a method described in:

# Aizenstein HJ, Nebes RD, Saxton JA, et al. 2008.
# Frequent amyloid deposition without significant
# cognitive impairment among the elderly. Arch
# Neurol 65: 1509–1517.

# A group of functions to create a PIB-positive cutoff and filter your SUVR data
# with it.

# This function removes entries greater than 1.5 standard deviations away from
# the means.
removeOutliersStDev <- function(data, stdevs=1.5) {
  loweroutliers <- (data < (mean(data) - stdevs*sd(data)))
  upperoutliers <- (data > (mean(data) + stdevs*sd(data)))
  outliers <- loweroutliers | upperoutliers
  return (data[!outliers])
}

# Remove outliers with upper inner fence
removeOutliersUIF <- function(data) {
  before <- length(data)
  new <- remove(data)
  after <- length(new)
  iterations <- 1
  while ((before - after) > 0) {
    before <- length(new)
    new <- remove(new)
    after <- length(new)
    iterations <- iterations + 1
  }
  cat("Stopped with", iterations, "iterations.\n")
  cat("Outliers removed:", length(data)-length(new), "\n")
  return (new)
}

remove <- function(data) {
  print(data)
  outliers <- data > (quantile(data, 0.75) + (1.5*IQR(data)))
  print(outliers)
  new <- data[!outliers]
  print(new)
  print("----")
  return(new)
}

# This function determines a PIB-Positive cutoff similar to the method described
# by Aizenstein et al. but simplified (check if this is suitable for you)
# The first step is to remove the outliers from a set of control participants'
# total neocortical SUVR. This step uses the removeOutliersStDev function with a
# standard deviation of 1.5. Then, with the cleaned data, it determines and
# returns the value of the upper inner fence, returning a single number as the
# PIB+ cutoff score.
# This could be repeated for multiple ROIs to emulate what was done in 
# Aizenstein et. al
uifCutoff <- function(data) {
  no_outliers <- removeOutliersUIF(data)
  cutoff <- (quantile(no_outliers, 0.75) + (1.5*IQR(data)))
  return(as.numeric(cutoff))
}

sdCutoff <- function(data) {
  no_outliers <- removeOutliersStDev(data, 1.5)
  cutoff <- (quantile(no_outliers, 0.75) + (1.5*IQR(data)))
  return(as.numeric(cutoff))
}

# This function returns a vector of values where 1 = greater than cutoff score
# (PIB+) and 0 is PIB-.
PIB_Positive <- function(corticalSUVR, cutoff) {
  return(as.numeric(corticalSUVR > cutoff))
}

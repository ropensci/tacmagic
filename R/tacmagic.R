#' tacmagic: PET Time Activity Curve Analysis in R
#'
#' This package provides functions to analyze of PET time activity curve (tac) 
#' data, including loading from various fomrmats, merging ROIs weighted for 
#' volume, calculating models including SUVR and DVR, plotting functions and
#' calculation of cutoff values. Please see the vignette for a detailed overview
#' of tacmagic functions.
#'
#' @docType package
#' @name tacmagic
#' @importFrom stats IQR approxfun integrate lm quantile sd weighted.mean
#' @importFrom graphics abline legend lines par plot
#' @importFrom utils read.csv write.csv write.table
#' @importFrom grDevices rainbow
#' @importFrom R.matlab readMat
NULL

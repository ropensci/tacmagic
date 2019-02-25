#' tacmagic: PET Analysis in R
#'
#' The main features of tacmagic are to load PET time activity curve (tac) data 
#' from multiple formats, merge ROIs weighted for volume, calculate binding 
#' potential models including SUVR and DVR, basic plotting, and calculation of 
#' cut-off values. Please see the walkthrough vignette for a detailed overview.
#'
#' @docType package
#' @name tacmagic
#' @importFrom stats IQR approxfun integrate lm quantile sd weighted.mean
#' @importFrom graphics abline legend lines par plot
#' @importFrom utils head read.csv read.delim read.table write.csv write.table
#' @importFrom grDevices rainbow
#' @importFrom R.matlab readMat
#' @importFrom tools file_ext
NULL

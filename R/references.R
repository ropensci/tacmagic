##################################
## PET Analysis in R            ##
## references.R                 ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################


ref_list <- list("Hammers, Alexander, Richard Allom, Matthias J. Koepp, Samantha L. Free, Ralph Myers, Louis Lemieux, Tejal N. Mitchell, David J. Brooks, and John S. Duncan. 2003. \"Three‐dimensional Maximum Probability Atlas of the Human Brain, with Particular Reference to the Temporal Lobe.\" Human Brain Mapping 19 (4): 224-247. doi:10.1002/hbm.10123")


#' Print and return a list of references for this package.
#'
#'@return List of references.
#'@examples references()
references <- function() {
    print(ref_list)
    return(ref_list)
}


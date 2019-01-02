##################################
## PET Analysis in R            ##
## references.R                 ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################


ref_list <- list(

	"Hammers, Alexander, Richard Allom, Matthias J. Koepp, Samantha L. Free, Ralph Myers, Louis Lemieux, Tejal N. Mitchell, David J. Brooks, and John S. Duncan. 2003. \"Three-dimensional Maximum Probability Atlas of the Human Brain, with Particular Reference to the Temporal Lobe.\" Human Brain Mapping 19 (4): 224-247. doi:10.1002/hbm.10123",
    "Logan, J., Fowler, J. S., Volkow, N. D., Wang, G.-J., Ding, Y.-S., & Alexoff, D. L. (1996). Distribution Volume Ratios without Blood Sampling from Graphical Analysis of PET Data. Journal of Cerebral Blood Flow & Metabolism, 16(5), 834-840. https://doi.org/10.1097/00004647-199609000-00008"

     )


#' Print and return a list of references for this package.
#'
#'@export
#'@return List of references.
#'@examples references()
references <- function() {
    print(ref_list)
    return(ref_list)
}


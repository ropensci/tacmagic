##################################
## tacmagic - PET Analysis in R ##
## references.R                 ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

#' Print and return a list of references for this package.
#'
#'@export
#'@return List of references.
#'@examples 
#' references()
references <- function() {

  ref_list <- list(

	Aizenstein_2008=
	"# Aizenstein HJ, Nebes RD, Saxton JA, et al. 2008. Frequent amyloid 
	deposition without significant cognitive impairment among the elderly. 
	Arch Neurol 65: 1509-1517.",

	Hammers_2003=
	"Hammers, Alexander, Richard Allom, Matthias J. Koepp, Samantha L. Free, 
	Ralph Myers, Louis Lemieux, Tejal N. Mitchell, David J. Brooks, and John S. 
	Duncan. 2003. Three-dimensional Maximum Probability Atlas of the Human 
	Brain, with Particular Reference to the Temporal Lobe. Human Brain Mapping 
	19 (4): 224-247. doi:10.1002/hbm.10123",
    
    Klunk_2015="Klunk W.E., Koeppe R.A., Price J.C., Benzinger T.L., 
    Devous M.D., Sr., Jagust W.J., Johnson K.A., Mathis C.A., Minhas D., 
    Pontecorvo M.J., Rowe C.C., Skovronsky D.M., Mintun M.A. The Centiloid 
    Project: standardizing quantitative amyloid plaque estimation by PET. 
    Alzheimers Dement. 2015;11",
    
    Logan_1996="Logan, J., Fowler, J. S., Volkow, N. D., Wang, G.-J., 
    Ding, Y.-S., & Alexoff, D. L. (1996). Distribution Volume Ratios without 
    Blood Sampling from Graphical Analysis of PET Data. Journal of Cerebral 
    Blood Flow & Metabolism, 16(5), 834-840. 
    https://doi.org/10.1097/00004647-199609000-00008",
    
    magia="http://aivo.utu.fi/magia/"
     )

  return(ref_list)
}

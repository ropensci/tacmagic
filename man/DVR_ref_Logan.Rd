% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logan.R
\name{DVR_ref_Logan}
\alias{DVR_ref_Logan}
\title{Non-invasive reference Logan method}
\usage{
DVR_ref_Logan(tac_data, target, ref, k2prime, t_star, error = 0.1,
  method = "trapz")
}
\arguments{
\item{tac_data}{The time-activity curve data from tac_roi()}

\item{target}{The name of the target ROI, e.g. "frontal"}

\item{ref}{The reference region, e.g. "cerebellum"}

\item{k2prime}{A fixed value for k2' must be specified (e.g. 0.2)}

\item{t_star}{If 0, t* will be calculated using find_t_star()}

\item{error}{For find_t_star()}

\item{method}{Method of integration, "trapz" or "integrate"}
}
\value{
Data frame with calculate DVRs for all ROIs
}
\description{
This calculates the coefficient from the non-invasive Logan method, which
is equal to DVR. Works for a single tac (target).
}
\examples{
f <- system.file("extdata", "AD06.tac", package="tacmagic")
fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")
AD06_tac <- load_tac(f, format="PMOD")
AD06_volume <- load_vol(fv, format="voistat")
AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(),  
                merge=FALSE, PVC=FALSE)                             
               
AD06_DVR_fr <- DVR_ref_Logan(AD06, target="frontal", ref="cerebellum",
                             k2prime=0.2, t_star=0) 
                            
}
\references{
Logan, J., Fowler, J. S., Volkow, N. D., Wang, G.-J., 
Ding, Y.-S., & Alexoff, D. L. (1996). Distribution Volume Ratios without 
Blood Sampling from Graphical Analysis of PET Data. Journal of Cerebral 
Blood Flow & Metabolism, 16(5), 834-840. 
https://doi.org/10.1097/00004647-199609000-00008
}
\seealso{
Other Logan plot functions: \code{\link{DVR_all_ref_Logan}},
  \code{\link{dvr}}, \code{\link{plot.ref_Logan}}
}
\concept{Logan plot functions}

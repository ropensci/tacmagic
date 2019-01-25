---
title: 'tacmagic: Positron emission tomography analysis in R'
authors:
- affiliation: "1, 2"
  name: Eric E. Brown
  orcid: 0000-0002-1575-2606
date: "25 January 2019"
bibliography: paper.bib
tags:
- positron emission tomography
- biomedical imaging
- neuroimaging
- neuroscience
- neuroinformatics
affiliations:
- index: 1
  name: Department of Psychiatry and Institute of Medical Science, University of Toronto, Toronto, Canada
- index: 2
  name: Centre for Addiction and Mental Health, Toronto, Canada
---

# Background

Positron emission tomography (PET) is a research and clinical imaging modality that uses radioactive tracers that bind to target molecules of interest. A PET scanner identifies the tracer location by virtue of the tracer's radioactive decay, providing information that can be used to determine the location of the target in the body. Analysis pipelines are used to calculate radiotracer activity over time within a spatial region of interest (ROI). The resulting time-activity curves (TAC) are analyzed to answer important clinical and research questions using kinetic models.[@Dierckx:2014]

# The ``tacmagic`` R package

BBy supporting multiple source data formats, ``tacmagic`` provides an open R [@R] platform for the analysis of PET TAC data that has been produced by existing image analysis pipelines. The data loading functions provide a common format for subsequent analysis in R. We have also implemented basic non-invasive models commonly used in PET research,[@Lopresti:2005; @Logan:1996] comparing the results against existing tools.[@tpcclib] The goal is to facilitate open, explicit and reproducible research.
 
The major features of ``tacmagic`` are documented in a walkthrough vignette that is included with the package. The features include:
 
1. loading TAC and volume data to analyze in R,
2. merging regional TAC data into larger ROIs weighted by volume,
3. basic TAC plotting,
4. calculation of standardized uptake value ratio (SUVR),[@Lopresti:2005;@Dierckx:2014]
5. calculation and plotting of the non-invasive reference region Logan DVR model [@Logan:1996;@tpcclib] and
6. calculation of cut-off values for dichotomizing data.[@Aizenstein:2008]
 
The package is published with an open source licence, enabling future collaboration and expansion of the package's functions, which may include future support for additional data formats, kinetic models, plotting and cut-off calculation.

# Acknowledgments

Many thanks are due to the kind mentorship of Ariel Graff-Guerrero, Philip Gerretsen and Bruce Pollock, as well as to Fernando Caravaggio and Tiffany Chow for their guidance in PET analysis techniques prior to the development of this package. 

Development of parts of this package involved work supported by the Canadian Institute for Health Research Canada Graduate Scholarship, the Ontario Graduate Scholarship, and the Clinician Scientist Program of the University of Toronto's Department of Psychiatry.

# References
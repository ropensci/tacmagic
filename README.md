# tacmagic: PET Analysis in R


[![DOI](https://zenodo.org/badge/131427691.svg)](https://zenodo.org/badge/latestdoi/131427691) [![Build Status](https://travis-ci.org/ropensci/tacmagic.svg?branch=master)](https://travis-ci.org/ropensci/tacmagic) [![Coverage status](https://codecov.io/gh/ropensci/tacmagic/branch/master/graph/badge.svg)](https://codecov.io/github/ropensci/tacmagic?branch=master) [![](https://badges.ropensci.org/280_status.svg)](https://github.com/ropensci/software-review/issues/280)
 [![JOSS](http://joss.theoj.org/papers/10.21105/joss.01281/status.svg)](https://doi.org/10.21105/joss.01281)


To foster openness, replicability, and efficiency, `tacmagic` facilitates loading and analysis of positron emission tomography data in R.

As a `tacmagic` is a new package, we strongly recommend checking all work against existing analyses to confirm the results are as expected, and welcome any feedback.

## Installation

The stable version of the package can be installed from CRAN, and the more recent development version can be installed with the devtools package. 

Use the following R commands to download the version you would like: for the CRAN release,  `install.packages("tacmagic")`, for the github release version that may not yet be available on CRAN, `devtools::install_github("ropensci/tacmagic")`, and for the very latest in-development version that is more likely to have bugs or errors and thus is not suitable for production use, use `devtools::install_github("ropensci/tacmagic", ref="devel")`.

## Features

The features of `tacmagic` are demonstrated in the package's walkthrough vignette, which is highly recommended for first-time uses.

### Data loading and weighted-averages

Time-activity curve (TAC) and/or region of interest (ROI) volume data can be loaded from various file formats including [PMOD](https://www.pmod.com/web/) .tac and .voistat files, a .mat file from the [magia](http://aivo.utu.fi/magia/) pipeline, and [Turku PET Centre's](http://turkupetcentre.fi) .DFT format. 

There is support for converting the radioactivity units in TAC data.

This package is not affiliated with any of the above pipelines.

### Time-activity curve plotting

Basic plotting of one or more TAC from one or more participants is available.

### Binding potential models

Non-invasive models are implemented including the standardized uptake volume (SUV), SUV ratio (SUVR), and the non-invasive Logan reference method.

### Batch and group-wise analysis

Loading and analysis functions can be run as a batch or by individual participant.

## Licence

    Copyright (C) 2018 Eric E. Brown

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

We also note specifically that this package is not intended for clinical use, and may contain bugs or errors, so any results should be verified. As above, we provide no warranty and assume no liability.

## Citation

Please cite this software package if you use it in your analyses. 

`Brown, E. E. (2019). tacmagic: PET Analysis in R. Journal of Open Source Software, 4(34), 1281. doi:10.21105/joss.01281.`

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)

# PET Analysis in R - PEAR

    Copyright (C) 2018 Eric Brown

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

This is a beta version of an R package to analyzed processed positron emission tomography (PET) data.

The main purpose is to load processed PET data, such as TAC, ROI volumes and ROI statistics (e.g. Logan, R1, etc.) in a common format regardless of the software that created them (e.g. PMOD). This then enables subsequent statistical calculations, such as calculating weighted mean TACs for ROIs made of small ROIs, calculating SUVR, group-wise means, cut-off values, etc.

These scripts are in NO WAY affiliated with or endorsed by PMOD Technologies. 

There is no warranty and they are to be considered in beta, and have not been 
thoroughly tested. Use at your own risk.

As far as I can tell, the R functionality that comes with PMOD does not provide
the same tools as these scripts. 

If you are interested in these scripts, or have any questions, suggestions, and
especially if you see any problems, please contact me at eric.brown@utoronto.ca.
I am interested in improving these scripts and making them more useful. If you 
do use them, please acknowledge me in your work.

## Features

The overall purpose of these scripts in their current form is to calculate 
useful measures from PMOD output files, namely, from .tac (time activity curve) 
and .voistat files.

PMOD/PNEURO calcultes TACs and other statistics (e.g. R1) for ROIs. In PNEURO,
ROIs can be merged (e.g. into a frontal lobe ROI), but for flexibility, it may 
be preferred to retain smaller ROIs and merge later in the data processing 
pipeline.

### calculateSUVR.R

Calculate standardized uptake value ratios (SUVR) for regions of interest 
(ROIs). For large ROIs made up of several smaller ROIs, calculates the mean SUVR 
weighted the subROIs by volume.

### batches.R

Calculates SUVR and other statistics for groups of participants.

### fullTAC.R

Calculates time-activity curves for individual ROIs, individual participants, 
groups, and has basic plotting functions for 1 or more TACs.

### ROI_defs.R

Used to create an object that defines the ROIs based on which smallers ROIs 
(subROIs), as found in the .tac files, make up the ROIs that you want to use.


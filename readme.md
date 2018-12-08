# PET Analysis in R - PEAR


This is a beta version of an R package to process and analyze time-activity curve (TAC) data from positron emission tomography (PET) studies.

The package provides loading functions to work with processed PET data, such as TAC, ROI volumes and ROI statistics (e.g. Logan, R1, etc.) in a common format regardless of the software that created them (e.g. PMOD). This then enables subsequent statistical calculations.

There is functionality to combine regions of interest (ROIs) weighted by volume.

Model calculation includes SUVR and non-invasive reference-tissue Logan graphical analysis. 

These scripts are in NO WAY affiliated with or endorsed by PMOD Technologies. 

There is no warranty and they are to be considered in beta, and have not been thoroughly tested. Use at your own risk.

If you are interested in these scripts, or have any questions, suggestions, and especially if you see any problems, please contact me at eric.brown@utoronto.ca. I am interested in improving these scripts and making them more useful. If you 
do use them, please acknowledge/cite this in your work.

## Features

### Data loading and weighted-averages

### SUVR calculation

Calculate standardized uptake value ratios (SUVR) for regions of interest 
(ROIs). For large ROIs made up of several smaller ROIs, calculates the mean SUVR 
weighted the subROIs by volume.

### Logan 

In progress/testing.

### Batch and group-wise analysis

Calculates SUVR and other statistics for groups of participants.

### Time-activity curve calculation and plotting

Calculates time-activity curves for individual ROIs, individual participants, 
groups, and has basic plotting functions for 1 or more TACs.


## Licence

    Copyright (C) 2018 Eric Brown

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

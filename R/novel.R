##################################
## PET Analysis in R            ##
## novel.R                      ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################

# Novel/non-standard functions.

# A function to find the upslope of the TAC, from start to peak.
peakSlope <- function(TAC) {
    SLOPE <- rep(NA, length(names(TAC)))
    SLOPEtable <- data.frame(row.names=names(TAC), SLOPE)
    
    for (i in 3:length(names(TAC))) { #3 to skip start/end time columns
        TAC_maximum <- max(TAC[,i])
        if (!is.na(TAC_maximum)) {
            # get TRUE for each of the TAC values that match the max value
            # (There should be just one, and only the first is needed.)
            TAC_maximum_bool <- TAC[,i] == TAC_maximum
            # returns the value for the end of the time frame of that maximum value
            frame_of_maximum <- TAC[TAC_maximum_bool,]$end
            
            #slope = rise over run = TAC_maximum - TAC_first / frame_of_maximum
            slope <- (TAC_maximum - TAC[1, i]) / frame_of_maximum[1]
            
            # fill in the calculated slope into the holder data.frame
            SLOPEtable[names(TAC)[i], "SLOPE"] <- slope
        }
    }
    
    return(SLOPEtable)
}

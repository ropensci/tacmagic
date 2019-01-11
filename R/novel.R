##################################
## tacmagic - PET Analysis in R ##
## novel.R                      ##
## (C) Eric E. Brown  2018      ##
## Beta version--check all work ##
##################################

# Novel/non-standard functions.

# A function to find the upslope of the TAC, from start to peak.
peakSlope <- function(TAC, SUVR_def=NULL, reference=NULL) {
  
  SLOPEtable <- new_table(TAC, "SLOPE")

  for (i in 3:length(names(TAC))) { #3 to skip start/end time columns
    TAC_maximum <- max(TAC[,i])
    if (!is.na(TAC_maximum)) {
      # get TRUE for each of the TAC values that match the max value
      # (There should be just one, and only the first is needed.)
      TAC_maximum_bool <- TAC[,i] == TAC_maximum
      # returns the value for the end of the time frame of that maximum value
      frame_of_maximum <- ( (TAC[TAC_maximum_bool,]$start + TAC[TAC_maximum_bool,]$end) / 2 )
            
      #slope = rise over run = TAC_maximum - TAC_first / frame_of_maximum
      slope <- (TAC_maximum - TAC[1, i]) / frame_of_maximum[1]
            
      # fill in the calculated slope into the holder data.frame
      SLOPEtable[names(TAC)[i], "SLOPE"] <- slope
    }
  }  
  return(SLOPEtable)
}

# A function to find the upslope of the TAC, from 0 to peak.
peakSlope_from0 <- function(TAC, SUVR_def=NULL, reference=NULL) {

  SLOPEtable <- new_table(TAC, "SLOPE")
    
  for (i in 3:length(names(TAC))) { #3 to skip start/end time columns
    TAC_maximum <- max(TAC[,i])
    if (!is.na(TAC_maximum)) {
      # get TRUE for each of the TAC values that match the max value
      # (There should be just one, and only the first is needed.)
      TAC_maximum_bool <- TAC[,i] == TAC_maximum
      # returns the value for the end of the time frame of that maximum value
      frame_of_maximum <- ( (TAC[TAC_maximum_bool,]$start + TAC[TAC_maximum_bool,]$end) / 2 )
            
      #slope = rise over run = TAC_maximum - TAC_first / frame_of_maximum
      slope <- (TAC_maximum) / frame_of_maximum[1]
            
      # fill in the calculated slope into the holder data.frame
      SLOPEtable[names(TAC)[i], "SLOPE"] <- slope
    }
  }  
  return(SLOPEtable)
}

# Find the maximum value for each tac.
maxTAC <- function(TAC, SUVR_def=NULL, reference=NULL) {

  MAXtable <- new_table(TAC, "MAX")
    
  for (i in 3:length(names(TAC))) { #3 to skip start/end time columns
    TAC_maximum <- max(TAC[,i])
    if (!is.na(TAC_maximum)) {
      MAXtable[names(TAC)[i], "MAX"] <- TAC_maximum
    }
  }
    return(MAXtable)
}
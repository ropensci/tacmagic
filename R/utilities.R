##################################
## PET Analysis in R            ##
## utilities.R                  ##
## (C) Eric E. Brown  2018      ##
## PEAR v devel                 ##
## Beta version--check all work ##
##################################


# Append text to headers in a data.frame
# Useful, for example, before merging 2 data frames that have the same header.
addColumnHeader <- function(data, headersuffix) {
  data <- as.data.frame(data)
  names(data) <- paste(names(data), headersuffix, sep="")
  return(data)
}


# Adds a column to a data frame from another CSV file, toaddfile.
# Adds the columns with the names from the vector varnames.
growDF <- function(master, toaddfile, varnames) {
  addfrom <- read.table(toaddfile, header=TRUE, sep=",")
  masterdf <- as.data.frame(master)
  for (varname in varnames) {
    masterdf <- data.frame(masterdf, addfrom[, varname])
    colnames(masterdf)[ncol(masterdf)] <- varname
  }
  return(masterdf)
}


mean_table <- function(ROI_def) {
  means <- data.frame(
    row.names = ROI_def@all,
    mean = rep(0, length(ROI_def@all)),
    proportion_of_hemilobe = rep(0, length(ROI_def@all)),
    proportion_of_lobe = rep(0, length(ROI_def@all)),
    proprtion_of_total = rep(0, length(ROI_def@all))
  )
  return(means)
}


fill_table <- function(means, storedvalues, ROI_def, proportiontable,
                       headername) {
  for (subROI in ROI_def@all) {
    means[subROI, "mean"] <- storedvalues[subROI, headername]
    means[subROI, "proportion_of_hemilobe"] <- proportiontable[subROI,
                                                       "proportion_of_hemilobe"]
    means[subROI, "proportion_of_lobe"] <- proportiontable[subROI,
                                                           "proportion_of_lobe"]
    means[subROI, "proportion_of_total"] <- proportiontable[subROI,
                                                          "proportion_of_total"]
  }
  return(means)
}

create_final_table <- function(ROI_def, header) {
  final_table <- data.frame(
    row.names = c(names(ROI_def@hemilobe), names(ROI_def@lobe), "totalcortical"),
    data_name = rep(0, length(c(names(ROI_def@hemilobe), names(ROI_def@lobe)))+1))
  names(final_table) <- header
  return(final_table)
}


fill_means_table <- function(single_mean, subROI, means, proportiontable) {
  means[subROI, "mean"] <- single_mean
  means[subROI, "proportion_of_hemilobe"] <- proportiontable[subROI,
                                                       "proportion_of_hemilobe"]
  means[subROI, "proportion_of_lobe"] <- proportiontable[subROI,
                                                         "proportion_of_lobe"]
  means[subROI, "proportion_of_total"] <- proportiontable[subROI,
                                                          "proportion_of_total"]
  return(means)
}



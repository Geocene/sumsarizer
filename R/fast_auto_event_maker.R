#' HAPIN Cooking Event Detector
#' 
#' An algorithm developed as part of the Household Air Pollution Intervention Network Randomized Control Trial.
#' @param data Dataframe or datatable containing at minimum timestamp and value columns
#' @param threshold Numeric temperature above which points are labeled as cooking
#' @param dgap Logical. When true, if the rolling difference in times exceeds the sampling interval in seconds, any points labeled as cooking as relabeled as non-cooking. 
#' @export
hapin_cooking_event_detector  <-  function(data, threshold = 75, dgap = TRUE) {

  if(!'data.table' %in% class(data)){data <- as.data.table(data)}

  if(nrow(data)>0){

    #CALCULATE FEATURES

    #calculate the sample interval
    sample_interval_secs = est_sample_interval(data$timestamp)
    sample_interval_mins = sample_interval_secs/60

    #make a column of 1st derivative (degC/minute)
    data[, difftemps := c(0, diff(value) / sample_interval_mins)]

    #make a column of delta timestamps
    data[, difftimes := c(as.numeric(diff(timestamp), units = "secs"), 0)]

    #cal running SD of 1st derivative of temperature over coming hour
    #or 100 data points, whichever is lower
    if (nrow(data) > 2) {
      data[, sd_difftemps := runsd(difftemps,
                                      min(100,
                                          min(round(60/sample_interval_mins),
                                              nrow(data))),
                                      align = 'right')]
    } else {
      data[, sd_difftemps := NA]
    }

    #look at running mean of 1st derivative of temperature over coming
    #hour or 100 data points, whichever is lower
    if (nrow(data) > 1) {
      data[, mean_difftemps := runmean(data$difftemps,
                                          min(100,
                                              min(round(60/sample_interval_mins),
                                                  nrow(data))),
                                          align = 'right')]
    } else {
      data[, mean_difftemps := NA]
    }

    #look at whether or not most of the data coming up in the next
    #hour is negative slope or 100 data points, whichever is lower
    if (nrow(data) > 1) {
      data[, quantile_difftemps := runquantile(data$difftemps,
                                                  min(100,
                                                      min(round(60/sample_interval_mins),
                                                          nrow(data))),
                                                  probs = 0.8,
                                                  align = 'right')]
    } else {
      data[, quantile_difftemps := NA]
    }

    #RUN THE DECISION TREE

    #just assume there is no cooking to start
    data[ ,event_raw := FALSE]

    #define points that are likely to be cooking
    data[value > threshold, event_raw := TRUE]

    #get rid of long runs of negative slopes
    data[quantile_difftemps < 0, event_raw := FALSE]

    #assume cooking for highly positive slopes
    data[difftemps > 2, event_raw := TRUE]

    #get rid of highly negative slopes
    data[difftemps < -1 * value / 500, event_raw := FALSE]

    #remove places with gaps longer than the sample interval
    if(dgap){data[difftimes > sample_interval_secs, event_raw := FALSE]}

    return(data)
  }
}
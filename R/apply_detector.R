#' Use an event detector to generate labels
#' @param data a sumsarizer formatted data table for one or more sensor missions
#' @param detector a sumsarizer event detector function. see \link{event_detectors}
#' @param ... arguments passed to the detector
#' @family event_detectors
#' @export
#' @importFrom RCurl url.exists
#' @import sl3
apply_detector <- function(data, detector, ...){
  data <- setDT(copy(data))
  data[,label:=as.numeric(detector(.SD,...)),by=list(filename)]
  
  return(data)
}
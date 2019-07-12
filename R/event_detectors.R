#' Detect Cooking Using Firefinder Algorithm
#' 
#' TODO: Danny document this 
#' 
#' @param data a sumsarizer formatted data table for one sensor mission
#' @param primary_threshold the main threshold to determine cooking
#' @param min_event_sec min number of seconds in a real event
#' @param min_break_sec min number of seconds to makeup a real break
#' @param ... not currently used
#' @family event_detectors
#' @export
firefinder_detector = function(data, 
                               primary_threshold = 75, 
                               min_event_sec = 5*60,
                               min_break_sec = 30*60,
                               ...) {
  setDT(data)
  data <- copy(data)
  
  
  max_run_length <- 100
  
    
  #CALCULATE FEATURES
  
  sample_interval <- get_sample_interval(data)
  
  sample_interval_mins <- sample_interval/60
  
  #make a column of 1st derivative (degC/minute)
  data[, difftemps := c(0, diff(value) / sample_interval_mins)]
  
  #make a column of delta timestamps
  data[, difftimes := c(as.numeric(diff(data$timestamp), units = "secs"), 0)]

  #look at whether or not most of the data coming up in the next
  #hour is negative slope or 100 data points, whichever is lower
  if (nrow(data) > 1) {
    data$quantile_difftemps = runquantile(data$difftemps,
                                          min(max_run_length,
                                              min(round(60/sample_interval_mins),
                                                  nrow(data))),
                                          probs = 0.8,
                                          align = 'right')
  } else {
    data$quantile_difftemps = NA
  }
  
  #RUN THE DECISION TREE
  
  #just assume there is no cooking to start
  data$event_raw = FALSE
  
  #define points that are likely to be cooking
  data[value > primary_threshold, event_raw := TRUE]
  
  #get rid of long runs of negative slopes
  data[quantile_difftemps < 0, event_raw := FALSE]
  
  #assume cooking for highly positive slopes
  data[difftemps > 2, event_raw := TRUE]
  
  #get rid of highly negative slopes
  data[difftemps < -1 * value / 500, "event_raw":= FALSE]
  
  #remove places with gaps longer than the sample interval
  data[difftimes > sample_interval, "event_raw":= FALSE]
  
  
  data[,"event_raw":=smooth_events(event_raw, sample_interval, min_event_sec, min_break_sec)]
  
  #remove events with very low cooking temps
  data[,"event_num":=number_events(event_raw)]
  data[,"event_max":=max(value),by=list(event_num)]
  data[event_max<primary_threshold,"event_raw":=FALSE]
  
  #remove events for data that is out of range and is probably an error
  data[!(data$value < 1000 & data$value > -50),"event_raw":=FALSE]
  
  return(data$event_raw)
}

#' Detect Cooking Using Simple Threshold
#' 
#' Detects cooking using a simple threshold. 
#' @param data a sumsarizer formatted data table for one sensor mission
#' @param threshold the main threshold to determine cooking
#' @param direction one of > < >= or <=
#' @param ... not currently used
#' @family event_detectors
#' @export
threshold_detector <- function(data, threshold = 75, direction = ">", ...) {
  setDT(data)
  directions <- c(">","<",">=","<=")
  if(!(direction%in%directions)){
    stop("direction must be one of ",paste(directions,collapse=" "))
  }
  
  op <- get(direction)
  event <- op(data$value, threshold)
  
  return(event)
}


#' Detect Constant Values
#' 
#' Detects a long run of constant values
#' @param data a sumsarizer formatted data table for one sensor mission
#' @param run_length the length of the run in minutes (min 5 samples)
#' @param ... not currently used
#' @family event_detectors
#' @export
constant_detector <- function(data, run_length=2*60*60, ...) {
  setDT(data)
  sample_interval <- get_sample_interval(data)
  
  window_size <- run_length / sample_interval
  window_size <- min(window_size, 5)
  
  value_sd <- runsd(data$value,
                    window_size,
                    align = 'right',
                    endrule='NA')

  #if the data has not changed at all in run_length data points, 
  # the thermocouple is probably broken
  event <- value_sd==0
  
  return(event)
}

#' Use sl3 Machine Learning for event detection
#' 
#' Uses a sl3 machine learner model trained on labels from TRAINSET to detect events. 
#' See TODO to train your own
#' @param data a sumsarizer formatted data table for one sensor mission
#' @param model_obj either a sl3 learner fit object, or a url or path to a .rdata file containing one
#' @param threshold a value between 0 and 1 indicating the sensitivity of the event detector 
#' @param ... not currently used
#' @family event_detectors
#' @export
#' @importFrom RCurl url.exists
#' @import sl3
sl3_model_detector <- function(data, model_obj = NULL, threshold = 0.5){
  setDT(data)
  if(is.null(model_obj)){
    model_obj <- system.file("extdata/serialized_model.rdata", package="sumsarizer")
  }

  if(is.character(model_obj)&&(url.exists(model_obj)|file.exists(model_obj))){
    if(url.exists(model_obj)){
      model_obj <- url(model_obj)
    }
    obj_name <- load(model_obj)
    model_obj <- get(obj_name)
    
  }

  mission_task <- sl3_task_from_data(data)
  mission_preds <- model_obj$predict(mission_task)
  raw_label <- as.numeric(mission_preds>threshold)
  sample_interval <- get_sample_interval(data)
  smooth_label <- sumsarizer:::smooth_events(raw_label, sample_interval)

  return(smooth_label)
  
}

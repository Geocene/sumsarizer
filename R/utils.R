
#' Sumsarizer
#'
#' Ingest, Clean, and Interpret Stove Use Monitoring Data
#' @rdname sumsarizer
#' @name sumsarizer
#' @importFrom lubridate ymd_hms parse_date_time dmy_hms mdy_hms
#' @import data.table
#' @importFrom stats reshape median predict
#' @importFrom utils head read.csv read.delim write.csv globalVariables
#' @import caTools
NULL

#' Convert a temperature in F to C
#' 
#' @param F Temperature in F
#'  
#' @export

f_to_c <- function(F) {
	return((F-32) / 1.8)
}

################################################################################
#' Generic SUMs import functions
#'
#' Uses string parsing to guess which type of sensor
#' is being imported -- currently imports all manner of iButton data,
#' Wellzion SSN-61, Lascar, and EME Systems KSUMs thermocouples. 
#' 
#' @param input_file Path to file being imported
#' @param ... extra arguments to sensor-specific read methods
#' 
#' @return A data.frame with two columns - timestamp (POSIXct) and value (numeric)
#' @import tools
#' @export
import_sums <- function(input_file, ...) {

	#parse headers to guess the type of file
	input_header <- suppressWarnings(read.delim(input_file, stringsAsFactors = F, nrow = 20, header = F))

	#one particular sensor's text encoding is UCS-2LE; if import fails, try that.
	if(nrow(input_header) == 0) {
		input_header <- read.delim(input_file, stringsAsFactors = F, nrow = 5, header = F, fileEncoding = "UCS-2LE") 
	}

	if(nrow(input_header) == 0) {
		message("Input File ", basename(input_file), " is not recognized as a valid SUMs file. Manually import.")
	}else{
		if(any(unlist(lapply(input_header$V1, function(x) { grepl('iButton|### ', x) })))) {
			read_ibutton(input_file, ...)
		} else if(any(unlist(lapply(input_header$V1, function(x) { grepl('kSUM', x) })))) {
			read_ksums(input_file, ...)
		} else if(any(unlist(lapply(input_header$V1, function(x) { grepl('Celsius\\(', x, perl = T) })))) {
			read_lascar(input_file, ...)
		} else if(any(unlist(lapply(input_header$V1, function(x) { grepl('Index,Timestamp,Thermocouple', x) })))){
			read_wellzion(input_file, ...)
		} else if(grepl('filename,timestamp,value,label', input_header$V1[[1]])){
		  import_trainset(input_file, ...)
		} else {
			message("Input File ", basename(input_file), " is not recognized as a valid SUMs file. Manually import.")
		}
	}
}

################################################################################
#' Read labeled files from trainset
#'
#' Imports labeled files exported from trainset
#' and merges with metadata if specified for further use by SUMSarizer
#' 
#' @param input_file Path to file being imported
#' @param metadata Path to file containing metadata for imported files or NA
#' @param ... not currently used
#' @return A data.frame with at least four columns - timestamp (POSIXct), value (numeric), label (logical), filename (character)
#' 
#' @export
import_trainset <- function(input_file, metadata = NA, ...) {

	labeled_file <- read.csv(input_file, stringsAsFactors = F, header = T)
	labeled_file$timestamp <- ymd_hms(labeled_file$timestamp)
	labeled_file$value <- as.numeric(labeled_file$value)

	if(!is.na(metadata)){
		long_metadata <- read.csv(metadata, stringsAsFactors = F, header = T)
		wide_metadata <- reshape(long_metadata, timevar = "variable", idvar = 'filename', direction = 'wide')
		names(wide_metadata) <- gsub("value.", "", names(wide_metadata))
		merge(labeled_file, wide_metadata, by='filename')
	}else{
		labeled_file
	}
}

#' Estimate Sample Interval
#' Estimates a sample interval in seconds as the median interval between adjacent samples
#' @param timestamp a vector of timestamps
#' @export
est_sample_interval <- function(timestamp){
  difftimes <- as.numeric(diff(sort(timestamp)), units = "secs")
  sample_interval <- median(difftimes)
  
  return(sample_interval)
}

#' Get Sample Interval, or estimate if not provided
#' @param data a sumsarizer formatted data table for one sensor mission
#' @export
get_sample_interval <- function(data){
  if(is.null(data$sample_interval)){
    sample_interval <- est_sample_interval(data$timestamp)
  } else {
    sample_interval <- data$sample_interval[1]
  }
  
  return(sample_interval)
}

#' Import SUMs File, Label Events, and Provide Summary Data
#' Imports SUMs files, labels events using a user-specified algorithm, and outputs a list of items including an object with the number of events and total duration of cooking, a table of event start times, stop times, duration, and min and max temperatures; and the raw data with labels. 
#' @param file Path to file being imported
#' @param algorithm Algorithm applied to convert temperature data into cooking
#' @param min_event_sec minmum number of seconds in a real event
#' @param min_break_sec minmum number of seconds in a real non-event
#' @export
import_and_summarize <- function(file, algorithm = 'hapin_cooking_event_detector', min_event_sec = 10*60, min_break_sec = 30 * 60){
	import_data <- as.data.table(import_sums(file))
	sample_interval_sec <- est_sample_interval(import_data$timestamp)
	if(algorithm == 'hapin_cooking_event_detector'){
		import_data <- hapin_cooking_event_detector(test2[value>0 & value<1000])
	}
	import_data[, event_smooth:=smooth_events(event_raw, sample_interval_sec, min_event_sec, min_break_sec)]
	import_data[, event_num:=number_events(event_smooth)]
	return(list(list_of_events = as.data.table(list_events(import_data)), event_summary = summarize_events(import_data$event_num, sample_interval_sec), labeled_data = import_data))
}

#' Import Folder of files and concatenate
#'
#' @param folder Path to folder
#' @param ... extra arguments to sensor-specific read methods
#' @export
import_folder <- function(folder, ...){
  files <- dir(folder, full.names=TRUE)
  all_data <- lapply(files, import_sums, ...)
  data <- rbindlist(all_data)
  
  return(data)
}
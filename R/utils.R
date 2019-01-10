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
#' 
#' @return A data.frame with two columns - timestamp (POSIXct) and value (numeric)
#' 
#' @export

import_sums <- function(input_file, ...) {

	#parse headers to guess the type of file
	input_header <- suppressWarnings(read.delim(input_file, stringsAsFactors = F, nrow = 20, header = F))

	#one particular sensors text encoding is UCS-2LE; if import fails, try that.
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
		} else {
			message("Input File ", basename(input_file), " is not recognized as a valid SUMs file. Manually import.")
		}
	}
}

################################################################################
#' Read labeled files from LABLR
#'
#' Imports labeled files exported from LABLR
#' and merges with metadata if specified for further use by SUMSarizer
#' 
#' @param input_file Path to file being imported
#' @param metadata Path to file containing metadata for imported files or NA
#' @return A data.frame with at least four columns - timestamp (POSIXct), value (numeric), label (logical), filename (character)
#' 
#' @export

import_lablr <- function(input_file, metadata = NA, ...) {

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
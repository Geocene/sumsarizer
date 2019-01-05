#' Read EME Systems kSUMs
#'
#' This function reads in data downloaded from EME Systems kSUMS. 
#' It returns a two column data.frame: timestamp (POSIXct) and value (numeric). 
#' The default timezone is UTC, but can be otherwise specified. See \code{\link{timezones}} for more details.
#'
#' @param input_file Path to kSUMs file
#' @param timezone Timezone. Defaults to UTC.
#' @param lablr_output Output a unique file for each thermocouple lead for use with LABLR.
#' @param wide Specifies the format of the returned data -- either a wide data.frame or a list 
#' 				of data.frames (one for each sensing element).
#' 
#' @return A data.frame with three columns - timestamp (POSIXct) and value (numeric); One for each thermocouple probe.
#' 
#' @seealso \code{\link{timezones}}
#' 
#' @export


read.ksums <- function(input_file, timezone = 'UTC', lablr_output = T, wide = F){
	#files are non-standard and include comments from the data logger
	#select only lines with data
	raw_input_data <- read.delim(input_file)
	data_lines <- as.numeric(sapply(raw_input_data, function(data_lines) grep('[0-9/0-9/0-9]{2,} [0-9:]{6,},[0-9.,]{3,}', data_lines)))
	chr_input_data <- as.character(raw_input_data[data_lines,])

	#write out data lines to a temporary file and read back in
	temp_file <- tempfile()
	write(chr_input_data, file = temp_file)
	input_data <- read.csv(temp_file, stringsAsFactors = F, header = F)
	#remove temporary file
	unlink(temp_file)

	#set column names
	names(input_data) <-  c('timestamp','batt', 'internal_temp', 'tc1', 'tc2', 'tc3')

	#deal with timestamps
	input_data$timestamp <- ymd_hms(input_data$timestamp, tz=timezone)
	input_data$timestamp <- strftime(input_data$timestamp , "%Y-%m-%dT%H:%M:%S%z", tz='timezone')


	if(lablr_output) {
		#reshape2 is much nicer, but using base
		input_data_long <- reshape(input_data, varying = c('tc1', 'tc2', 'tc3'), v.names = 'value', timevar='variable', times=c('tc1', 'tc2', 'tc3'), direction = 'long', new.row.names = NULL)
		#drop reshape cruft
		row.names(input_data_long) <- NULL
		input_data_long$id <- NULL
		#drop unnecessary columns
		input_data_long$batt <- NULL
		input_data_long$internal_temp <- NULL

		#get unique lead ids
		tc_ids <- unique(input_data_long$variable)

		for(i in tc_ids) { 
			output_data <- input_data_long[input_data_long$variable==i,]
			output_data$variable <- NULL
			write.csv(output_data, file = paste(tools::file_path_sans_ext(input_file),".", i, ".lablr.csv", sep=""), row.names = F)
		}
	}

	if(wide) {
		return(input_data)
	}else { 
		return(input_data_long) 
	}
}
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


read_ksums <- function(input_file, timezone = 'UTC', lablr_output = F, wide = F){

	if(file.size(input_file)<100){
		return(NULL)
	}else{

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

		input_data_long <- reshape(input_data, varying = c('tc1', 'tc2', 'tc3'), v.names = 'value', timevar='variable', times=c('tc1', 'tc2', 'tc3'), direction = 'long', new.row.names = NULL)
		row.names(input_data_long) <- NULL

		if(lablr_output) {
			#reshape2 is much nicer, but using base
			#drop reshape cruft
			input_data_long$id <- NULL
			#drop unnecessary columns
			input_data_long$batt <- NULL
			input_data_long$internal_temp <- NULL
			input_data$timestamp <- strftime(input_data$timestamp , "%Y-%m-%dT%H:%M:%S%z", tz='timezone')

			#get unique lead ids
			tc_ids <- unique(input_data_long$variable)

			for(i in tc_ids) { 
				output<- input_data_long[input_data_long$variable==i,]
				output$filename <- paste(tools::file_path_sans_ext(basename(input_file)),'.',i, '.csv', sep="")
				output$variable <- NULL
				output$label <- 0
				output <- output[, c(3,1,2,4)]
				write.csv(output, file = paste(tools::file_path_sans_ext(input_file),".", i, ".trainset.csv", sep=""), row.names = F)
			}
		}

		if(wide) {
			input_data$filename <- basename(input_file)
			return(input_data)
		}else { 
			input_data_long$filename <- basename(input_file)
			return(input_data_long) 
		}
	}
}
#' Read a file from Lascar thermocouples
#'
#' This function reads in data downloaded from Lascar thermocouple dataloggers. 
#' It returns a five column data.frame: timestamp (POSIXct), unit (character), value (numeric), 
#' filename (character), and label. The default timezone is UTC, but can be otherwise specified. 
#' See \code{\link{timezones}} for more details.
#'
#' @param input_file Path to the Lascar Thermocouple file to be read in
#' @param timezone Timezone. Defaults to UTC.
#' @param trainset_output Output a file for use with TRAINSET with four columns: filename, timestamp, value, label
#' 
#' @return A data.frame with five columns - timestamp (POSIXct), unit (character), value (numeric), filename (character), and label.
#' 
#' @seealso \code{\link{timezones}}
#' 
#' @export

read_lascar <- function(input_file, timezone = "UTC", trainset_output = F) {
	if(file.size(input_file)<100){
		return(NULL)
	}else{
		file_import <- read.csv(input_file, skip = 2, header = F)
		file_import$V1 <- NULL
		names(file_import) <-  c('timestamp', 'value')
		file_import$timestamp <- parse_date_time(file_import$timestamp, orders = c("ymd HMS", "dmy HMS", "mdy HMS", "ymd HM", "dmy HM", "mdy HM"), tz = timezone)
		if(trainset_output) {
			output <- file_import
			output$filename  <- tools::file_path_sans_ext(basename(input_file))
			output$label <- 0
			output$timestamp <- strftime(file_import$timestamp , "%Y-%m-%dT%H:%M:%S%z", tz = 'timezone')
			output <- output[, c(3,1,2,4)]
			write.csv(output, file = paste(tools::file_path_sans_ext(input_file), ".trainset.csv", sep=""), row.names = F)
		}
		file_import$filename  <- tools::file_path_sans_ext(basename(input_file))
		return(file_import)
	}
}
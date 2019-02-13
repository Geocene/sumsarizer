#' Read a file from Lascar thermocouples
#'
#' This function reads in data downloaded from Lascar thermocouple dataloggers. 
#' It returns a two column data.frame: timestamp (POSIXct) and value (numeric).
#' The default timezone is UTC, but can be otherwise specified. See \code{\link{timezones}}
#' for more details.
#'
#' @param input_file Path to the Lascar Thermocouple file to be read in
#' @param timezone Timezone. Defaults to UTC.
#' @param lablr_output Output a file for use with LABLR.
#' 
#' @return A data.frame with two columns - timestamp (POSIXct) and value (numeric) 
#' 
#' @seealso \code{\link{timezones}}
#' 
#' @export

read_lascar <- function(input_file, timezone = "UTC", lablr_output = T) {
	if(file.size(input_file)<100){
		return(NULL)
	}else{
		file_import <- read.csv(input_file, skip = 2, header = F)
		file_import$V1 <- NULL
		names(file_import) <-  c('timestamp', 'value')
		file_import$timestamp <- parse_date_time(file_import$timestamp, orders = c("ymd HMS", "dmy HMS", "mdy HMS", "ymd HM", "dmy HM", "mdy HM"), tz = timezone)
		if(lablr_output) {
			output <- file_import
			output$timestamp <- strftime(file_import$timestamp , "%Y-%m-%dT%H:%M:%S%z", tz = 'timezone')
			write.csv(output, file = paste(tools::file_path_sans_ext(input_file), ".lablr.csv", sep=""), row.names = F)
		}
		file_import$filename <- basename(input_file)
		file_import$label <- 'null'
		return(file_import)
	}
}
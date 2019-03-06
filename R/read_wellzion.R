#' Read a Wellzion SSN61 file
#'
#' This function reads in data downloaded from a Wellzion SSN-61. It assumes
#' input files are CSVs exported from Wellzion's Data Logger Graph version 6.5
#' or greater. It returns a five column data.frame: timestamp (POSIXct), unit (character), 
#' value (numeric), filename (character), and label. The default timezone is UTC, 
#' but can be otherwise specified. See \code{\link{timezones}} for more details.
#'
#' @param input_file Path to the Wellzion SSN61 file to be read in
#' @param timezone Timezone. Defaults to UTC.
#' @param trainset_output Output a file for use with TRAINSET with four columns: filename, timestamp, value, label
#' 
#' @return A data.frame with five columns - timestamp (POSIXct), unit (character), value (numeric), filename (character), and label.
#' 
#' @seealso \code{\link{timezones}}
#' 
#' @export

read_wellzion <- function(input_file, timezone = "UTC", trainset_output = F) {

	if(file.size(input_file)<100){
		return(NULL)
	}else{

		file_import <- read.csv(input_file, fileEncoding = "UCS-2LE", skip = 2, header = F, stringsAsFactors = F)
		file_import$V1 <- NULL
		names(file_import) <-  c('timestamp', 'value')

		#use data from the files only to estimate the expected sampling duration
		#does not rely on unreliable header data, etc
		#import 20 rows
		time_test <- head(file_import, 20)

		#calculate whether time is four digits or six digits
		time_length <- unique(unlist(rapply(strsplit(time_test$timestamp, ":"), length, how='list')))
		am_pm <- any(grepl("AM|PM", time_test$timestamp))

		#calculate the unique sampling interval in minutes
		if(time_length==2 & !am_pm){
			time_format <- "ymd_HM"
		}else if(time_length==3 & !am_pm){
			time_format <- "ymd_HMS"
		}else if(time_length==2 & am_pm){
			time_format <- "ymd_I!M p!"
		}else if(time_length==3 & am_pm){
			time_format <- "ymd_I!MS p!"
		}

		time_interval <- unique(
			diff(
				as.numeric(
					parse_date_time(
						paste(Sys.Date(),
							sapply(strsplit(time_test$timestamp, " "), '[[', 2), 
							if(am_pm){sapply(strsplit(time_test$timestamp, " "), '[[', 3)}else{""}), 
						orders=time_format))))/60

		# #calculate the unique sampling interval in minutes
		# if(time_length==2){time_format <- "ymd_HM"}else if(time_length==3){time_format <- "ymd_HMS"}

		# time_interval <- as.numeric(sort(table(diff(as.numeric(parse_date_time(paste(Sys.Date(),(sapply(strsplit(time_test$timestamp, " "), '[[', 2))), orders=time_format)))), decreasing=TRUE)[1])

		nrow_import <- nrow(file_import)
		#calculate the total days of sampling
		est_samp_dur <- (nrow_import * time_interval)/1440

		#get the first and last date of data in the file
		date_ranges <- c(file_import$timestamp[length(file_import$timestamp)], file_import$timestamp[1])

		#transform the date_ranges variable into POSIXct using lubridate
		#calculate differences using numeric time and diff
		#bind into a dataframe
		range_exceedance <- suppressWarnings(rbind(
			data.frame(variable='ymd', value=abs(diff(as.numeric(ymd_hms(date_ranges))))/(60*60*24) - est_samp_dur),
			data.frame(variable='dmy', value=abs(diff(as.numeric(dmy_hms(date_ranges))))/(60*60*24) - est_samp_dur),
			data.frame(variable='mdy', value=abs(diff(as.numeric(mdy_hms(date_ranges))))/(60*60*24) - est_samp_dur)
		))

		#set the date format to the the value closest to the est_sampling_duration & the appropriate number of time digits
		orders <- as.character(range_exceedance[range_exceedance$value==min(range_exceedance$value, na.rm=T) & !is.na(range_exceedance$value), 'variable'])

		if(time_length==2 & !am_pm){
			orders <- paste(orders, "_HM", sep="")
		}else if(time_length==3 & !am_pm){
			orders <- paste(orders, "_HMS", sep="")
		}else if(time_length==2 & am_pm){
			orders <- paste(orders, "_I!M p!", sep="")
		}else if(time_length==3 & am_pm){
			orders <- paste(orders, "_I!MS p!", sep="") 
		}

		file_import$timestamp <- parse_date_time(file_import$timestamp, orders, tz = timezone)

		if(trainset_output) {
			output <- file_import
			output$filename <- basename(input_file)
			output$timestamp <- strftime(file_import$timestamp , "%Y-%m-%dT%H:%M:%S%z", tz = 'timezone')
			output$label <- 0
			output <- output[, c(3,1,2,4)]
			write.csv(output, file = paste(tools::file_path_sans_ext(input_file), ".trainset.csv", sep=""), row.names = F)
		}
		
		file_import$filename <- basename(input_file)
		return(file_import)
	}
}
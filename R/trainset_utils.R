#' Convert a Folder of Raw SUMs Files to TRAINSET Compatible Files
#' @param raw_sums_path the path to a folder of raw sums files
#' @param trainset_path the path to a folder where the trainset compatible files will go
#' @export
raw_sums_to_trainset <- function(raw_sums_path, trainset_path){
  
  raw_sums_files <- dir(raw_sums_path, full.names = TRUE)
  if(!file.exists(trainset_path)){
    dir.create(trainset_path, recursive=TRUE)
  }
  
  file <- raw_sums_files[[1]]
  for(file in raw_sums_files){
    sums_data <- import_sums(file)
    setDT(sums_data)
    filename <- sums_data$filename[[1]]
    out_filename <- sprintf("for_trainset_%s.csv",filename)
    out_path <- file.path(trainset_path, out_filename)
    output <- sums_data[,list(filename=out_filename,
                             timestamp=strftime(timestamp,"%Y-%m-%dT%H:%M:%S%z"),
                             value = value,
                             label = 0)]
    
    write.csv(output, out_path, row.names=FALSE)
  }

}
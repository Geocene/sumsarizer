#' Download Example Sumsarizer Data Used in Vignette
#' 
#' @import aws.s3
#' @export
download_example_data <- function(path="example_data"){
  if(!file.exists(path)){
    dir.create(path, recursive=TRUE)
  }
  files <- get_bucket(bucket = 'example-sumsarizer-data',check_region=FALSE,max=Inf)
  file <- files[[2]]
  for(file in files){
    if(file$Size!=0){
      file_path <- file.path(path, file$Key)
      save_object(file, bucket = 'example-sumsarizer-data', check_region=FALSE,file=file_path)     
    }
  }
}
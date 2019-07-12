globalVariables(c("value", "difftemps", "quantile_difftemps", "difftimes", 
                  "event_num","cooking_danny","event_max")) 


#' @rdname make_features
sumsarizer_feature_names <- c("value", "difftemps", "difftemps_left", "mean_difftemps", 
                              "sd_difftemps", "quantile_difftemps", "rmin", "rmax", "rmean", 
                              "temp_rmin", "temp_rmax", "temp_rmean", "slope", "curvature", 
                              "smoothpred", "temp_sm", "leftsmoothed", "rightsmoothed", "logslope", 
                              "logcurvature", "logsmoothpred", "leftlogsmoothed", "rightlogsmoothed", 
                              "cooking_danny")

globalVariables(c("logsmoothpred", "mean_pcooking", "sd_difftemps",
               "smooth.Pspline", "smoothpred", "timenum", "timestamp"))
#' Make Features to Predict Cooking
#' TODO: DOCUMENT
#' @param timestamps a vector of POSIXct timestamps
#' @param values a vector of cooking temperatures
#' @param sample_interval the sample interval in seconds
#' @importFrom pspline smooth.Pspline
make_features <- function(data, sample_interval=NULL){
  
  # make feature data.table
  features <- copy(setDT(data))
  
  # helper vectors
  
  features[, "difftimes":=c(as.numeric(diff(timestamp), units ="secs"), 0)]
  features[, "timenum":=as.numeric(timestamp)]
  if(is.null(sample_interval)){
    sample_interval <- median(features$difftimes)
    samples_per_hour <- round( (60*60) / sample_interval)
    
  }

  features[, "difftemps":= c(diff(value), 0)/sample_interval]
  features[, "difftemps_left":=c(0,diff(value))/sample_interval]
  
  
  #moving averages
  #hour long windows
  window <- min(samples_per_hour, nrow(features))
  features[, "mean_difftemps":= runmean(difftemps, 
                                       window, 
                                       align = 'right')]
  
  if(window>2){
    features[, "sd_difftemps":= runsd(difftemps, 
                                     window,
                                     align = 'right')]
    features[1, "sd_difftemps":= 0]
  } else {
    features[, "sd_difftemps":= 0]
  }  
  
  features[, "quantile_difftemps":= runquantile(difftemps, 
                                               window, 
                                               probs = 0.8,
                                               align = 'right')]
  
  features[, "rmin":=runmin(features$value,window,align="right")]
  features[, "rmax":=runmax(features$value,window,align="right")]
  features[, "rmean":=runmean(features$value,window,align="center")]
  
  features[,"temp_rmin":=features$value-features$rmin]
  features[,"temp_rmax":=features$value-features$rmax]
  features[,"temp_rmean":=features$value-features$rmean]
  
  
  
  
  #spline smoothing
  spmod <- smooth.Pspline(features$timenum,features$value,method="4")
  
  features[, "slope":=predict(spmod,timenum,nderiv=1)/sample_interval]
  features[, "curvature":=predict(spmod,timenum,nderiv=2)/sample_interval^2]
  features[, "smoothpred":=predict(spmod,timenum,nderiv=0)]
  
  features[,"temp_sm":=value-smoothpred]
  features[, "leftsmoothed":=c(0,diff(smoothpred))/sample_interval]
  features[, "rightsmoothed":=c(diff(smoothpred),0)/sample_interval]
  
  #now on log scale
  mintemp <- min(features$value)-1
  logspmod <- smooth.Pspline(features$timenum,log(features$value-mintemp),method="4")
  
  features[, "logslope":=predict(logspmod,timenum,nderiv=1)/sample_interval]
  features[, "logcurvature":=predict(logspmod,timenum,nderiv=2)/sample_interval^2]
  features[, "logsmoothpred":=predict(logspmod,timenum,nderiv=0)]
  
  features[, "leftlogsmoothed":=c(0,diff(logsmoothpred))/sample_interval]
  features[, "rightlogsmoothed":=c(diff(logsmoothpred),0)/sample_interval]
  
  
  
  # features$ismax=features$rmax==features$value
  # maxes=which(features$ismax)
  # features$lastmaxindex=rep(maxes,diff(c(maxes,nrow(features)+1)))
  # features$lastmax=features$value[features$lastmaxindex]
  # features$slopefrommax=ifelse(features$lastmaxindex==features$index,0,(features$value-features$lastmax)/(features$index-features$lastmaxindex))/sample_interval
  # 
  # #try to fit an exponential to cooling
  # features$expdecay=ifelse(features$value==features$lastmax,0,-(sample_interval/(features$index-features$lastmaxindex))*log((features$value-mintemp)/(features$lastmax-mintemp)))
  # decreasing=features[features$right<=0&features$left<=0,]
  # z=log((decreasing$right/sample_interval+decreasing$value-mintemp)/(decreasing$value-mintemp))
  # lambda=mean(z)
  # 
  # features$exppred=(features$value-features$left/sample_interval-mintemp)*exp(lambda)
  # features$expresid=features$value-features$exppred-mintemp
  # features$lexpresid=features$exppred/(features$value-mintemp)
  # 
  #expsub=which(features$lastmax!=features$value)
  #expmod=nls(value ~ lastmax * exp(-1*lambda*(index-lastmaxindex)),data=features[expsub,],start=list(lambda=1),trace=T)
  #features$expresid=0
  #features$expresid[expsub]=resid(expmod)
  
  # features$ismin=features$rmin==features$value
  # mins=which(features$ismin)
  # features$lastminindex=rep(mins,diff(c(mins,nrow(features)+1)))
  # features$lastmin=features$value[features$lastminindex]
  # features$slopefrommin=ifelse(features$lastminindex==features$index,0,(features$value-features$lastmin)/(features$index-features$lastminindex))/sample_interval
  # features$expgrowth=ifelse(features$value==features$lastmin,0,(1/(features$index-features$lastminindex))*log((features$value-mintemp)/(features$lastmin-mintemp)))
  
  # danny_algo
  features[, "cooking_danny":=firefinder_detector(data)]
  features <- features[,sumsarizer_feature_names, with = FALSE]
  return(features)
}

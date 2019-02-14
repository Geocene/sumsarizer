#' Danny's Magic Cooking Algorithm
#' TODO: Document
#' TODO: use set instead of :=
cooking_danny_algo <- function(one_file, sample_interval){
  one_file <- copy(one_file)
  # implement danny cooking algorithm
  threshold <- 50
  
  one_file[, cooking_danny:=as.numeric(value>threshold)]
  
  #get rid of highly negative slopes
  one_file[difftemps < -1*value/500, cooking_danny := 0]
  
  #get rid of long runs of negative slopes
  one_file[quantile_difftemps < 0, cooking_danny := 0]
  
  #add highly positive slopes
  one_file[difftemps > 5, cooking_danny := 1]
  
  #remove places with gaps longer than the sample interval
  one_file[difftimes > sample_interval, cooking_danny := 0]
  
  #refine the definition of "cooking" into "events"
  max_break_sec <- 30*60 #max number of seconds to makeup a real break
  min_event_sec <- 5*60 #minmum number of seconds in a real event
  min_event_temp <- 50 #minimum temperature in an event to be considered a real event
  rl_obj <- rle(one_file$cooking_danny)
  
  #remove short breaks between cooking
  rl_obj$values[(rl_obj$lengths * sample_interval) < max_break_sec & rl_obj$values == F] = T
  cooking_danny <- inverse.rle(rl_obj)
  
  #remove short cooking events
  rl_obj2 <- rle(cooking_danny)
  rl_obj2$values[(rl_obj2$lengths * sample_interval) < min_event_sec & rl_obj2$values == T] = F
  cooking_danny <- inverse.rle(rl_obj2)
  
  #remove events with very low maximum temperatures
  rl_obj3 <- rle(cooking_danny)
  run_lengths <- rl_obj3$lengths
  one_file[ , event:= rep(seq_along(run_lengths), run_lengths)]
  rl_obj3$max_temp_in_event <- one_file[,list(run_max=max(value)),by=list(event)]$run_max
  rl_obj3$values[rl_obj3$max_temp_in_run < min_event_temp & rl_obj3$lengths > 1 & rl_obj3$values == T] = F
  cooking_danny <- inverse.rle(rl_obj3)
  one_file$event <- NULL
  return(cooking_danny)
  
}

#' Make Features to Predict Cooking
#' TODO: DOCUMENT
#' TODO: use set instead of :=
make_features <- function(one_file){
  one_file <- copy(one_file)
  #stuff we need
  one_file[,index:=1:nrow(one_file)]
  one_file[,timenum:=as.numeric(timestamp)]
  
  one_file[, difftimes := c(as.numeric(diff(timestamp), units = "secs"), 0)]
  sample_interval <- median(one_file$difftimes)
  
  one_file[, difftemps := c(diff(value), 0)/sample_interval]
  one_file[, difftemps_left:=c(0,diff(value))/sample_interval]
  
  
  #moving averages
  #hour long windows
  samples_per_hour <- round(60 / (sample_interval / 60))
  window <- min(samples_per_hour, nrow(one_file))
  
  one_file[, mean_difftemps := runmean(difftemps, 
                                       window, 
                                       align = 'right')]
  
  if(window>2){
    one_file[, sd_difftemps := runsd(difftemps, 
                                     window,
                                     align = 'right')]
    one_file[1, sd_difftemps := 0]
  } else {
    one_file[, sd_difftemps := 0]
  }  
  
  one_file[, quantile_difftemps := runquantile(difftemps, 
                                               window, 
                                               probs = 0.8,
                                               align = 'right')]

  one_file[,rmin:=runmin(one_file$value,window,align="right")]
  one_file[,rmax:=runmax(one_file$value,window,align="right")]
  one_file[,rmean:=runmean(one_file$value,window,align="center")]
  
  one_file[,temp_rmin:=one_file$value-one_file$rmin]
  one_file[,temp_rmax:=one_file$value-one_file$rmax]
  one_file[,temp_rmean:=one_file$value-one_file$rmean]
  
  
  

  #spline smoothing
  spmod <- smooth.Pspline(one_file$timenum,one_file$value,method="4")
  
  one_file[,slope:=predict(spmod,timenum,nderiv=1)/sample_interval]
  one_file[,curvature:=predict(spmod,timenum,nderiv=2)/sample_interval^2]
  one_file[,smoothpred:=predict(spmod,timenum,nderiv=0)]
  
  one_file[,temp_sm:=value-smoothpred]
  one_file[,leftsmoothed:=c(0,diff(smoothpred))/sample_interval]
  one_file[,rightsmoothed:=c(diff(smoothpred),0)/sample_interval]
  
  #now on log scale
  mintemp <- min(one_file$value)-1
  logspmod <- smooth.Pspline(one_file$timenum,log(one_file$value-mintemp),method="4")
  
  one_file[,logslope:=predict(logspmod,timenum,nderiv=1)/sample_interval]
  one_file[,logcurvature:=predict(logspmod,timenum,nderiv=2)/sample_interval^2]
  one_file[,logsmoothpred:=predict(logspmod,timenum,nderiv=0)]
  
  one_file[,leftlogsmoothed:=c(0,diff(logsmoothpred))/sample_interval]
  one_file[,rightlogsmoothed:=c(diff(logsmoothpred),0)/sample_interval]
  
  
  
  # one_file$ismax=one_file$rmax==one_file$value
  # maxes=which(one_file$ismax)
  # one_file$lastmaxindex=rep(maxes,diff(c(maxes,nrow(one_file)+1)))
  # one_file$lastmax=one_file$value[one_file$lastmaxindex]
  # one_file$slopefrommax=ifelse(one_file$lastmaxindex==one_file$index,0,(one_file$value-one_file$lastmax)/(one_file$index-one_file$lastmaxindex))/sample_interval
  # 
  # #try to fit an exponential to cooling
  # one_file$expdecay=ifelse(one_file$value==one_file$lastmax,0,-(sample_interval/(one_file$index-one_file$lastmaxindex))*log((one_file$value-mintemp)/(one_file$lastmax-mintemp)))
  # decreasing=one_file[one_file$right<=0&one_file$left<=0,]
  # z=log((decreasing$right/sample_interval+decreasing$value-mintemp)/(decreasing$value-mintemp))
  # lambda=mean(z)
  # 
  # one_file$exppred=(one_file$value-one_file$left/sample_interval-mintemp)*exp(lambda)
  # one_file$expresid=one_file$value-one_file$exppred-mintemp
  # one_file$lexpresid=one_file$exppred/(one_file$value-mintemp)
  # 
  #expsub=which(one_file$lastmax!=one_file$value)
  #expmod=nls(value ~ lastmax * exp(-1*lambda*(index-lastmaxindex)),data=one_file[expsub,],start=list(lambda=1),trace=T)
  #one_file$expresid=0
  #one_file$expresid[expsub]=resid(expmod)
  
  # one_file$ismin=one_file$rmin==one_file$value
  # mins=which(one_file$ismin)
  # one_file$lastminindex=rep(mins,diff(c(mins,nrow(one_file)+1)))
  # one_file$lastmin=one_file$value[one_file$lastminindex]
  # one_file$slopefrommin=ifelse(one_file$lastminindex==one_file$index,0,(one_file$value-one_file$lastmin)/(one_file$index-one_file$lastminindex))/sample_interval
  # one_file$expgrowth=ifelse(one_file$value==one_file$lastmin,0,(1/(one_file$index-one_file$lastminindex))*log((one_file$value-mintemp)/(one_file$lastmin-mintemp)))
  
  # danny_algo
  one_file[,cooking_danny:=cooking_danny_algo(one_file,sample_interval)]
  
  return(one_file)
}

#' @rdname make_features
FEATURE_NAMES=c("value", "difftemps", "difftemps_left", "mean_difftemps", 
                "sd_difftemps", "quantile_difftemps", "rmin", "rmax", "rmean", 
                "temp_rmin", "temp_rmax", "temp_rmean", "slope", "curvature", 
                "smoothpred", "temp_sm", "leftsmoothed", "rightsmoothed", "logslope", 
                "logcurvature", "logsmoothpred", "leftlogsmoothed", "rightlogsmoothed", 
                "cooking_danny")
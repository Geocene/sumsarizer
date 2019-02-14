# TODO: rethink
#' @importFrom changepoint cpt.mean
segment_probs <- function(pcooking){
  cpt_fit <- cpt.mean(log(pcooking),method="BinSeg",Q=floor(length(pcooking)/2))
  cpt_locs <- cpt_fit
  cutpoints <- unique(c(0,cpt_locs,length(pcooking)))
  cuts <- cut(seq_along(pcooking),cutpoints,labels=FALSE)
  cutdata <- data.table(cut=cuts,pcooking)
  cutdata[,mean_pcooking:=mean(pcooking),by=list(cut)]
  cutdata[,"index":=.I]
  cutdata$mean_pcooking
}

cooking_event_loss <- function(events){
  risk <- with(events,(duration-pduration)^2+ifelse(nevents==0,0,(duration/nevents)^2*(nevents-pnevents)^2))
}

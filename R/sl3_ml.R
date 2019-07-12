#' Construct a sl3 Task From SUMs Data
#' @import sl3
#' @param data a sumsarizer formatted data table for one or more sensor missions
#' @importFrom origami make_folds
#' @export
sl3_task_from_data <- function(data){
  setDT(data)
  if(!is.null(data$filename)){
    mission_features <- data[,make_features(.SD),by=list(filename)]
    folds <- origami::make_folds(cluster_ids=data$filename)
  } else {
    mission_features <- make_features(data)
    folds <- origami::make_folds(nrow(mission_features))
  }
  
  if(!is.null(data$label)){
    mission_features$label <- data$label
    outcome <- "label"
  } else {
    outcome <- NULL
  }
  
  mission_task <- make_sl3_Task(mission_features, 
                                outcome=outcome,
                                covariates=sumsarizer_feature_names,
                                folds=folds)
  
  return(mission_task)
}

#' Learn a sl3 Machine Learning Model from Labels
#' @import sl3
#' @param data a sumsarizer formatted data table for one or more sensor missions
#' @param sl3_learner A sl3 learner, or Lrnr_xgboost if no learner is specified
#' @import sl3
#' @export
learn_labels <- function(data, sl3_learner=NULL){
  if(is.null(sl3_learner)){
    sl3_learner <- make_learner(Lrnr_xgboost)
  }
  
  task <- sl3_task_from_data(data)
  fit <- sl3_learner$train(task)
  
  return(fit)
}

context("test-threshold_detector.R -- Simple Threshold Detector")

library(data.table)
data <- import_lablr(system.file("extdata", "sumsarizer_v1_013408_danny-labeled.csv", package = "sumsarizer"), metadata = NA)
setDT(data)
events <- threshold_detector(data,30)


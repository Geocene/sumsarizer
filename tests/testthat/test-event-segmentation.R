context("test-event-segmentation.R -- Event Identification and Segmentatio")

test_data <- import_lablr(system.file("extdata", "trainset_data.csv", package = "sumsarizer"), metadata = NA)
setDT(test_data)

test_that("Labelled cooking data can be segmented into events", {
  sample_interval <- est_sample_interval(test_data$timestamp)
  test_data[,event_num:=number_events(label)]
  event_summaries <- summarize_events(test_data$event_num, sample_interval)
  expect_equal(event_summaries, list(nevents = 255L, duration = 255174.666666667))
})
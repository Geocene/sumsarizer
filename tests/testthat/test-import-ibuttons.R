context('import_sums_ibutton')

test_that('read_ibutton imports 1921G mdy_hms', {
	test_data <- import_sums(system.file("extdata", "1921G_mmddyy_hhmmss.csv", package = "sumsarizer"), lablr_output = F)
	expect_equal(as.numeric(difftime(test_data[nrow(test_data), 'timestamp'], test_data[1, 'timestamp'], unit = 'mins')), 2230)
	expect_equal(names(test_data), c('timestamp', 'unit', 'value'))
})

test_that('read_ibutton imports 1922E dmy_ims', {
	test_data <- import_sums(system.file("extdata", "1922E_ddmmyy_iimmss.csv", package = "sumsarizer"), lablr_output = F)
	expect_equal(round(as.numeric(difftime(test_data[nrow(test_data), 'timestamp'], test_data[1, 'timestamp'], unit = 'mins')),0), 2555)
	expect_equal(names(test_data), c('timestamp', 'unit', 'value'))
})

test_that('read_ibutton imports 1922L dmy_ims', {
	test_data <- import_sums(system.file("extdata", "1922L_ddmmyy_iimmss.csv", package = "sumsarizer"), lablr_output = F)
	expect_equal(round(as.numeric(difftime(test_data[nrow(test_data), 'timestamp'], test_data[1, 'timestamp'], unit = 'mins')),0), 3010)
	expect_equal(names(test_data), c('timestamp', 'unit', 'value'))
})

test_that('read_ibutton imports 1922T dmy_ims', {
	test_data <- import_sums(system.file("extdata", "1922T_ddmmyy_iimmss.csv", package = "sumsarizer"), lablr_output = F)
	expect_equal(round(as.numeric(difftime(test_data[nrow(test_data), 'timestamp'], test_data[1, 'timestamp'], unit = 'mins')),0), 3690)
	expect_equal(names(test_data), c('timestamp', 'unit', 'value'))
})
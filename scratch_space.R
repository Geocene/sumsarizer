setwd("/Users/dlwilson/GitHub/Geocene/sumsarizer")

install.packages("devtools")
library(devtools)
install_github("geocene/sumsarizer")

usethis::use_vignette()
library(usethis)


use_vignette("how-to", title = "How to Use SUMSarizer")

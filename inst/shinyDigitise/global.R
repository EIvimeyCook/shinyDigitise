#create global variables for storing
utils::globalVariables(c("details", "counter_total", "image_name", "id", "pch", "n"))

#load internal functions
source("./utilities/shinyDigitise_functions.R", local = TRUE)
source("./utilities/metaDigitise_functions.R", local = TRUE)


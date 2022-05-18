library(data.table)
library(naniar)

# set up stata-R interface
library(RStata)
print("Choose where your stata .exe file lies")
ifelse(is.null(options("RStata.StataPath")), chooseStataBin(), "Stata path already chosen")
options("RStata.StataVersion" = 14.2)
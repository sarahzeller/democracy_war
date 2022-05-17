library(data.table)
library(naniar)

# set up stata-R interface
library(RStata)
print("You should choose where your stata .exe file lies")
chooseStataBin()
options("RStata.StataVersion" = 14.2)
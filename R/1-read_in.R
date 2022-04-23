############################################
#Project: Topics in International Economics
#Author: Sarah
#Data: Baliga, S, D.O. Lucaa and T. Sjöström (2011)
#Aim:   Read in data
#last edit: 22/04/14
#############################################

source("set-up.R")
sourceDT <- fread("input/data/bls.data.raw")
names(sourceDT)
str(sourceDT)
saveRDS(sourceDT, "output/sourceDT.rds")


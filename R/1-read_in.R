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

##replicate manipulation from bls.data.raw.do
sourceDT[sourceDT == -99] <- NA

## replicate manipulation from bls.data.do
#drop variables
dataDT <- sourceDT[, .SD, .SDcols = c("ccode1",
                                    "ccode2",
                                    "year",
                                    "cap_1",
                                    "cap_2",
                                    "majpow1",
                                    "majpow2",
                                    "alliance",
                                    "dem1",
                                    "dem2",
                                    "contig",
                                    "distance",
                                    "mzongo",
                                    "mzmid",
                                    "mzmidnm",
                                    "mzjoany",
                                    "polity1",
                                    "polity2")]



#generate unique dyad identifier
dataDT[, dcode := ccode1*1000 + ccode2]

#log of capability ratio
dataDT[, logcapr := log(max(cap_1, cap_2)/min(cap_1, cap_2))]

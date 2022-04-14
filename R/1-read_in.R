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

## replicate manipulation from bls.data.do and bls.data.raw.do
#drop variables
dataDT <- sourceDT[, .SD, .SDcols = c(
                                    "ccode1",
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

#replace NA values with NA
dataDT[dataDT == -99 | 
         dataDT == -88 | 
         dataDT == -77 | 
         dataDT == -66 |
         dataDT == -999] <- NA

columns <- c(
             "ccode1",
             "ccode2",
             "year",
             "cap_1",
             "cap_2",
             "majpow1",
             "majpow2",
             "alliance",
             "contig",
             "distance",
             "mzongo",
             "mzmid",
             "mzmidnm",
             "mzjoany" 
             )
dataDT[, (columns) := lapply(.SD, 
                             function(x) replace(x, which(x==-9), NA)), 
       .SDcols = columns]

#generate unique dyad identifier
dataDT[, dcode := ccode1*1000 + ccode2]

#log of capability ratio
dataDT[, logcapr := log(pmax(cap_1, cap_2)/pmin(cap_1, cap_2))]
dataDT[, `:=`(cap_1 = NULL,
              cap_2 = NULL)]

#log distance
dataDT[, logdist := log(distance)]
dataDT[, distance := NULL]

#simplify contiguous variable
dataDT[, dircont := contig < 6]
dataDT[, contig := NULL]

#simplify major powers
dataDT[, majpow := majpow1 == 1 | majpow2 == 1]
dataDT[, `:=` (majpow1 = NULL,
               majpow2 = NULL)]

#simplify alliance 
dataDT[, allianced := alliance != 4]
dataDT[, alliance := NULL]

#change MIDs: dropping any ongoing or joiner MIDs
dataDT[mzongo == 1 | mzjoany == 1, mzmid := NA]
dataDT[, `:=`(mzjoany = NULL,
              mzongo = NULL)]

#lead mzmid

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
  "polity2"
)]

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
                             function(x)
                               replace(x, which(x == -9), NA)),
       .SDcols = columns]

#generate unique dyad identifier
dataDT[, dcode := ccode1 * 1000 + ccode2]

#log of capability ratio
dataDT[, logcapr := log(pmax(cap_1, cap_2) / pmin(cap_1, cap_2))]
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
#check first that there are no years missing
dataDT[, miss := year != shift(year) + 1, by = .(ccode1, ccode2)]
sum(dataDT$miss, na.rm = T)
dataDT[, miss := NULL]
##yup, definitely some missing
# so add all year-dcode combinations
data_completeDT <- dataDT[CJ(dcode = dcode,
                             year = year,
                             unique = T),
                          on = .(dcode, year)]
#take out all years & combinations which are never used
#TODO make this a bit shorter & prettier
#create a dummy for implied missing observations
data_completeDT[, not_missing := year * !is.na(ccode1)][#find the first and last year of observation for dyad
  , `:=`(first_year = ifelse(length(unique(not_missing)) == 1,
                             0,
                             min(not_missing[not_missing > 0])),
         last_year = max(not_missing)),
  by = dcode]
#keep only those which are within the interval
dataDT <- data_completeDT[year >= first_year &
                            year <= last_year][,  `:=`(first_year = NULL,
                                                       last_year = NULL,
                                                       not_missing = NULL)]
dataDT[, mzmid1 := shift(mzmid), by = dcode]
saveRDS(data_completeDT, "output/data_completeDT.rds")
rm(data_completeDT)

#################################
###regime type dummies
#################################
#check that correlation is really high
cor(dataDT$polity1,
    dataDT$dem1,
    use = "complete.obs")
cor(dataDT$polity2,
    dataDT$dem2,
    use = "complete.obs")

#drop Polity-III scores, replace them with Polity-IV scores
dataDT[, `:=`(
  dem1 = polity1,
  dem2 = polity2,
  polity1 = NULL,
  polity2 = NULL
)]

# dummy variables for each net-democracy level
#baseline dummies
dataDT[, `:=`(
  d7c1n1 = dem1 < -3,
  d7c1n2 = -3 >= dem1 & dem1 <= 3,
  d7c1n3 = dem1 > 3,
  d7c2n1 = dem2 < -3,
  d7c2n2 = -3 >= dem2 & dem1 <= 3,
  d7c2n3 = dem2 > 3,
  #Mansfield and Snyder (2002) dummies
  dbisc1n1 = dem1 < -6,
  dbisc1n2 = dem1 >= -6 & dem1 <= 6,
  dbisc1n3 = dem1 > 6,
  dbisc2n1 = dem1 < -6,
  dbisc2n2 = dem1 >= -6 & dem1 <= 6,
  dbisc2n3 = dem1 > 6
)]

# Mansfield and Snyder (2002) transition dummy: 
# transition from n1 to n2
dataDT[, c(paste0("dbisanoctransc", 1:2), paste0("d7anoctransc", 1:2)) := .(
  dbisc1n2 == 1 & shift(dbisc1n1, n = 5) == 1,
  dbisc2n2 == 1 & shift(dbisc2n1, n = 5) == 1,
  d7c1n2 == 1 & shift(d7c1n1, n = 5) == 1,
  d7c2n2 == 1 & shift(d7c2n1, n = 5) == 1
)]

#at least one country in the dyad has a transition
dataDT[, c("dbisanoctransij", "d7anoctransij") := .(
  dbisanoctransc1 == 1 | dbisanoctransc2 == 1,
  d7anoctransc1 == 1 | d7anoctransc2 == 1
)]

#drop the unneeded dummies
set(dataDT, ,
    c("dbisanoctransc1", "dbisanoctransc2"), 
    value = NULL)

# create dummies for all regime-type dyads
for(c1 in 1:3) {
  for (c2 in 1:3) {
    dataDT[, paste0("d7n", c1, c2) := 
             get(paste0("d7c1n", c1)) + get(paste0("d7c2n", c2))]
    dataDT[get(paste0("d7n", c1, c2)) == 1, 
           paste0("d7n", c1, c2) := 0]
    dataDT[get(paste0("d7n", c1, c2)) == 2, 
           paste0("d7n", c1, c2) := 1]
  }
}

# Symmetric definitions for mixed-type dyads in nondirected data
dataDT[, d7n21s := d7n21 + d7n12]


## Mzmid does not exist after 2000, so delete all observations after
dataDT <- dataDT[year <= 2000]

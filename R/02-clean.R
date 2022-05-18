#read in data
source("set-up.R")
dataDT <- readRDS("output/sourceDT.rds")

## replicate manipulation from bls.data.do and bls.data.raw.do
#drop variables
dataDT <- dataDT[, .SD, .SDcols = c(
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
  "mzjoany"
)
dataDT[, (columns) := lapply(.SD,
                             function(x)
                               replace(x, which(x == -9), NA)),
       .SDcols = columns]

#generate unique dyad identifier
dataDT[, dcode := ccode1 * 1000 + ccode2]

#log of capability ratio
dataDT[, logcapr := log(pmax(cap_1, cap_2) / pmin(cap_1, cap_2))
       ][is.infinite(logcapr), logcapr := NA
       ][, `:=`(cap_1 = NULL,
                cap_2 = NULL)]

#log distance
dataDT[, logdist := log(distance)
       ][, distance := NULL]

#simplify contiguous variable
dataDT[, dircont := contig < 6
       ][, contig := NULL]

#simplify major powers
dataDT[, majpow := majpow1 == 1 | majpow2 == 1
       ][, `:=`(majpow1 = NULL,
              majpow2 = NULL)]

#simplify alliance
dataDT[, allianced := alliance != 4
       ][, alliance := NULL]

#change MIDs: dropping any ongoing or joiner MIDs
dataDT[mzongo == 1 | mzjoany == 1, mzmid := NA
       ][, `:=`(mzjoany = NULL,
              mzongo = NULL)]

#lead mzmid
#check first that there are no years missing
sum(dataDT[, .(miss = year != shift(year) + 1), by = dcode]$miss, 
    na.rm = TRUE)

##yup, definitely some missing
# so add all year-dcode combinations
# check min and max year of observation
min_max <- dataDT[, .(min_year = min(year),
                      max_year = max(year)),
                  by = dcode, ]

data_completeDT <- dataDT[CJ(dcode = dcode,
                             year = year,
                             unique = T),
                          on = .(dcode, year)][
                            min_max, on = "dcode"
                          ]
rm(min_max)
#take out all years & combinations which are never used
#keep only those which are within the interval
dataDT <- data_completeDT[year >= min_year &
                            year <= max_year
                          ][,  `:=`(min_year = NULL,
                                    max_year = NULL)
                          ][, mzmid1 := shift(mzmid), by = dcode]
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
  d7c1n2 = dem1 >= -3 & dem1 <= 3,
  d7c1n3 = dem1 > 3,
  d7c2n1 = dem2 < -3,
  d7c2n2 = dem2 >= -3 & dem2 <= 3,
  d7c2n3 = dem2 > 3,
  #Mansfield and Snyder (2002) dummies
  dbisc1n1 = dem1 < -6,
  dbisc1n2 = dem1 >= -6 & dem1 <= 6,
  dbisc1n3 = dem1 > 6,
  dbisc2n1 = dem2 < -6,
  dbisc2n2 = dem2 >= -6 & dem2 <= 6,
  dbisc2n3 = dem2 > 6
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
for (c1 in 1:3) {
  for (c2 in 1:3) {
    dataDT[, paste0("d7n", c1, c2) :=
             get(paste0("d7c1n", c1)) + get(paste0("d7c2n", c2))]
    # #if only one country matches: it's not really in that dyad
    dataDT[get(paste0("d7n", c1, c2)) == 1,
           paste0("d7n", c1, c2) := 0]
    dataDT[get(paste0("d7n", c1, c2)) == 2,
           paste0("d7n", c1, c2) := 1]
    # and for Mansfield and Snyder (2002)
    dataDT[, paste0("dbisn", c1, c2) :=
             get(paste0("dbisc1n", c1)) + get(paste0("dbisc2n", c2))]
    # #if only one country matches: it's not really in that dyad
    dataDT[get(paste0("dbisn", c1, c2)) == 1,
           paste0("dbisn", c1, c2) := 0]
    dataDT[get(paste0("dbisn", c1, c2)) == 2,
           paste0("dbisn", c1, c2) := 1]
  }
}

# Symmetric definitions for mixed-type dyads in nondirected data
dataDT[, `:=`(
  d7n21s = d7n21 + d7n12,
  d7n32s = d7n32 + d7n23,
  d7n31s = d7n31 + d7n13,
  d7n11s = d7n11,
  d7n22s = d7n22,
  d7n33s = d7n33,
  #Mansfield and Snyder (2002) dummies
  dbisn21s = dbisn21 + dbisn12,
  dbisn32s = dbisn32 + dbisn23,
  dbisn31s = dbisn31 + dbisn13,
  dbisn11s = dbisn11,
  dbisn22s = dbisn22,
  dbisn33s = dbisn33
)]

#drop unused variables
set(x = dataDT,
    j = names(dataDT)[names(dataDT) %like% "d7n..$" |
                        names(dataDT) %like% "dbisn..$" |
                        names(dataDT) %like% "d7c.n.$" |
                        names(dataDT) %like% "d7anoctransc." |
                        names(dataDT) %like% "dbisc.n.$"],
    value = NULL)

#order
setcolorder(dataDT,
            c("dcode", "year"))

## Mzmid does not exist after 2000, so delete all observations after
dataDT <- dataDT[year <= 2000]

# add a variable for duration since last war
# assume NAs are 0 for war spells: otherwise this function won't work
dataDT[, event_no := fifelse(is.na(mzmid) == TRUE, 0, mzmid) - 
         ifelse(is.na(mzmid1) == TRUE, 0, mzmid1), 
       by = dcode
       ][, event_no := cumsum(ifelse(event_no < 0, 0, event_no)), by = dcode
       ][, py := 0:(.N-1), by = .(dcode, event_no)
       ][, event_no := NULL]
# this is coded differently in stata, so I take that
# dataDT[event_no == 0, py := 0]

# also need to add NATURAL CUBIC SPLINES:
# 3 terms with 3 equally-spaced-out knots
# library(splines)

saveRDS(dataDT, "output/dataDT.rds")

# compare with statafull dataset, which already includes splines
statafullDT <- as.data.table(readRDS("output/statafull.rds"))
statafullDT[, year := as.integer(format(year, format = "%Y"))]
nrow(statafullDT) == nrow(dataDT) # not identical row numbers
sum(names(dataDT) %in% names(statafullDT) == F) #all cols included but splines

sum(unique(dataDT$dcode) %in% unique(statafullDT$dcode) == F) # all the same dcodes
sum(unique(statafullDT$dcode) %in% unique(dataDT$dcode) == F)

# check out min and max years per dcode: they're exactly the same
yearsDT <- dataDT[, .(min_year_R = min(year),
                      max_year_R = max(year)),
                  by = dcode]
yearsDT <- merge(yearsDT,
                 statafullDT[, .(min_year_S = min(year),
                                 max_year_S = max(year)),
                             by = dcode],
                 by = "dcode")
yearsDT[, .(diff_min = sum(min_year_S != min_year_R),
            diff_max = sum(max_year_S != max_year_R))]

rm(yearsDT)

# TODO: complete the stata data set, i.e. also enter dyad-year combinations where
# all values are missing. Then check if nrow is the same. in that case: merge.

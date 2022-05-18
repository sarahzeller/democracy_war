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
dataDT[, (columns) := lapply(.SD, function(x) replace(x, which(x == -9), NA)),
       .SDcols = columns
        #generate unique dyad identifier
       ][, dcode := ccode1 * 1000 + ccode2
        #log of capability ratio
       ][, logcapr := log(pmax(cap_1, cap_2) / pmin(cap_1, cap_2))
       ][is.infinite(logcapr), logcapr := NA
       ][, `:=`(cap_1 = NULL,
                cap_2 = NULL)
        #log distance
       ][, logdist := log(distance)
       ][, distance := NULL
        #simplify contiguous variable
       ][, dircont := contig < 6
       ][, contig := NULL
        #simplify major powers
       ][, majpow := majpow1 == 1 | majpow2 == 1
       ][, `:=`(majpow1 = NULL,
              majpow2 = NULL)
        #simplify alliance
       ][, allianced := alliance != 4
       ][, alliance := NULL
        #change MIDs: dropping any ongoing or joiner MIDs
       ][mzongo == 1 | mzjoany == 1, mzmid := NA
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
                          on = .(dcode, year)
                          ][min_max, on = "dcode"
                          ][is.na(ccode1) == TRUE, 
                            missing_year := 1]

#take out all years & combinations which are never used
#keep only those which are within the interval
dataDT <- data_completeDT[year >= min_year & year <= max_year
                          ][, mzmid1 := shift(mzmid, type = "lead"), 
                            by = dcode
                          ][missing_year == 1,
                            mzmid1 := NA
                          ][, `:=`(min_year = NULL,
                                   max_year = NULL,
                                   missing_year = NULL)]
if ("data_completeDT.rds" %in% list.files("output") == F) {
  saveRDS(data_completeDT, "output/data_completeDT.rds")
}
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
          # ensure NAs are excluded
dataDT[, trans_na := is.na(dbisc1n2) | is.na(shift(dbisc1n1, n = 5)) |
         is.na(dbisc1n2) | is.na(shift(dbisc2n1, n = 5)) |
         is.na(d7c1n2) | is.na(shift(d7c1n1, n = 5)) |
         is.na(d7c2n2) | is.na(shift(d7c2n1, n = 5)),
       by = dcode
       ][, c(paste0("dbisanoctransc", 1:2), paste0("d7anoctransc", 1:2)) := .(
           dbisc1n2 == 1 & shift(dbisc1n1, n = 5) == 1,
           dbisc2n2 == 1 & shift(dbisc2n1, n = 5) == 1,
           d7c1n2 == 1   & shift(d7c1n1, n = 5) == 1,
           d7c2n2 == 1   & shift(d7c2n1, n = 5) == 1),
         by = dcode
         # exclude NAs
       ][trans_na == TRUE,
         c(paste0("dbisanoctransc", 1:2), paste0("d7anoctransc", 1:2)) := NA
       ][, trans_na := NULL]

#at least one country in the dyad has a transition
dataDT[, c("dbisanoctransij", "d7anoctransij") := .(
  dbisanoctransc1 == 1 | dbisanoctransc2 == 1,
  d7anoctransc1 == 1 | d7anoctransc2 == 1)
  ][, `:=`("dbisanoctransc1" = NULL,
           "dbisanoctransc2" = NULL)]

# create dummies for all regime-type dyads
for (c1 in 1:3) {
  for (c2 in 1:3) {
    dataDT[, paste0("d7n", c1, c2) :=
             get(paste0("d7c1n", c1)) + get(paste0("d7c2n", c2))
    # #if only one country matches: it's not really in that dyad
          ][get(paste0("d7n", c1, c2)) == 1,
           paste0("d7n", c1, c2) := 0
          ][get(paste0("d7n", c1, c2)) == 2,
           paste0("d7n", c1, c2) := 1
    # and for Mansfield and Snyder (2002)
          ][, paste0("dbisn", c1, c2) :=
             get(paste0("dbisc1n", c1)) + get(paste0("dbisc2n", c2))
    # #if only one country matches: it's not really in that dyad
          ][get(paste0("dbisn", c1, c2)) == 1,
           paste0("dbisn", c1, c2) := 0
          ][get(paste0("dbisn", c1, c2)) == 2,
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
dataDT[, event_no := fifelse(is.na(mzmid) == TRUE, 0, mzmid),
       by = dcode
       ][, event_no := cumsum(event_no),
         by = dcode
       ][, py := 0:(.N-1), by = .(dcode, event_no)
         # show missing values
       ][is.na(mzmid1) , 
         war_missing := 1
       ][war_missing == TRUE,
         py := NA
       ][, `:=`(event_no = NULL,
                war_missing = NULL)]

# also need to add NATURAL CUBIC SPLINES:
# compare with statafull dataset, which includes splines
statafullDT <- as.data.table(readRDS("output/statafull.rds"))

# adjust variable type
logcols <- colnames(dataDT)[which(as.vector(dataDT[, lapply(.SD, class)]) == "logical")]

statafullDT[, year := as.integer(format(year, format = "%Y"))
            ][, (logcols) := lapply(.SD, as.logical),
              .SDcols = logcols]
rm(logcols)

# Complete the stata data set, i.e. enter dyad-year combinations where all 
# values are missing. Then check if nrow is the same.

stata_completeDT <- statafullDT[CJ(dcode = dcode, year = year, unique = T),
                                on = .(dcode, year)
                                ][min_max[, c("dcode", 
                                              "min_year", 
                                              "max_year")], 
                                  on = "dcode"
                                ][year >= min_year & year <= max_year
                                ][,  `:=`(min_year = NULL,
                                          max_year = NULL)
                                ][order(dcode, year)]
rm(min_max, statafullDT)

# check that the data.tables are exactly the same, excluding btscs terms
dataDT <- dataDT[order(dcode, year), -"py"]

all.equal(dataDT, 
          stata_completeDT[, .SD, .SDcols = names(dataDT)],
          check.attributes = FALSE, 
          tolerance = 1e-6)

# add final touches: dummy for years w/o war
stata_completeDT[is.na(dem1) == FALSE & is.na(dem2) == FALSE & 
                   is.na(majpow) == FALSE & is.na(logcapr) == FALSE &
                   is.na(allianced) == FALSE & is.na(py) == FALSE,
                 mmzmid1 := mean(mzmid1),
                 by = year
                 ][, dmmzmid1 := mmzmid1 == 0 | is.na(mmzmid1)]

saveRDS(stata_completeDT, "output/full_data.rds")
rm(dataDT, stata_completeDT, c1, c2, columns)

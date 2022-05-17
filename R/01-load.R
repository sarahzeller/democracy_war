source("set-up.R")
# load data set directly into R
sourceDT <- fread("input/data/bls.data.raw")
names(sourceDT)
str(sourceDT)
saveRDS(sourceDT, "output/sourceDT.rds")

# load data set from stata
stata("stata/01bls.data.raw.do")
stata("stata/02bls.data.do")
stata("stata/03bls.spline.terms.do")

statafull <- readstata13::read.dta13("output/bls.data.full.dta")
saveRDS(statafull, "output/statafull.rds")

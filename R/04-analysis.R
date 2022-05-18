# load data
dataDT <- as.data.table(readRDS("output/full_data.rds"))


# clogit baseline
# TODO: find a close approximation

library(alpaca)
model1 <- feglm(mzmid1 ~ d7n11s + d7n21s + d7n31s + d7n32s + d7n33s + 
                  allianced + majpow + logcapr + py + `_spline1` + `_spline2` + 
                  `_spline3` + factor(year) | dcode, 
                data = dataDT[dmmzmid1 == 0])
summary(model1,
        type = "sandwich")

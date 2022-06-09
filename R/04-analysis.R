# load data
require(data.table)
dataDT <- as.data.table(readRDS("output/full_data.rds"))


# direct comparison with paper

library(alpaca)
model1 <- feglm(mzmid1 ~ d7n11s + d7n21s + d7n31s + d7n32s + d7n33s + 
                  allianced + majpow + logcapr | dcode + year, 
                data = dataDT,
                binomial("logit"))
summary(model1,
        type = "sandwich")
saveRDS(model1, "output/model1.rds")

#############################################
# only LiLi or not: Hypothesis 1
##########################################
model_only_lili <- feglm(mzmid1 ~ d7n22s + allianced + majpow + logcapr| 
                           dcode + year, 
                data = dataDT,
                binomial("logit"))
summary(model_only_lili,
        type = "sandwich")
saveRDS(model1, "output/model_only_lili.rds")
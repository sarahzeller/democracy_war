# load data
require(data.table)
require(dplyr)
dataDT <- as.data.table(readRDS("output/full_data.rds"))

##############################
# direct comparison with paper
###############################

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

##########################################
# check out models' subset
#############################################
# dyads
n_distinct(model1$data$dcode)/n_distinct(dataDT$dcode) # 0.03
# observations
nrow(model1$data)/nrow(dataDT)  # 0.06
# years
n_distinct(model1$data$year)/n_distinct(dataDT$year)
unique(dataDT$year)[!unique(dataDT$year) %in% unique(model1$data$year)]

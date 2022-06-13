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
# bias correction
model1_bc <- biasCorr(model1)
summary(model1_bc)
saveRDS(model1_bc, "output/model1_bc.rds")

# pseudo R2
1 - (model1_bc$deviance / model1_bc$null.deviance)

#############################################
# only LiLi or not: Hypothesis 1
##########################################
model_only_lili <- feglm(mzmid1 ~ d7n22s + allianced + majpow + logcapr| 
                           dcode + year, 
                data = dataDT,
                binomial("logit"))
model_only_lili_bc <- biasCorr(model_only_lili)
summary(model_only_lili_bc)
saveRDS(model_only_lili_bc, "output/model_only_lili_bc.rds")

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

###########################################
# only consider specific periods
#########################################

# before WWI
before_wwi <- feglm(mzmid1 ~ d7n22s + allianced + majpow + logcapr | dcode + year, 
                     data = dataDT[year < 1915],
                     binomial("logit"))
before_wwi_bc <- biasCorr(before_wwi)
summary(before_wwi_bc)
saveRDS(before_wwi_bc, "output/before_wwi_bc.rds")

# before WWII
before_wwii <- feglm(mzmid1 ~ d7n22s + allianced + majpow + logcapr | dcode + year, 
                data = dataDT[year < 1946],
                binomial("logit"))
before_wwii_bc <- biasCorr(before_wwii)
summary(before_wwii_bc)
saveRDS(before_wwii_bc, "output/before_wwii_bc.rds")

# between WWI and WWII
between_ww <- feglm(mzmid1 ~ d7n22s + allianced + majpow + logcapr | dcode + year, 
                     data = dataDT[year < 1946 & year > 1913],
                     binomial("logit"))
between_ww_bc <- biasCorr(between_ww)
summary(between_ww_bc)
saveRDS(between_ww_bc, "output/between_ww_bc.rds")


# cold war
cold_war <- feglm(mzmid1 ~ d7n22s + allianced + majpow + logcapr | dcode + year, 
                  data = dataDT[year > 1945],
                  binomial("logit"))
cold_war_bc <- biasCorr(cold_war)
summary(cold_war_bc)
saveRDS(cold_war_bc, "output/cold_war_bc.rds")

# post 1985
post_1985 <- feglm(mzmid1 ~ d7n22s + allianced + majpow + logcapr | dcode + year, 
                  data = dataDT[year > 1984],
                  binomial("logit"))
post_1985_bc <- biasCorr(post_1985)
summary(post_1985_bc)
saveRDS(post_1985_bc, "output/post_1985_bc.rds")

# post cold war
new_era <- feglm(mzmid1 ~ d7n22s + allianced + majpow + logcapr | dcode + year, 
                 data = dataDT[year > 1989],
                 binomial("logit"))
new_era_bc <- biasCorr(new_era)
summary(new_era_bc)
saveRDS(new_era_bc, "output/new_era_bc.rds")


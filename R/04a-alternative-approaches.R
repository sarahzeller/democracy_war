library(alpaca)

# check Proportional Hazard (PH) along Metzger and Jones (2021):
# interact covariates with splines, check if they're significant
# TODO: find a method in which LR works for alpaca result

model1_a <- feglm(mzmid1 ~ d7n11s + d7n21s + d7n31s + d7n32s + d7n33s + 
                    allianced + majpow + logcapr + py + `_spline1` + `_spline2` + 
                    `_spline3` + allianced * `_spline1` + 
                    allianced * `_spline2` + allianced * `_spline3` +
                    allianced * py| dcode + year, 
                  data = dataDT[dmmzmid1 == 0])

library(lmtest)
lrtest(model1, model1_a)


# check out the model if DeDe is excluded: everything else should be less peaceful,
# right? so all coefficients should be positive
model_dede <- feglm(mzmid1 ~ d7n11s + d7n21s + d7n31s + d7n32s + d7n22s + 
                      allianced + majpow + logcapr + py + `_spline1` + `_spline2` + 
                      `_spline3` | dcode + year, 
                    data = dataDT[dmmzmid1 == 0])
summary(model_dede,
        type = "sandwich")
saveRDS(model1, "output/model_dede.rds")
# interesting: this matches exactly. Also, LiLi increase the probability the 
# most, so matches hypothesis. 


#########################################################
# follow Signorio: check out splines and interpret them
##########################################################
# find spline terms for each relative time t bzw. py
splines <- dataDT[, .(spline1 = unique(`_spline1`),
                      spline2 = unique(`_spline2`),
                      spline3 = unique(`_spline3`)), by = "py"]

# calculate effect for each relative time, setting binary v. to 0 and logcapr
# to mean
mean_logcapr <- mean(model1$data$logcapr)
# TODO: figure out how to link these to produce output
# maybe use getAPEs for that
splines[, prob := mean_logcapr * model1$coefficients["logcapr"] + 
          model1$coefficients["`_spline1`"] * spline1 +
          model1$coefficients["`_spline2`"] * spline2 + 
          model1$coefficients["`_spline3`"] * spline3 + 
          model1$coefficients["py"] * py]

#####################################################
# Cox-PH model
####################################################
library(survival)
model_cox <- coxph(Surv(year, mzmid1) ~ d7n11s + d7n21s + d7n31s + d7n32s + d7n33s + 
                     allianced + majpow + logcapr + py + `_spline1` + `_spline2` + 
                     `_spline3`,
                   data = dataDT[dmmzmid1 == 0])

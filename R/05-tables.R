#################################
# SUMMARY TABLE
###################################
library(data.table)
# create summary table for data used in baseline
model1_bc <- readRDS("output/model1_bc.rds")
model_only_lili <- readRDS("output/model_only_lili_bc.rds")
# need to merge d7n22 in as well so we have data on LiLi dyads
used_dataDT <- model1$data

fulldataDT <- as.data.table(readRDS("output/full_data.rds"))
fulldataDT <- fulldataDT[, c("dcode",
                             "year",
                             "dem1",
                             "dem2",
                             "d7n22s")
                         ][ , `:=`(dcode = as.factor(dcode),
                                   year = as.factor(year))]

used_dataDT <- merge(used_dataDT,
                     fulldataDT,
                     by = c("dcode", "year"),
                     all.x = TRUE)
rm(fulldataDT)
saveRDS(used_dataDT, "output/used_dataDT.rds")

# eliminate variables which aren't interesting in Table 1
used_dataDT <- used_dataDT[, c("mzmid1",
                               # "d7n11s",
                               # "d7n21s",
                               # "d7n31s",
                               # "d7n32s",
                               # "d7n33s",
                               "d7n22s",
                               "allianced",
                               "majpow",
                               "logcapr")]


names(used_dataDT) <- c("MID onset",
                 # "D_{DiDi}",
                 # "D_{LiDi}",
                 # "D_{DeDi}",
                 # "D_{DeLi}",
                 # "D_{DeDe}",
                 "D_{LiLi}",
                 "Allianced",
                 "MajPow",
                 "LogCapRatio")

library(stargazer)
library(tidyverse)

table_one <- stargazer(data = used_dataDT,
                       omit.summary.stat = "n",
                       title = "Sample description for the baseline model",
                       notes = "\\parbox[t]{10cm}{Summary measures for the
                       variables with all included observations ($n$ = 40,786).}",
                       notes.append = TRUE,
                       notes.align = "l",
                       label = "tab:summary")
table_one <- table_one %>%
  str_replace("Statistic", "Variable") %>%
  str_replace("D\\\\_", "D_") %>%
  str_replace("\\\\\\{", "{") %>%
  str_replace("\\\\\\}", "}")

writeLines(table_one, "tables/table_one.tex")

######################################
# direct comparison with paper
#####################################

# create table for model1
sum_model1 <- data.frame(variable = names(coef(model1)[1:8]),
                         coefficient = coef(model1)[1:8],
                         row.names = NULL)
raw_tables <- list(model1 = sum_model1)

# create folders, if not they do not exist yet
paths <- c("tables/", "output/")
ifelse(!dir.exists(paths), 
       dir.create(path = paths),
       paste0(paths, " already exists"))
rm(paths)

# save list to rds file
saveRDS(raw_tables, file = "tables/raw_tables.RDS")

# write PDF file
knitr::knit2pdf("R/simple.Rnw", "tables/simple.tex")

# ALTERNATIVE, a lot prettier
# alpaca-baseline model
library(texreg)
baseline <- texreg(model1_bc,
                  caption = "Baseline model: Logit with TWFE",
                  caption.above = TRUE,
                  label = "tab:baseline_alpaca",
                  custom.model.names = "Using alpaca")

baseline <- baseline %>%
  str_replace("d7n11s", "$D_{DiDi}$") %>%
  str_replace("d7n21s", "$D_{LiDi}$") %>%
  str_replace("d7n31s", "$D_{DeDi}$") %>%
  str_replace("d7n32s", "$D_{DeLi}$") %>%
  str_replace("d7n33s", "$D_{DeDe}$") %>%
  str_replace("alliancedTRUE", "Allianced") %>%
  str_replace("majpowTRUE", "MajPow") %>%
  str_replace("logcapr", "LogCapRatio") 

writeLines(baseline, "tables/baseline.tex")
# original baseline model
# TODO: actually incorporate this
original <- read.csv2("input/baseline_model.csv",
                      sep = ",")

##########################
# models with only lili
###########################

# load time-specific models
before_wwi_bc <- readRDS("output/before_wwi_bc.rds")
between_ww_bc <- readRDS("output/between_ww_bc.rds")
cold_war_bc <- readRDS("output/cold_war_bc.rds")
post_1985_bc <- readRDS("output/post_1985_bc.rds")

# calculate pseudo R2
# pseudo_r2 <- c(1-model_only_lili_bc$deviance / model_only_lili_bc$null.deviance,
#                1-before_wwi_bc$deviance / before_wwi_bc$null.deviance,
#                1-between_ww_bc$deviance / between_ww_bc$null.deviance,
#                1-cold_war_bc$deviance / cold_war_bc$null.deviance,
#                1-post_1985_bc$deviance / post_1985_bc$null.deviance)

table_lili <- texreg(l = list(model_only_lili_bc,
                              before_wwi_bc,
                              between_ww_bc,
                              cold_war_bc,
                              post_1985_bc),
                     caption = "Binary choice model with TWFE",
                     caption.above = TRUE,
                     label = "tab:only_lili",
                     custom.model.names = paste0("(", 1:5, ")"),
                     custom.gof.rows = list("Years" = c("1815--2000",
                                                        "1815--1914",
                                                        "1915--1945",
                                                        "1946--2000",
                                                         "1986--2000") #,
                                            # "Pseudo $R^2$" = pseudo_r2
                                            ),
                     big.mark = " ",
                     custom.note = "%stars. Coefficients with analytical bias correction \\citep{stammann2018}")

table_lili <- table_lili %>%
  str_replace("d7n22s", "$D_{LiLi}$") %>%
  str_replace("alliancedTRUE", "Alliance") %>%
  str_replace("majpowTRUE", "MajPow") %>%
  str_replace("logcapr", "LogCapRatio") %>%
  # replace first one
  str_replace("hline", "toprule \n \\\\midrule") %>%
  # replace the middle ones
  str_replace("hline", "midrule") %>%
  str_replace("hline", "midrule") %>%
  str_replace("hline", "midrule \n \\\\bottomrule") %>%
  str_replace("Alliance", "\\\\midrule\n Alliance") %>%
  str_replace("Num. groups: dcode", "Num. dyads") %>%
  str_replace("Num. groups: year", "Num. years") 

writeLines(table_lili, "tables/only_lili.tex")

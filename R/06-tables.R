# create summary table for data used in baseline
model1 <- readRDS("output/model1.rds")
# need to merge d7n22 in as well so we have data on LiLi dyads
used_dataDT <- model1$data

fulldataDT <- as.data.table(readRDS("output/full_data.rds"))
fulldataDT <- fulldataDT[, c("dcode", "year", "d7n22s")
                         ][, `:=`(dcode = as.factor(dcode),
                                  year = as.factor(year))]

used_dataDT <- merge(used_dataDT,
                     fulldataDT,
                     by = c("dcode", "year"),
                     all.x = TRUE)
rm(fulldataDT)

# eliminate variables which aren't interesting in Table 1
used_dataDT <- used_dataDT[, c("mzmid1",
                               "d7n11s",
                               "d7n21s",
                               "d7n31s",
                               "d7n32s",
                               "d7n33s",
                               "d7n22s",
                               "allianced",
                               "majpow",
                               "logcapr")]


names(used_dataDT) <- c("MID onset",
                 "D_{DiDi}",
                 "D_{LiDi}",
                 "D_{DeDi}",
                 "D_{DeLi}",
                 "D_{DeDe}",
                 "D_{LiLi}",
                 "Allianced",
                 "MajPow",
                 "LogCapRatio")

library(stargazer)
library(tidyverse)

table_one <- stargazer(data = used_dataDT,
                       omit.summary.stat = "n",
                       title = "Sample description for the baseline model",
                       notes = "\\parbox[t]{10cm}{Observations: 40,786. Summary measures for the 
                       dependent variable (MID) and the explanatory variables with all
                       observations included in the baseline regression.}",
                       notes.append = TRUE,
                       notes.align = "l",
                       label = "tab:summary")
# TODO: delete unnecessary backslashes
# table_one <- table_one %>%
#   str_replace("\_", "_") %>%
#   str_replace("\\{", "{") %>%
#   str_replace("\\}", "}")

writeLines(table_one, "tables/table_one.tex")

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

# ALTERNATIVE
# TODO extract only variables of interest
texreg::texreg(model1)

# write PDF file
knitr::knit2pdf("R/simple.Rnw", "tables/simple.tex")


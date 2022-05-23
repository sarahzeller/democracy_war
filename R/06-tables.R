# create summary table for data used in baseline
model1 <- readRDS("output/model1.rds")
data <- model1$data[, 1:9]
names(data) <- c("MID onset",
                 "D_{DiDi}",
                 "D_{LiDi}",
                 "D_{DeDi}",
                 "D_{DeLi}",
                 "D_{DeDe}",
                 "Allianced",
                 "MajPow",
                 "LogCapRatio")

library(stargazer)
library(tidyverse)

table_one <- stargazer(data = data,
                       omit.summary.stat = "n",
                       title = "Sample description for the baseline model",
                       notes = "\\parbox[t]{10cm}{Observations: 40,786. Summary measures for the 
                       dependent variable (MID) and the explanatory variables with all
                       observations included in the baseline regression.}",
                       notes.append = TRUE,
                       notes.align = "l",
                       label = "tab:summary")
# TODO: delete unnecessary backslashes
# TODO: add D_lili
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

# write PDF file
knitr::knit2pdf("R/simple.Rnw", "tables/simple.tex")


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
library(xtable)
table1 <- xtable(coef(summary(model1))[1:8,])
writeLines(table1, "tables/model1.tex")


# TODO: get this to work so I can compile PDFs directly from R
# library(tinytex)
# pdflatex("tables/basic_document.tex")

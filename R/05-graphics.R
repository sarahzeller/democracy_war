source("set-up.R")
dataDT<- readRDS("output/used_dataDT.rds")
library(tidyverse)
library(scales)

# show CapRatio and LogCapRatio

not_scientific <- format_format(big.mark = " ",
                                scientific = FALSE)

hist_lcapr <- dataDT %>%
ggplot(aes(logcapr)) +
  geom_histogram(bins = 50)+
  custom_theme()+
  ggtitle("Logarithmized Capital Ratio")+
  xlab("LogCapRatio") + 
  scale_x_continuous(labels = not_scientific) + 
  scale_y_continuous(labels = not_scientific)


hist_capr <- dataDT %>%
  ggplot(aes(exp(logcapr))) +
  geom_histogram(bins = 50) +
  custom_theme() +
  ggtitle("Capital Ratio") +
  xlab("CapRatio") + 
  scale_x_continuous(labels = not_scientific) + 
  scale_y_continuous(labels = not_scientific)

both_hist <- cowplot::plot_grid(hist_capr, hist_lcapr) 
ggsave_embed("graphics/both_hist.pdf",
             width = 10)


######################################
# bar chart with percentage of dyads
#####################################
# create categorical variable for each dyad regime type combination
dataDT[`D_{DeDi}` == 1, regime_type := "1_dedi"
       ][`D_{DiDi}` == 1, regime_type := "2_didi"
       ][`D_{DeDe}` == 1, regime_type := "3_dede"
       ][`D_{LiDi}` == 1, regime_type := "4_lidi"
       ][`D_{DeLi}` == 1, regime_type := "5_deli"
       ][`D_{LiLi}` == 1, regime_type := "6_lili"]

dataDT %>%
  ggplot(aes(x = regime_type,
             fill = factor(regime_type != "6_lili"))) +
  geom_bar(width = .4,
           show.legend = FALSE) +
  coord_flip() + 
  xlab("") +
  ylab("Frequency") +
  # ggtitle("Distribution of regime type combinations") +
  custom_theme() + 
  scale_fill_manual(name = "regime_type", values=c("blue4","grey50")) +
  scale_x_discrete(labels = c("DeDi",
                              "DiDi",
                              "DeDe",
                              "LiDi",
                              "DeLi",
                              "LiLi")) + 
  scale_y_continuous(labels = not_scientific)
ggsave_embed(name = "graphics/regime_type_bar_chart.pdf",
             width = 5,
             height = 2.5)

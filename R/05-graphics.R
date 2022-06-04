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


###############################################
# show alliance in used data
##############################################
# find proportion of alliances
alliances <- dataDT[, .(sum_alliance = sum(allianced),
                        sum_dyads = length(unique(dcode))), by = year
                    ][, `:=`(prop_alliance = sum_alliance / sum_dyads * 100,
                             year = as.integer(as.character(year)))] 

# find extreme years
alliances[prop_alliance == min(prop_alliance) | 
            prop_alliance == max(prop_alliance)]

alliances %>%
ggplot(aes(x = year, y = prop_alliance)) +
  geom_line() +
  custom_theme() +
  xlab("") + 
  ylab("Allied dyads (%)") +
  annotate("rect",
           xmin = c(1939, 1914, 1989, 1815),
           xmax = c(1945, 1918, 1990, 1816),
           ymin = 0,
           ymax = 45,
           alpha = .2) +
  annotate("text",
           x = c(1941, 1915, 1985, 1830),
           y = 47,
           label = c("WW II",
                     "WW I",
                     "End of Cold War",
                     "Congress of Vienna"),
           size = 3,
           col = "grey30") +
  scale_x_continuous(limits = c(1810, 2000), 
                     breaks = c(1815, 
                                1850, 
                                1900,
                                1950,
                                2000))
ggsave_embed(name = "graphics/alliances_number.pdf",
             width = 7)


############
# TODO: show relationship b/w original data + used data
# how many kicked out?
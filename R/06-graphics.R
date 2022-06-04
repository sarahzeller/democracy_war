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
dataDT[d7n31s == 1, regime_type := "DeDi"
       ][d7n11s == 1, regime_type := "DiDi"
       ][d7n33s == 1, regime_type := "DeDe"
       ][d7n21s == 1, regime_type := "DiLi"
       ][d7n32s == 1, regime_type := "DeLi"
       ][d7n22s == 1, regime_type := "LiLi"]

# summarize first
dataDT[, .(Frequency = .N), by = regime_type
       # order by upside-down frequency
       ][order(-Frequency)
         # force this order
       ][, regime_type := factor(regime_type,
                                 levels = regime_type)] %>%
  ggplot(aes(x = regime_type,
             y = Frequency,
             fill = factor(regime_type != "LiLi"))) +
  geom_col(width = .4,
           show.legend = FALSE) +
  coord_flip() + 
  xlab("") +
  # ggtitle("Distribution of regime type combinations") +
  custom_theme() + 
  scale_fill_manual(name = "regime_type", values=c("blue4","grey50")) +
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
                             year = as.integer(as.character(year)))
                    ][order(year)] 

# add NAs for years without conflict
# years without conflicts
no_conflict <- data.table(year = (1815:2000)[which(!1815:2000 %in% dataDT$year)])
alliances <- merge(x = alliances,
                   y = no_conflict,
                   by = "year",
                   all = TRUE)

# find extreme years
alliances[prop_alliance == min(prop_alliance, na.rm = TRUE) | 
            prop_alliance == max(prop_alliance, na.rm = TRUE)]

alliances %>%
ggplot(aes(x = year, y = prop_alliance)) +
  geom_path() +
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
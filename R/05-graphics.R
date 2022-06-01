dataDT <- readRDS("output/full_data.rds")
library(tidyverse)
library(scales)

# show CapRatio and LogCapRatio
hist_lcapr <- dataDT %>%
ggplot(aes(logcapr)) +
  geom_histogram(bins = 50)+
  custom_theme()+
  ggtitle("Logarithmized Capital Ratio")+
  xlab("LogCapRatio") + 
  scale_x_continuous(labels = not_scientific) + 
  scale_y_continuous(labels = not_scientific)

not_scientific <- format_format(big.mark = " ",
                                scientific = FALSE)

hist_capr <- dataDT %>%
  ggplot(aes(exp(logcapr))) +
  geom_histogram(bins = 50) +
  custom_theme() +
  ggtitle("Capital Ratio") +
  xlab("CapRatio") + 
  scale_x_continuous(labels = not_scientific) + 
  scale_y_continuous(labels = not_scientific)

both_hist <- cowplot::plot_grid(hist_capr, hist_lcapr) 
ggsave("graphics/both_hist.pdf", 
       both_hist,
       width = 10)

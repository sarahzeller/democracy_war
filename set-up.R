library(data.table)

# set up stata-R interface
library(RStata)
print("Choose where your stata .exe file lies")
ifelse(is.null(options("RStata.StataPath")), chooseStataBin(), "Stata path already chosen")
options("RStata.StataVersion" = 14.2)


## Custom theme for ggplot
library(extrafont)
loadfonts(device = "win")
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.55.0/bin/gswin64c.exe")
ggsave_embed <- function(name, width = NA, height = NA){
  ggsave(name, width = width, height = height)
  embed_fonts(name)
}

custom_theme <- function(){
  theme_minimal()+
    theme(text = element_text(family = "Open Sans"),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 13, 
                                       color = "grey50"),
          axis.text.y = element_text(size = 10, 
                                     color = "grey10",
                                     hjust = 0),
          axis.text.x = element_text(hjust = 0),
          axis.line.x = element_line(colour = "gray40"),
          panel.grid = element_blank())
}

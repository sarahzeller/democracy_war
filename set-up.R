library(data.table)
library(naniar)

# set up stata-R interface
library(RStata)
print("Choose where your stata .exe file lies")
ifelse(is.null(options("RStata.StataPath")), chooseStataBin(), "Stata path already chosen")
options("RStata.StataVersion" = 14.2)


## Custom theme for ggplot
library(extrafont)
loadfonts(device = "win")
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.55.0/bin/gswin64c.exe")
ggsave_embed <- function(name, width){
  ggsave(name, width = width)
  embed_fonts(name)
}

custom_theme <- function(){
  theme_minimal()+
    theme(text = element_text(family = "Open Sans"),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 13, color = "grey30"),
          axis.text.y = element_text(size = 10, color = "grey10"),
          axis.line.x = element_line(colour = "gray40"),
          panel.grid = element_blank())
}

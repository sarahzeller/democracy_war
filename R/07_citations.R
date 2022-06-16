library(renv)
dep <- dependencies()
packages <- unique(dep$Package)
#
# references <- c(toBibtex(citation()))
# for(p in 1:length(packages)){
#   references[[p+1]] <- toBibtex(citation(packages[p]))
# }

knitr::write_bib(c(packages, "base"), "output/packages.bib")

# library(grateful)
# cite_packages(output = "paragraph",
#               out.dir = "output/",
#               cite.tidyverse = TRUE,
#               include.RStudio = TRUE)

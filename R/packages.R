if (!require(pacman)){
    install.packages("pacman")
}

pacman::p_load(char = c("rnaturalearth","rnaturalearthdata",
                 "rgeos", "mapproj", "ggrepel",
                 "tidyverse", "plotly", 
                 "grid", "scales", "countrycode",
                 "igraph", "conflicted"
))

conflicted::conflict_prefer("crossing", "tidyr")
conflicted::conflict_prefer("filter", "dplyr")

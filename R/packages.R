if (!require(pacman)){
    install.packages("pacman")
}

pacman::p_load(char = c("rnaturalearth","rnaturalearthdata",
                 "rgeos", "mapproj", "ggrepel",
                 "tidyverse", "plotly", 
                 "grid", "scales", "countrycode"
))

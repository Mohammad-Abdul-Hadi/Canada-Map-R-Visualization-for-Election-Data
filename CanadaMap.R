# my_packages <- c("tidyverse", "broom", "coefplot", "cowplot",
#                  "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
#                  "here", "interplot", "margins", "maps", "mapproj",
#                  "mapdata", "MASS", "quantreg", "rlang", "scales",
#                  "survey", "srvyr", "viridis", "viridisLite", "devtools")
# 
# install.packages(my_packages, repos = "http://cran.rstudio.com")
# 
# devtools::install_github("kjhealy/socviz")

# install.packages("sf")
# install.packages("rgdal")
# install.packages("geojsonio")
# install.packages("spdplyr")
# install.packages("rmapshaper")
# install.packages("tidyverse")
# install.packages("socviz")

library(geojsonio)
library(rmapshaper)
library(rgdal)
library(tidyverse)
library(spdplyr)
library(sf)
library(socviz)

## Map theme
theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

theme_set(theme_map())

## Make a "figures" directory if one doesn't exist
ifelse(!dir.exists(file.path("C:/Users/mashuk/Documents/GitHub/Canada-Map-R-Visualization-for-Election-Data/figures")), dir.create(file.path("figures")), FALSE)

canada_raw <- readOGR(dsn = "C:/Users/mashuk/Documents/GitHub/Canada-Map-R-Visualization-for-Election-Data/data", layer = "gcd_000b11a_e", use_iconv=TRUE, encoding="utf8")

canada_raw_json <- geojson_json(canada_raw)
canada_raw_sim <- ms_simplify(canada_raw_json)

geojson_write(canada_raw_sim, file = "C:/Users/mashuk/Documents/GitHub/Canada-Map-R-Visualization-for-Election-Data/data/canada_cd_sim.geojson")

canada_cd <- st_read("C:/Users/mashuk/Documents/GitHub/Canada-Map-R-Visualization-for-Election-Data/data/canada_cd_sim.geojson", quiet = TRUE)
canada_cd

canada_cd <- st_transform(canada_cd, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
canada_cd

map_colors <-  RColorBrewer::brewer.pal(8, "Pastel1")
map_colors <- rep(map_colors, 37)
## Draw the map
p <- ggplot(data = canada_cd, 
            mapping = aes(fill = PRUID))
p_out <- p + geom_sf(color = "gray60", 
                     size = 0.1) + 
  scale_fill_manual(values = map_colors) + 
  guides(fill = FALSE) + 
  theme_map() + 
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))
ggsave("C:/Users/mashuk/Documents/GitHub/Canada-Map-R-Visualization-for-Election-Data/figures/canada.jpg", p_out, height = 12, width = 15)
p_out

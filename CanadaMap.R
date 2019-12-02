my_packages <- c("tidyverse", "broom", "coefplot", "cowplot",
                "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
                "here", "interplot", "margins", "maps", "mapproj",
                "mapdata", "MASS", "quantreg", "rlang", "scales",
                "survey", "srvyr", "viridis", "viridisLite", "devtools")
install.packages(my_packages, repos = "http://cran.rstudio.com")
devtools::install_github("kjhealy/socviz")

install.packages("geojsonio")
install.packages("spdplyr")
install.packages("rmapshaper")
install.packages("socviz")

library(geojsonio)
library(rmapshaper)
library(rgdal)
library(tidyverse)
library(spdplyr)
library(sf)
library(socviz)
# example("inner_join")

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
## ifelse(!dir.exists(file.path("D:/Area 51.2/UBC/Semester 1/DATA 501/Canada-Map-R-Visualization-for-Election-Data/FirstProject/figures")), dir.create(file.path("figures")), FALSE)

canada_raw <- readOGR(dsn = "D:/Area 51.2/UBC/Semester 1/DATA 501/Canada-Map-R-Visualization-for-Election-Data/FirstProject/data", layer = "gcd_000b11a_e", use_iconv=TRUE, encoding="utf8")

canada_raw_json <- geojson_json(canada_raw)
canada_raw_sim <- ms_simplify(canada_raw_json)

geojson_write(canada_raw_sim, file = "D:/Area 51.2/UBC/Semester 1/DATA 501/Canada-Map-R-Visualization-for-Election-Data/FirstProject/data/canada_cd_sim.geojson")

canada_cd <- st_read("D:/Area 51.2/UBC/Semester 1/DATA 501/Canada-Map-R-Visualization-for-Election-Data/FirstProject/data/canada_cd_sim.geojson", quiet = TRUE)
## canada_cd

canada_cd <- st_transform(canada_cd, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
canada_mp <- as.data.frame(canada_cd)
str(canada_mp)
# canada_cd$PRUID

canada_mp$CDNAME <- NULL
canada_mp$CDTYPE <- NULL
canada_mp$PRNAME <- NULL

canada_mp$PRUID <- as.numeric(levels(canada_mp$PRUID))[canada_mp$PRUID]
canada_mp$CDUID <- as.numeric(levels(canada_mp$CDUID))[canada_mp$CDUID]

# Read Data into R
url <- "D:/Area 51.2/UBC/Semester 1/DATA 501/Canada-Map-R-Visualization-for-Election-Data/FirstProject/data/PRUID(1st election on the database).csv"
elections1 <- read_csv(file = url)
elections1 <- as.data.frame(elections1)
class(elections1)

# Read Data into R
url <- "D:/Area 51.2/UBC/Semester 1/DATA 501/Canada-Map-R-Visualization-for-Election-Data/FirstProject/data/PRUID(2nd election on the database).csv"
elections2 <- read_csv(file = url)
elections2 <- as.data.frame(elections2)
class(elections2)

# Read Data into R
url <- "D:/Area 51.2/UBC/Semester 1/DATA 501/Canada-Map-R-Visualization-for-Election-Data/FirstProject/data/PRUID(3rd election on the database).csv"
elections3 <- read_csv(file = url)
elections3 <- as.data.frame(elections3)
class(elections3)

map_colors <-  RColorBrewer::brewer.pal(8, "Pastel1")
map_colors <- rep(map_colors, 37)

canada_mp <- left_join(canada_mp, elections3[])
canada_mp$PRUID <- as.factor(canada_mp$PRUID)
canada_mp$CDUID <- as.factor(canada_mp$CDUID)
canada_mp$Province <- as.factor(canada_mp$Province)
canada_mp$Party <- as.factor(canada_mp$Party)

party_colors <- c("#EC7063", "#A569BD", "#5DADE2", "#48C9B0", "#52BE80", "#F4D03F", "#DC7633", "#CACFD2", "#99A3A4", "#566573", "#A93226", "#2C3E50","#229954")

head(canada_mp)

## Draw the map
p <- ggplot(data = canada_mp, 
            mapping = aes(fill = Party, geometry = geometry))  # Party # PRUID # CDUID
p_out <- p + geom_sf(color = "white", # gray60
                     size = 0.1) + 
  scale_fill_manual(values = party_colors) + # values = party_colors # rep("#8D021F", 13) # map_colors
  # guides(fill = FALSE) + 
  theme_map() + 
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))
# ggsave("D:/Area 51.2/UBC/Semester 1/DATA 501/Canada-Map-R-Visualization-for-Election-Data/FirstProject/figures/canada.jpg", p_out, height = 12, width = 15)
p_out

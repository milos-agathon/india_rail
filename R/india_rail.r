# How to compute and map rail density using R
# Milos Popovic
# 2021/05/23

#load libraries
library(plyr, quietly=T)
library(tidyverse, quietly=T) 
library(sf, quietly=T)
library(ggplot2, quietly=T) 
library(dplyr, quietly=T)
library(classInt, quietly=T)

set.seed(20210523)

#download official 2019 Indian sub-district shapefiles
u <- "https://github.com/justinelliotmeyers/India_Official_Boundaries_2019/archive/refs/heads/master.zip"
download.file(u, basename(u), mode="wb")
unzip("master.zip") #unzip master file
shpzip <- list.files(path = paste0(getwd(), "/India_Official_Boundaries_2019-master"), 
  pattern="^India_Subdistrict.*\\.zip$", 
  full.names = T)
outDir <- getwd()
ldply(.data = shpzip, .fun = unzip, exdir = outDir) # unzip all shapefiles

#create a list of Indian subdistrict shapefiles for import
shps <- list.files(pattern="^India_Subdistrict.*\\.shp$")
#Use lapply to import all shapefiles in the list
india <- lapply(shps, function(ind_shp) {
  ind <- st_read(ind_shp) %>%  #next, read all shp files as "sf" object and assign WSG84 projection
         st_transform(crs = 4326)
  return(ind)
  }) %>% 
bind_rows() #finally, merge subdistricts into single data.frame

# get OSM data for India (size 1992.9 MB)
url <- "https://download.geofabrik.de/asia/india-latest-free.shp.zip"
download.file(url, basename(url), mode="wb")
unzip("india-latest-free.shp.zip")

#load railways and subset rail type
rail <- st_read("gis_osm_railways_free_1.shp", stringsAsFactors = FALSE) %>% 
        st_transform(4326) %>% 
        st_as_sf() %>%
        filter(fclass=="rail")

#create copies
ind <- india
r <- rail

# intersect rails by subdistrict polys and compute length
ints <- st_intersection(r, ind) %>% 
        dplyr::mutate(len_m = sf::st_length(geometry)) %>% # returns length in meters
        dplyr::group_by(sdtcode11)

int <- as.data.frame(as.matrix(ints)) # place it into data.frame
int$len_m <- as.numeric(as.character(int$len_m)) #length must be numeric
int$sdtcode11 <- as.character(int$sdtcode11) #subdistrict code must be string
ii <- ddply(int, "sdtcode11", summarise, lr_m = sum(len_m, na.rm = TRUE)) #aggregate length by subdistrict

# join indian subdistricts and rail length by subdistrict code
df <- merge(ind, ii, by='sdtcode11', all.x=T)

# let's calculate area size in square kilometers
df$area_sqkm <- st_area(df) / 1000000 #st_area returns square meters so we get square km by dividing the result by 1000 squared

# rail density: km or rail / 100 square km
df$rail_dens <- (df$lr_m/1000) / df$area_sqkm * 100

# let's find a natural interval with quantile breaks
ni = classIntervals(df$rail_dens, 
           n = 6, 
           style = 'quantile')$brks

# this function uses above intervals to create categories
labels <- c()
for(i in 1:length(ni)){
    labels <- c(labels, paste0(round(ni[i], 0), 
                             "–", 
                             round(ni[i + 1], 0)))
}
labels <- labels[1:length(labels)-1]

# finally, carve out the categorical variable based on the breaks and labels above
df$cat <- cut(df$rail_dens, 
              breaks = ni, 
              labels = labels, 
              include.lowest = T)
levels(df$cat) # let's check how many levels it has (6)

# label NAs, too
lvl <- levels(df$cat)
lvl[length(lvl) + 1] <- "No rail"
df$cat <- factor(df$cat, levels = lvl)
df$cat[is.na(df$cat)] <- "No rail"
levels(df$cat)

# plot
p <- ggplot() +
geom_sf(data=df, aes(fill = cat), color=NA, size=0) +
    coord_sf(crs = 4326, datum = NA) +
    theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size=9, color="grey60", hjust=.55, vjust=25),
    axis.title.y = element_blank(),
    legend.position = c(.7, .85),
    legend.text = element_text(size=10, color="grey20"),
    legend.title = element_text(size=11, color="grey20"),
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin     =   unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
    plot.title = element_text(face="bold", size=20, color="#2a1559", hjust=.5, vjust=-2),
    plot.subtitle = element_text(size=16, color="#b72286", hjust=.5, vjust=-2),
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    legend.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank()) +
  labs(x = "©2021 Milos Popovic https://milospopovic.net\n Data: OSM Geofabrik India",
    title = "Railway density in India", 
    subtitle = "at subdistrict level", 
    caption = "") +
  scale_fill_manual(name= expression(paste("1 km of rail per 100", km^{2}, "of land area")), 
    values = rev(c("grey80", '#2a1559', '#701d6f', '#b12884', '#de529a', '#ee8eb0', '#f8c5c6')),
    labels = c("~0–3",     "3–5",    "5–7",   "7–11",   "11–18", ">18",  "No rail"),
    drop = F)+
  guides(color=F, fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.15, units = "mm"),
            keywidth = unit(12, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            # also the guide needs to be reversed
            reverse = F,
            label.position = "bottom"
          )
    )

ggsave(filename="ind_rail_density.png", width= 7, height= 9.15, dpi = 600, device='png', p)
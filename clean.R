# load packages
library(rgdal)
library(dplyr)


# load data
mapping_table_ori<-read.csv("data/clsc_census_mapping_gd.csv", stringsAsFactors = FALSE)
# cd16.shp <- readOGR(dsn = "data/cd", layer = "cd_qc_16", encoding = "UTF-8", stringsAsFactors = FALSE)
# csd16.shp <- readOGR(dsn = "data/csd", layer = "csd_qc_16", encoding = "UTF-8", stringsAsFactors = FALSE)
# ct16.shp <- readOGR(dsn = "data/ct", layer = "ct_qc_16", encoding = "UTF-8", stringsAsFactors = FALSE)
# da16.shp <- readOGR(dsn = "data/da", layer = "da_qc_16", encoding = "UTF-8", stringsAsFactors = FALSE)
clsc.shp <-readOGR(dsn = "data/clsc", layer="Territoires_CLSC_2017", encoding = "UTF-8", stringsAsFactors = FALSE)
# clsc.shp<-spTransform(clsc.shp, CRS(proj4string(csd16.shp)))

# creat mapping table with clsc name
clsc.name <- clsc.shp@data[, c("CLSC_nom", "CLSC_code")]
clsc.name$CLSC_code <- as.integer(clsc.name$CLSC_code)
clsc.name$CLSC_nom <-iconv(clsc.name$CLSC_nom, from = "UTF-8", to ="LATIN1")
mapping_table<- left_join(mapping_table_ori, clsc.name, by = "CLSC_code")

write.csv(mapping_table, file = "mapping_table_cleaned.csv")
# # creat mapping table with cd name
# cd.name <- cd16.shp@data[, c("CDUID", "CDNAME")]
# cd.name$CDUID <- as.numeric(cd.name$CDUID)
# cd.name$CDNAME <-iconv(cd.name$CDNAME, from = "UTF-8", to ="LATIN1")
# mapping_table<- left_join(mapping_table_ori[mapping_table_ori$census_type == "CD",], cd.name, by = "CLSC_code")
# 

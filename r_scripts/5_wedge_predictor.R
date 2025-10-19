
# Load in shapefiles --------------------------------------------
#
#
#---
# US layer (projected)
spread_ALBERS = st_read("shapefiles/invasion_data/uscounties_ALBERS.shp")
# wedges (projected)
wedges_16_long <- st_read("shapefiles/wedges/wedges_16_long.shp")
wedges_16_long$FID <- c(2:16,1)
plot(wedges_16_long)
# map to confirm overlay
resize.win(9,9)
plot(st_geometry(spread_ALBERS))
plot(wedges_16_long,col="transparent", add=T)
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''




# check and save output --------------------------------------------
#
#
#---
i <- 1
county_centroids <- st_centroid(spread_ALBERS)
county_centroids$cty_a_km <- round(st_area(spread_ALBERS))/1e6
wedge_ID_sp <-  st_join(wedges_16_long, county_centroids)
table(wedge_ID_sp$FID)

wedge_ID_sp[which(wedge_ID_sp$FID %in% 3:7),] # we lose two counties

county_wedge_size_sp <- wedge_ID_sp
st_write(county_wedge_size_sp, "shapefiles/wedges/wedge_cty_area_sp.shp", delete_layer=T)


wedge_ID_sp$FID <- ifelse(wedge_ID_sp$FID == 3, 2, wedge_ID_sp$FID)
table(wedge_ID_sp$FID)
wedge_ID_sp[which(wedge_ID_sp$FID %in% 3:7),] # we lose two counties

st_write(wedge_ID_sp, "shapefiles/wedges/wedge_ID_sp.shp", delete_layer=T)
warnings()
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''







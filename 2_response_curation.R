# Load packages  --------------------------------------------
#
#
#---
source("r_scripts/1_packages.R")
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Beach leaf disease read in excel 
BLD_cty <- read_excel("data/human_population_density/US BLD Counties by Year 2024.xlsx")

# Load in county shapefile --------------------------------------------

BLD_cty <- read_excel("data/human_population_density/US BLD Counties by Year 2024.xlsx")

BLD_cty$State_province <- toupper(BLD_cty$State_province)

BLD_cty$State_Name <- state.name[match(BLD_cty$State_province, state.abb)]

BLD_cty <- as_tibble(BLD_cty)

BLD_cty <- write_xlsx(BLD_cty,"data/human_population_density/US_BLD_Counties_by_Year_2024_full_state_names.xlsx")


# Load libraries
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(tidycensus)  

# Read original Excel file

BLD_cty <- read_excel("data/human_population_density/US BLD Counties by Year 2024.xlsx")

BLD_all <- BLD_cty %>%
  rename(state = State_province) %>%
  mutate(
    state = toupper(state),
    State_Name = if_else(
      state %in% state.abb,
      state.name[match(state, state.abb)],
      state
    ),
    Country = if_else(state %in% state.abb, "USA", "Canada"),
    
    county_clean = case_when(
      Country == "USA" ~ County %>%
        str_remove_all(regex(" County| Parish| Borough| Census Area| Municipality| city and borough", ignore_case = TRUE)) %>%
        str_trim() %>%
        str_to_title(),
      TRUE ~ NA_character_
    ),
    
    State_Name = str_to_title(State_Name)
  )


library(dplyr)
library(stringr)
library(tidycensus)

data("fips_codes", package = "tidycensus")

library(dplyr)
library(stringr)

fips_clean <- fips_codes %>%
  mutate(
    county_clean = county %>%
      str_remove_all(regex(" county| parish| borough| census area| municipality| city and borough", ignore_case = TRUE)) %>%
      str_trim() %>%
      str_to_title()
  )

BLD_all_joined <- BLD_all %>%
  left_join(fips_clean, by = c("state", "county_clean"))



# Add both FIPS together 
BLD_all_joined <- BLD_all_joined %>%
  mutate(full_fips = paste0(state_code, county_code))

# Create a new excel file 

write_xlsx(BLD_all_joined, "data/human_population_density/BLD_all_joined_full_fips.xlsx")

BLD_all_joined <- read_excel("data/human_population_density/BLD_all_joined_full_fips.xlsx")

# Convert the numeric values into numeric 
BLD_all_joined$full_fips <- as.numeric(BLD_all_joined$full_fips)  
table(nchar(BLD_all_joined$full_fips))

summary(BLD_all_joined)
hist(na.omit(BLD_all_joined$full_fips))

# Load in county BLD shapefile --------------------------------------------
#
#
#---

install.packages("sf")
library(sf)

## For USA
uscountiesall <- st_read(dsn="BLD_shapefiles", layer = "cb_2024_us_cousub_500k")
uscounties <- uscountiesall %>% filter(!STATE_NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))
table(uscounties$STATE_NAME)
uscounties$FIPS <- uscounties$GEOID
#uscounties$tot.pop <- uscounties$B01001_001
#uscounties$popden.km <- uscounties$B01001_cal
#uscounties <- uscounties %>% dplyr::select(FIPS, STATE_NAME, NAME, tot.pop, popden.km)

uscountiesall$full_fips <- paste(uscountiesall$STATEFP, uscountiesall$COUNTYFP, sep="") #- use this as column to left join 
unique(uscountiesall$STATE_NAME)
'%!in%' <- function(x,y)!('%in%'(x,y))
uscountiesall_crop <- uscountiesall[which(uscountiesall$STATE_NAME %!in% c("Alaska","Puerto Rico", "Hawaii", "United States Virgin Islands", "Guam", "American Samoa", "Commonwealth of the Northern Mariana Islands")),]
plot(st_geometry(uscountiesall_crop))

library(stringr)
## Ensures that I am only keeping the rows that I want 
BLD_filtered <- BLD_all_joined[, c("full_fips", "county_clean", "BLD_Year", "state")]

## All fips now have 5 digits
BLD_filtered$full_fips <- str_pad(BLD_filtered$full_fips, width = 5, side = "left", pad = "0")

## All fips are now unique from each other 
#BLD_filtered_unique <- BLD_filtered %>%
  #distinct(full_fips, .keep_all = TRUE)

## Keeps everything that matches with my data frame 
combined_sf_matched <- uscountiesall_crop %>%
  left_join(BLD_filtered, by = "full_fips")

# Want to look at Ohio only 
Ohio <- combined_sf_matched %>%
  filter(STATE_NAME == "Ohio")

plot(st_geometry(Ohio), 
     col = "lightblue", 
     border = "black",
     lwd = 1.2, 
     main = "Ohio Counties with BLD")

ggplot() + 
  geom_sf(data = Ohio, aes(fill = BLD_Year)) + 
  scale_fill_viridis_c() + 
  ggtitle('Chloropleth Map of Ohio') + 
  theme_minimal ()

## United States as a Whole 

ggplot() + 
  geom_sf(data = combined_sf_matched, aes(fill = BLD_Year)) + 
  scale_fill_viridis_c() + 
  ggtitle('Chloropleth Map of US') + 
  theme_minimal()

## Main 

Maine <- combined_sf_matched %>%
  filter(STATE_NAME == "Maine")
ggplot() + 
  geom_sf(data = Maine, aes(fill = BLD_Year)) + 
  scale_fill_viridis_c() + 
  ggtitle('Chloropleth Map of Maine') + 
  theme_minimal ()
  


## For Canada 

cancountiesall1 <- st_read ("/Users/brookeescue/Desktop/Beech Leaf Disease Project/BLD_can_shapefiles/lcd_000b21a_e/lcd_000b21a_e.shp")

cancountiesall2<- st_read(
  "/Users/brookeescue/Desktop/Beech Leaf Disease Project/BLD_can_shapefiles/lpr_000b21a_e/lpr_000b21a_e.shp"
)

cancounties_cropped <- st_intersection(cancountiesall1, st_union(cancountiesall2))

cancounties_all <- cancounties_cropped %>%
  rename(County = CDNAME) %>%
  mutate(
    County = str_trim(str_to_title(County))
  )


cancounties_all <- st_transform(cancounties_all, st_crs(combined_sf_matched))


Can_US <- bind_rows(combined_sf_matched, cancounties_all)


Can_US_albers <- st_transform(Can_US, 5070)

# Saving shapefile 
st_write(Can_US_albers, "shapefiles/invasion_data/Can_US_ALBERS.shp", delete_layer = TRUE)

uscounties_ALBERS <- st_transform(uscounties, CRS("+init=epsg:5070"))

st_write(uscounties_ALBERS, "shapefiles/invasion_data/uscounties_ALBERS.shp", delete_layer =T)

# Loop assigning invasion year to county --------------------------------------------

invaded_counties <- LWD_cty[!is.na(LWD_cty$lw_detection_year),]
nrow(invaded_counties)
invaded_counties
i <- 1
uscounties$Invaded <- NULL
uscounties$YrInv <- NULL
for(i in 1:nrow(uscounties)){
  curr_fip <- uscounties$FIPS[i]
  curr_fip_inv_status <- LWD_cty[which(LWD_cty$FIPS %in% curr_fip),"lw_detection_year"]
  curr_fip_inv_status <- ifelse(nrow(curr_fip_inv_status)==0,NA,curr_fip_inv_status)
  uscounties$Invaded[i] <- ifelse(is.na(curr_fip_inv_status), "no", "yes")
  uscounties$YrInv[i] <- curr_fip_inv_status
}
uscounties$YrInv <- unlist(uscounties$YrInv)
summary(uscounties$YrInv)
table(uscounties$YrInv)
sum(table(uscounties$YrInv))
#plot(uscounties["YrInv"])



# Load in master data LWD distribution data --------------------------------------------


# from Lynn: If the lw_detection_year is blank, it hasn't been found there yet. 
LWD_cty <- read_excel(path="data/LWD_county_level/Laurel_Wilt_Counties_2_9_2022.xlsx",
                      sheet = "Laurel_Wilt_Counties_2_9_2022",
                      range = "A1:H1264", col_names=T)
table(nchar(LWD_cty$FIPS))
LWD_cty$FIPS <- ifelse(nchar(LWD_cty$FIPS) == 4, paste("0", LWD_cty$FIPS, sep=""), paste(LWD_cty$FIPS))
table(nchar(LWD_cty$FIPS))

summary(LWD_cty)
hist(LWD_cty$lw_detection_year)

# remove 2022 data
LWD_cty <- LWD_cty[which(LWD_cty$lw_detection_year <= 2021),]
hist(LWD_cty$lw_detection_year)



# Load in county shapefile --------------------------------------------
#
#
#---
uscountiesall <- st_read(dsn="data/human_population_density", layer="County")
uscounties <- uscountiesall %>% filter(State %!in% c("Alaska", "Hawaii", "Puerto Rico"))
#plot(st_geometry(uscounties))
table(uscounties$State)
uscounties$FIPS <- uscounties$GEOID
uscounties$tot.pop <- uscounties$B01001_001
uscounties$popden.km <- uscounties$B01001_cal
uscounties <- uscounties %>% dplyr::select(FIPS, State, NAME, tot.pop, popden.km)


# Loop assigning invasion year to county --------------------------------------------
#
#
#---
invaded_counties <- LWD_cty[!is.na(LWD_cty$lw_detection_year),]
nrow(invaded_counties)
invaded_counties
i <- 1
uscounties$Invaded <- NULL
uscounties$YrInv <- NULL
for(i in 1:nrow(uscounties)){
  curr_fip <- uscounties$FIPS[i]
  curr_fip_inv_status <- LWD_cty[which(LWD_cty$FIPS %in% curr_fip),"lw_detection_year"]
  curr_fip_inv_status <- ifelse(nrow(curr_fip_inv_status)==0,NA,curr_fip_inv_status)
  uscounties$Invaded[i] <- ifelse(is.na(curr_fip_inv_status), "no", "yes")
  uscounties$YrInv[i] <- curr_fip_inv_status
}
uscounties$YrInv <- unlist(uscounties$YrInv)
summary(uscounties$YrInv)
table(uscounties$YrInv)
sum(table(uscounties$YrInv))
#plot(uscounties["YrInv"])

uscounties_ALBERS <- st_transform(uscounties, CRS("+init=epsg:5070"))
#uscounties_ALBERS$centroids <- st_centroid(st_geometry(uscounties_ALBERS))

st_write(uscounties_ALBERS, "shapefiles/invasion_data/uscounties_ALBERS.shp", delete_layer =T)







# IGNORE EVERYTHING BELOW HERE FOR NOW



# Loop creating dataset for COXPH model --------------------------------------------
#
#
#---
# create data frame to be populated
lwd_start_inv <- range(na.omit(uscounties$YrInv))[1]
lwd_last_obs <- 2021
lwd_time_series_length <- length(lwd_start_inv:lwd_last_obs)

lwd_df <- as.data.frame(uscounties)
# repeat each row so that it can have one row for each year in the invasion time series
lwd_df_CPH <-   lwd_df[rep(seq_len(nrow(lwd_df)), each = lwd_time_series_length), ]
nrow(lwd_df_CPH) == nrow(lwd_df)*lwd_time_series_length
lwd_counties <- unique(lwd_df_CPH$FIPS)
length(lwd_counties)==nrow(lwd_df)

table(lwd_df_CPH$YrInv)


lwd_pb = txtProgressBar(min = 0, max = length(lwd_counties), initial = 0) 

for(i in lwd_counties){
  #i <- lwd_df_CPH$FIPS[1]
  if(i == lwd_df_CPH$FIPS[1]){lwd_counter <- 1} else {lwd_counter <- lwd_counter +1}
  
  lwd_curr_county_df <- lwd_df_CPH[which(lwd_df_CPH$FIPS %in% i),]
  
  # create rows for CPH model
  lwd_curr_county_df$time0 <- lwd_start_inv:lwd_last_obs
  lwd_curr_county_df$time1 <- (lwd_start_inv+1):(lwd_last_obs+1)
  
  # get year of initial invasion at point AQUI
  lwd_year_of_invasion <- unique(ifelse(is.na(lwd_curr_county_df$YrInv), 2022,lwd_curr_county_df$YrInv))
  
  # assign 0s and 1s (if invaded) based on year of initial invasion
  lwd_curr_county_df$invaded_time1 <- ifelse(lwd_curr_county_df$time1 <= lwd_year_of_invasion, 0, 1)

  # run for loop to assess propagule pressure in each year for each point
  lwd_curr_county_df$prop_press_dist <- NA
  lwd_curr_county_df$prop_press_dist2 <- NA
  #
  for(r in 1:nrow(lwd_curr_county_df)){
    # r <- 13
    lwd_curr_year <- lwd_curr_county_df[r,"time0"]
    
    # get all previous invaded points, and calculate distance to such points
    lwd_curr_FIPS <- uscounties_ALBERS[which(uscounties_ALBERS$FIPS %in% lwd_curr_county_df$FIPS),]
    lwd_previously_invaded_points <- uscounties_ALBERS[which(uscounties_ALBERS$YrInv < lwd_curr_year),]
    
    suppressWarnings(
      lwd_dists_to_all_points <- st_distance(st_centroid(lwd_curr_FIPS), st_centroid(lwd_previously_invaded_points))
    )
    #
    lwd_dists_to_all_points <- lwd_dists_to_all_points[which(as.numeric(lwd_dists_to_all_points)>0)]/1000
    
    #
    lwd_curr_county_df$prop_press_dist[r] <- sum(1/(lwd_dists_to_all_points))
    lwd_curr_county_df$prop_press_dist2[r] <- sum(1/(lwd_dists_to_all_points^2))
  }
  
  #
  invasion_row  <- which(lwd_curr_county_df$invaded_time1 == 1)[1]
  if(is.na(invasion_row)==F){
    lwd_curr_county_df <- lwd_curr_county_df[1:invasion_row,]}
  
  
  if(i == lwd_df_CPH$FIPS[1]){lwd_CPH_fin <- lwd_curr_county_df} else {
    lwd_CPH_fin <- rbind.data.frame(lwd_CPH_fin,lwd_curr_county_df)}
  
  setTxtProgressBar(lwd_pb, lwd_counter)
}
summary(lwd_CPH_fin)
nrow(lwd_CPH_fin)

lwd_CPH_fin$geometry <- NULL
fwrite(lwd_CPH_fin, "data/CPH_data/CPH_LWD.csv", na=".")


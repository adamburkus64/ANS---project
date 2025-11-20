getwd()
setwd("/Users/ujvaribotond/Desktop/R/Applied_Network_Science/Project_work/202510-citibike-tripdata")


install.packages("janitor")
install.packages("ggraph")


# all this shit is needed
library(tidyverse)
library(sf)
library(data.table)
library(janitor)
library(igraph)
library(dplyr)

# getting the data
# link :  https://s3.amazonaws.com/tripdata/index.html
tripdata <- fread("202510-citibike-tripdata_1.csv")
tripdata <- janitor::clean_names(tripdata)



### If we want to use every data for the selected month (this is for the latest, 2025 October)
# tripdata1 <- fread("202510-citibike-tripdata_1.csv")
# tripdata2 <- fread("202510-citibike-tripdata_2.csv")
# tripdata3 <- fread("202510-citibike-tripdata_3.csv")
# tripdata4 <- fread("202510-citibike-tripdata_4.csv")
# tripdata5 <- fread("202510-citibike-tripdata_5.csv")
# tripdata <- rbind(tripdata, tripdata2, tripdata3, tripdata4, tripdata5)

# Start stations converted to sf points
start_sf <- tripdata %>%
  select(start_station_id, start_lat, start_lng) %>%
  distinct() %>%
  drop_na(start_lat, start_lng) %>%    # <-- remove broken stations
  st_as_sf(coords = c("start_lng", "start_lat"), crs = 4326) ## Defines the GPS system to be used
                                                              ## lat and longitudes + measures in degrees

### There are duplicates in this (2148 unique stations, but 2158 observations total)
start_sf %>% 
  distinct(start_station_id) %>% 
  count()

start_sf<- start_sf %>% 
  distinct(start_station_id, .keep_all = TRUE)


# end stations converted to sf points
end_sf <- tripdata %>%
  select(end_station_id, end_lat, end_lng) %>%
  distinct() %>%
  drop_na(end_lat, end_lng) %>%        # <-- remove broken stations
  st_as_sf(coords = c("end_lng", "end_lat"), crs = 4326)

### There are duplicates in this (1954 unique stations, but 1962 observations total)
end_sf %>% 
  distinct(end_station_id) %>% 
  count()

end_sf <- end_sf %>% 
  distinct(end_station_id, .keep_all = TRUE)


#### There are more starter stations than ending stations (this can be because we are using only part of the monthly data)
#### But still keep it in our mind when trying it with a whole month for example

?st_read
#--------------------------------
# getting the community districts
#--------------------------------
#(important, must get the shape (.shp)file and not csv or other shit)
cd <- st_read("NYC_Community_Districts_-6420008696060170624/NYC_Community_Districts.shp") %>%
  st_transform(4326)
view(cd)
# community district identifier is the left column, the number meanings:
# 1 - Manhattan
# 2 - Bronx
# 3 - Brooklyn
# 4 - Queens
# 5 - Staten Island
# the numbers after that are the district numbers within the boroughs (1-18)
# See all the community districts here: https://boundaries.beta.nyc/?map=cd


cd <- st_read("NYC_Community_Districts_-6420008696060170624/NYC_Community_Districts.shp") %>%
  st_transform(4326) %>%
  mutate(
    borough  = floor(BoroCD / 100),
    district = BoroCD %% 100
  ) %>%
  filter(district < 60)          # keep only real land CDs

#-------------------------------------------------------------
# spatial join to get the community district of start stations

start_join <- st_join(start_sf, cd["BoroCD"], left = TRUE)
### There are missing values for the BoroCDs!!!!
### But this is because parts of NYC, mainly Central Park are not under any boroughs

end_join <- st_join(end_sf, cd["BoroCD"], left = TRUE)
#-------------------------------------------------------------

# merging back to the tripdata
# attaching start and end districts to the data
tripdata_cd <- tripdata %>%
  left_join(
    start_join %>% st_drop_geometry() %>% rename(start_BoroCD = BoroCD),
    by = "start_station_id"
  ) %>%
  left_join(
    end_join %>% st_drop_geometry() %>% rename(end_BoroCD = BoroCD),
    by = "end_station_id"
  )
# getting a couple of warnings i do not know shit about but still running...

################################################################################
### HERE WE CAN FILTER THE DATA TO SEPERATE WEEKDAYS, WEEKENDS, ETC.
################################################################################
## Get weekdays (using ending days/times)
tripdata_cd$Weekday <- weekdays(tripdata_cd$ended_at)

tripdata_cd <- relocate(tripdata_cd, Weekday, .after = ended_at)

## Get hour to divide days and nights (using ending days/times)
tripdata_cd$Hour <- hour(tripdata_cd$ended_at)

tripdata_cd <- relocate(tripdata_cd, Hour, .after = Weekday)

### Slicing up the data to different time periods
tripdata_cd_morning <- tripdata_cd %>% filter(Hour>= 6 & Hour < 11)

tripdata_cd_afternoon <- tripdata_cd %>% filter(Hour>= 11 & Hour < 18)

tripdata_cd_evening <- tripdata_cd %>% filter(Hour >=18 & Hour < 23)

#### Slicing it to weekends and weekdays

tripdata_cd_weekday <- tripdata_cd %>% filter (Weekday == "Monday" |
                                   Weekday == "Tuesday" |
                                   Weekday == "Wednesday" |
                                   Weekday == "Thursday" |
                                   Weekday == "Friday")
unique(data_weekday$Weekday)


tripdata_cd_weekend <- data %>% filter (Weekday == "Saturday" | Weekday == "Sunday")


### Optionally get rid of the original tripdata_cd so that we don't fry our computers
### but it might be too late at this point
################################################################################



#lets try to get a district level bikeflow network

### Here is the same as 
###district_network <- tripdata_cd %>%
###  filter(!is.na(start_BoroCD), !is.na(end_BoroCD)) %>%
###  count(start_BoroCD, end_BoroCD, name = "trips")

### But in a function for every type of dataset

make_district_network_edgelist <- function(data) {
  data %>%
    filter(!is.na(start_BoroCD), !is.na(end_BoroCD)) %>%
    count(start_BoroCD, end_BoroCD, name = "trips")
}

### Do every edgelist with weights for all the different specifications


#-------------------------------------------------

# FLOW NETWORK GRAPH

library(igraph)
library(ggraph)
library(tidygraph)


# Convert to graph
g <- graph_from_data_frame(district_network, directed = TRUE)
### same but in a function for the Infomap (has to be directed)

do_the_graph_from_edgelist_Infomap <- function(edgelist_with_weights) {
  graph_from_data_frame(edgelist_with_weights, directed = T)
}

### same but in a function for the Louvain (CANNOT be directed)

do_the_graph_from_edgelist_Louvain <- function(edgelist_with_weights) {
  graph_from_data_frame(edgelist_with_weights, directed = F)
  
  

  
  

# Plot using ggraph, just  the function version instead of 
# ggraph(g, layout = "fr") + 
# geom_edge_link(aes(width = trips, alpha = trips),
#                  arrow = arrow(length = unit(3, 'mm')), end_cap = circle(3, 'mm'),
#                  colour = "steelblue") +
#   geom_node_point(size = 4, color = "black") +
#   geom_node_text(aes(label = name), repel = TRUE, size = 4) +
#   scale_edge_width(range = c(0.3, 3)) +
#   theme_minimal() +
#   labs(title = "NYC District Bike Flow Network",
#        edge_width = "trips")
# holy fucking shit it worked...or at least i got something XD
  
  plot_district_network <- function(g, title, link_color, node_color) {
    ggraph(g, layout = "fr") + 
      geom_edge_link(
        aes(width = trips, alpha = trips),
        arrow = arrow(length = unit(3, 'mm')),
        end_cap = circle(3, 'mm'),
        colour = link_color
      ) +
      geom_node_point(size = 4, color = node_color) +
      geom_node_text(aes(label = name), repel = TRUE, size = 4) +
      scale_edge_width(range = c(0.3, 3)) +
      theme_minimal() +
      labs(
        title = title,
        edge_width = "trips"




### This gives us the ability to compare the specifications with the same methods (Louvain and InfoMap)
### And also compare the different results of the two methods

### Still need to look at how we can compare these 
### Pssible ideas: For Between slices (e.g. morning/evening with Louvain): 
### Adjusted Rand Index (ARI), Normalized Mutual Information (NMI), Variation of Information (VI)
### Number of communities, community size distribution, modularity — compare summary stats



#------------------------------------------------------------------------------

# cord diagram ( funny coloreed circle that shows bike trips from district to district) (COMPUTATIONALY HEAVY ON THE PC!!)
install.packages("circlize")
library(circlize)


# Prepare matrix for chordDiagram
flow_mat <- district_network %>% 
  tidyr::pivot_wider(
    names_from = end_BoroCD,
    values_from = trips,
    values_fill = 0
  ) %>% 
  tibble::column_to_rownames("start_BoroCD") %>% 
  as.matrix()

# Chord diagram
circos.clear()
chordDiagram(
  flow_mat,
  transparency = 0.25,
  directional = 1,
  annotationTrack = "grid",
  preAllocateTracks = list(track.height = 0.05)
)

title("NYC District Bike Trip Flows — Chord Diagram")


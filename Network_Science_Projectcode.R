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
tripdata <- fread("202510-citibike-tripdata_1.csv")
tripdata2 <- fread("202510-citibike-tripdata_2.csv")
tripdata3 <- fread("202510-citibike-tripdata_3.csv")
tripdata4 <- fread("202510-citibike-tripdata_4.csv")
tripdata5 <- fread("202510-citibike-tripdata_5.csv")
tripdata <- rbind(tripdata, tripdata2, tripdata3, tripdata4, tripdata5)

rm(tripdata2, tripdata3, tripdata4, tripdata5)


# Start stations converted to sf points
start_sf <- tripdata %>%
  select(start_station_id, start_lat, start_lng) %>%
  distinct() %>%
  drop_na(start_lat, start_lng) %>%    # <-- remove broken stations
  st_as_sf(coords = c("start_lng", "start_lat"), crs = 4326) ## Defines the GPS system to be used
                                                              ## lat and longitudes + measures in degrees

### There are duplicates in this 
start_sf %>% 
  distinct(start_station_id) %>% 
  count()

### Getting rid of the duplicates (these may be due to very slight change in the coordinates)
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

### Getting rid of the duplicates
end_sf <- end_sf %>% 
  distinct(end_station_id, .keep_all = TRUE)

################################################################################
##### Looking at the stations that are in the starting stations but not the ending ones
##### and vice versa
################################################################################

starts  <- tripdata %>% distinct(start_station_id) %>% pull(start_station_id)
ends    <- tripdata %>% distinct(end_station_id)   %>% pull(end_station_id)

only_start <- setdiff(starts, ends)

only_end <- setdiff(ends, starts)
only_end
################################################################################



#### There are more starter stations than ending stations (this can be because we are using only part of the monthly data)
#### But still keep it in our mind when trying it with a whole month for example

?st_read
#--------------------------------
# getting the community districts
#--------------------------------
#(important, must get the shape (.shp)file and not csv or other shit)
### Keep the file inside the downloaded folder!!!! (otherwise it does not work)
setwd("/Users/ujvaribotond/Desktop/R/Applied_Network_Science/Project_work")



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
### But this is because parts of NYC, mainly because Central Park is not under any boroughs

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
unique(tripdata_cd_weekday$Weekday)


tripdata_cd_weekend <- tripdata_cd %>% filter (Weekday == "Saturday" | Weekday == "Sunday")
unique(tripdata_cd_weekend$Weekday)


### Optionally get rid of the original tripdata_cd so that we don't fry our computers
### but it might be too late at this point
rm(tripdata_cd, tripdata)
################################################################################




#lets try to get a district level bikeflow network

### Here is the same as 
###district_network <- tripdata_cd %>%
###  filter(!is.na(start_BoroCD), !is.na(end_BoroCD)) %>%
###  count(start_BoroCD, end_BoroCD, name = "trips")

### But in a function for every type of dataset (((((SOMETHING IS FUCKED ABOUT THIS)))))

make_district_network_edgelist <- function(data) {
  data %>%
    filter(!is.na(start_BoroCD), !is.na(end_BoroCD)) %>%
    count(start_BoroCD, end_BoroCD, name = "weight")
}

### Do every edgelist with weights for all the different specifications


#-------------------------------------------------

# FLOW NETWORK GRAPH

library(igraph)
library(ggraph)
library(tidygraph)


# Convert to graph
# g <- graph_from_data_frame(district_network, directed = TRUE)
### same but in a function for the Infomap (has to be directed)
?graph_from_data_frame

do_the_graph_from_edgelist_Infomap <- function(edgelist_with_weights) {
  graph_from_data_frame(edgelist_with_weights, directed = T)
}

### same but in a function for the Louvain (CANNOT be directed)

do_the_graph_from_edgelist_Louvain <- function(edgelist_with_weights) {
  graph_from_data_frame(edgelist_with_weights, directed = F)
}
  

  
  

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
        edge_width = "trips")
}



### This gives us the ability to compare the specifications with the same methods (Louvain and InfoMap)
### And also compare the different results of the two methods

### Still need to look at how we can compare these 
### Pssible ideas: For Between slices (e.g. morning/evening with Louvain): 
### Adjusted Rand Index (ARI), Normalized Mutual Information (NMI), Variation of Information (VI)
### Number of communities, community size distribution, modularity — compare summary stats

################################################################################
## Compare data specifications inside methods
################################################################################

####### Louvain #########
  ### Weekdays vs. weekends
weekday_edgelist <- make_district_network_edgelist(tripdata_cd_weekday)
  
weekend_edgelist <- make_district_network_edgelist(tripdata_cd_weekend)
  
  
  ### Had to make sure that all of the community districts are represented
################################################################################
  weekday_edgelist_nodes  <- weekday_edgelist %>% distinct(end_BoroCD) %>% pull(end_BoroCD)
  weekend_edgelist_nodes    <- weekend_edgelist %>% distinct(end_BoroCD)   %>% pull(end_BoroCD)
  
  only_start <- setdiff(weekday_edgelist_nodes, weekend_edgelist_nodes)
  only_start
  only_end <- setdiff(weekday_edgelist_nodes, weekend_edgelist_nodes)
  only_end
  
  
  morning_edgelist_nodes  <- morning_edgelist %>% distinct(start_BoroCD) %>% pull(start_BoroCD)
  evening_edgelist_nodes    <- evening_edgelist %>% distinct(start_BoroCD)   %>% pull(start_BoroCD)
  
  only_start <- setdiff(morning_edgelist_nodes, evening_edgelist_nodes)
  only_start
  only_end <- setdiff(morning_edgelist_nodes, evening_edgelist_nodes)
  only_end
################################################################################

#### Create the undirected graphs for the Louvain method
weekday_graph_Louvain <- do_the_graph_from_edgelist_Louvain(weekday_edgelist)

### Make sure the weights are indeed for the edges
E(weekday_graph_Louvain)$weight
  
  
weekend_graph_Louvain <- do_the_graph_from_edgelist_Louvain(weekend_edgelist)
### Make sure the weights are indeed for the edges
E(weekday_graph_Louvain)$weight

#### Run Louvain method on the graphs
louvain_weekday <- cluster_louvain(weekday_graph_Louvain, weights = E(weekday_graph_Louvain)$weight)
louvain_weekend <- cluster_louvain(weekend_graph_Louvain, weights = E(weekend_graph_Louvain)$weight)

#### Extract memberships
nodes <- V(weekday_graph_Louvain)$name  # choose canonical order

mem_louvain_weekday <- membership(louvain_weekday)[nodes]
mem_louvain_weekend <- membership(louvain_weekend)[nodes]

### Now compare them according to Adjusted Rand Index (ARI), 
###Normalized Mutual Information (NMI) and Variation of Information (VI)

# ARI
ari_me <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "adjusted.rand")

# NMI
nmi_me <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "nmi")

# VI
vi_me  <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "vi")




#### Mornings vs. evenings (optionally vs.afternoon)
morning_edgelist <- make_district_network_edgelist(tripdata_cd_morning)

afternoon_edgelist <- make_district_network_edgelist(tripdata_cd_afternoon)

evening_edgelist <- make_district_network_edgelist(tripdata_cd_evening)


#### Create the undirected graphs for the Louvain method
morning_graph_Louvain <- do_the_graph_from_edgelist_Louvain(morning_edgelist)
E(morning_graph_Louvain)$weight



afternoon_graph_Louvain <- do_the_graph_from_edgelist_Louvain(afternoon_edgelist)

evening_graph_Louvain <- do_the_graph_from_edgelist_Louvain(evening_edgelist)
E(evening_graph_Louvain)$weight



#### Run Louvain method on the graphs
louvain_morning <- cluster_louvain(morning_graph_Louvain, weights = E(morning_graph_Louvain)$weight)

louvain_afternoon <- cluster_louvain(afternoon_graph_Louvain, weights = E(afternoon_graph_Louvain)$weight)

louvain_evening <- cluster_louvain(evening_graph_Louvain, weights = E(evening_graph_Louvain)$weight)


#### Extract memberships
nodes <- V(morning_graph_Louvain)$name  # choose canonical order

mem_louvain_morning <- membership(louvain_morning)[nodes]

mem_louvain_afternoon <- membership(louvain_afternoon)[nodes]

mem_louvain_evening <- membership(louvain_evening)[nodes]

### Now compare them according to Adjusted Rand Index (ARI), 
###Normalized Mutual Information (NMI) and Variation of Information (VI)

# ARI
ari_me <- compare(mem_louvain_morning, mem_louvain_evening, method = "adjusted.rand")

# NMI
nmi_me <- compare(mem_louvain_morning, mem_louvain_evening, method = "nmi")

# VI
vi_me  <- compare(mem_louvain_morning, mem_louvain_evening, method = "vi")



################################################################################
#### INFOMAP (experimental, nem futtattam még le xd)
################################################################################
### Weekdays vs. weekends

#### Create the directed graph for the Infomap method
weekday_graph_Infomap <- do_the_graph_from_edgelist_Infomap(weekday_edgelist)

weekend_graph_Infomap <- do_the_graph_from_edgelist_Infomap(weekend_edgelist)

#### Run Infomap method on the graphs
?cluster_infomap
infomap_weekday <- cluster_infomap(weekday_graph_Infomap, e.weights = E(weekday_graph_Infomap)$weight)
infomap_weekend <- cluster_infomap(weekend_graph_Infomap, e.weights = E(weekend_graph_Infomap)$weight)

#### Extract memberships
nodes <- V(weekday_graph_Louvain)$name  # choose canonical order

mem_infomap_weekday <- membership(infomap_weekday)[nodes]
mem_infomap_weekend <- membership(infomap_weekend)[nodes]

### Now compare them according to Adjusted Rand Index (ARI), 
###Normalized Mutual Information (NMI) and Variation of Information (VI)

# ARI
ari_me <- compare(mem_infomap_weekday, mem_infomap_weekend, method = "adjusted.rand")

# NMI
nmi_me <- compare(mem_infomap_weekday, mem_infomap_weekend, method = "nmi")

# VI
vi_me  <- compare(mem_infomap_weekday, mem_infomap_weekend, method = "vi")





#### Mornings vs. evenings (optionally vs.afternoon) ((doesn't work yet...))

#### Create the directed graphs for the Infomap method
morning_graph_Infomap <- do_the_graph_from_edgelist_Infomap(morning_edgelist)

afternoon_graph_Infomap <- do_the_graph_from_edgelist_Infomap(afternoon_edgelist)

evening_graph_Infomap <- do_the_graph_from_edgelist_Infomap(evening_edgelist)

#### Run Infomap method on the graphs
infomap_morning <- cluster_infomap(morning_graph_Infomap, e.weights = E(morning_graph_Infomap)$weight)

infomap_afternoon <- cluster_infomap(afternoon_graph_Infomap, e.weights = E(afternoon_graph_Infomap)$weight)

infomap_evening <- cluster_infomap(evening_graph_Infomap, e.weights = E(evening_graph_Infomap)$weight)


#### Extract memberships

nodes <- V(morning_graph_Louvain)$name  # choose canonical order

mem_infomap_morning <- membership(infomap_morning)[nodes]

mem_infomap_afternoon <- membership(infomap_afternoon)[nodes]

mem_infomap_evening <- membership(infomap_evening)[nodes]

### Now compare them according to Adjusted Rand Index (ARI), 
###Normalized Mutual Information (NMI) and Variation of Information (VI)

# ARI
ari_me <- compare(mem_infomap_morning, mem_infomap_evening, method = "adjusted.rand")

# NMI
nmi_me <- compare(mem_infomap_morning, mem_infomap_evening, method = "nmi")

# VI
vi_me  <- compare(mem_infomap_morning, mem_infomap_evening, method = "vi")


### Comparing Louvain and Infomap along the same criteria on the same specifications
### Morning data
ari_louvain_infomap_morning <- compare(mem_louvain_morning, mem_infomap_morning, method = "adjusted.rand")
nmi_louvain_infomap_morning <- compare(mem_louvain_morning, mem_infomap_morning, method = "nmi")
vi_louvain_infomap_morning  <- compare(mem_louvain_morning, mem_infomap_morning, method = "vi")

### Evening data
ari_louvain_infomap_evening <- compare(mem_louvain_evening, mem_infomap_evening, method = "adjusted.rand")
nmi_louvain_infomap_evening <- compare(mem_louvain_evening, mem_infomap_evening, method = "nmi")
vi_louvain_infomap_evening  <- compare(mem_louvain_evening, mem_infomap_evening, method = "vi")

### Weekday data
ari_louvain_infomap_evening <- compare(mem_louvain_evening, mem_infomap_evening, method = "adjusted.rand")
nmi_louvain_infomap_evening <- compare(mem_louvain_evening, mem_infomap_evening, method = "nmi")
vi_louvain_infomap_evening  <- compare(mem_louvain_evening, mem_infomap_evening, method = "vi")




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


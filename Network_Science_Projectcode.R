
setwd("C:/Users/User/OneDrive - Corvinus University of Budapest/Dokumentumok/Coding and shi")

# all this shit is needed
library(tidyverse)
library(sf)
library(data.table)
library(janitor)
library(igraph)
library(dplyr)

# getting the data
tripdata <- fread("202501-citibike-tripdata_1.csv")
tripdata <- janitor::clean_names(tripdata)

# Start stations converted to sf points
start_sf <- tripdata %>%
  select(start_station_id, start_lat, start_lng) %>%
  distinct() %>%
  drop_na(start_lat, start_lng) %>%    # <-- remove broken stations
  st_as_sf(coords = c("start_lng", "start_lat"), crs = 4326)

# end stations converted to sf points
end_sf <- tripdata %>%
  select(end_station_id, end_lat, end_lng) %>%
  distinct() %>%
  drop_na(end_lat, end_lng) %>%        # <-- remove broken stations
  st_as_sf(coords = c("end_lng", "end_lat"), crs = 4326)

#--------------------------------
# getting the community districts
#--------------------------------
#(important, must get the shape (.shp)file and not csv or other shit)
cd <- st_read("NYC_Community_Districts.shp") %>%
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


cd <- st_read("NYC_Community_Districts.shp") %>%
  st_transform(4326) %>%
  mutate(
    borough  = floor(BoroCD / 100),
    district = BoroCD %% 100
  ) %>%
  filter(district < 60)          # keep only real land CDs

#-------------------------------------------------------------
# spatial join to get the community district of start stations

start_join <- st_join(start_sf, cd["BoroCD"], left = TRUE)

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

#lets try to get a district level bikeflow network

district_network <- tripdata_cd %>%
  filter(!is.na(start_BoroCD), !is.na(end_BoroCD)) %>%
  count(start_BoroCD, end_BoroCD, name = "trips")
#-------------------------------------------------

# FLOW NETWORK GRAPH

library(igraph)
library(ggraph)
library(tidygraph)


# Convert to graph
g <- graph_from_data_frame(district_network, directed = TRUE)

# Plot using ggraph
ggraph(g, layout = "fr") + 
  geom_edge_link(aes(width = trips, alpha = trips),
                 arrow = arrow(length = unit(3, 'mm')), end_cap = circle(3, 'mm'),
                 colour = "steelblue") +
  geom_node_point(size = 4, color = "black") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_edge_width(range = c(0.3, 3)) +
  theme_minimal() +
  labs(title = "NYC District Bike Flow Network",
       edge_width = "Trips")

# holy fucking shit it worked...or at least i got something XD
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


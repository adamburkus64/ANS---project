

################################################################################
##                       NETWORK SCIENCE PROJECTCODE                          ##
##                               NYC CITYBIKE                                 ##
################################################################################
# UJVÁRI BOTOND, GARAS FÁBIÁN, NAGY ZALÁN ZSOLT, BURKUS ÁDÁM, SZABÓ LEVENTE   ##
################################################################################

getwd()
setwd("C:/Users/User/OneDrive - Corvinus University of Budapest/Dokumentumok/Coding and shi")
setwd("/Users/ujvaribotond/Desktop/R/Applied_Network_Science/Project_work/202510-citibike-tripdata")
setwd("/Users/ujvaribotond/Desktop/R/Applied_Network_Science/Project_work")



##### PACKAGES NEEDED #####
library(tidyverse)
library(sf)
library(data.table)
library(janitor)
library(igraph)
library(dplyr)
library(ggraph)
library(poweRlaw)
library(geosphere)
library(ggplot2)
library(scales)

### If we want to use every data for the selected month (this is for the latest, 2025 October)
tripdata1 <- fread("202510-citibike-tripdata_1.csv")
tripdata2 <- fread("202510-citibike-tripdata_2.csv")
tripdata3 <- fread("202510-citibike-tripdata_3.csv")
tripdata4 <- fread("202510-citibike-tripdata_4.csv")
tripdata5 <- fread("202510-citibike-tripdata_5.csv")
tripdata <- rbind(tripdata1, tripdata2, tripdata3, tripdata4, tripdata5)

tripdata <- janitor::clean_names(tripdata)

rm(tripdata1, tripdata2, tripdata3, tripdata4, tripdata5)


## Start stations converted to sf points
start_sf <- tripdata %>%
  select(start_station_id, start_lat, start_lng) %>%
  distinct() %>%
  drop_na(start_lat, start_lng) %>%    # <-- remove broken stations
  st_as_sf(coords = c("start_lng", "start_lat"), crs = 4326) 
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

### There are duplicates in this
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
only_start

only_end <- setdiff(ends, starts)
only_end
################################################################################
#### There are more starter stations than ending stations (this can be because we are using only part of the monthly data)
#### But still keep it in our mind when trying it with a whole month for example


#--------------------------------
# getting the community districts
#--------------------------------

cd <- st_read("NYC_Community_Districts.shp") %>%
  st_transform(4326)


# community district identifier is the left column, the number meanings:
# 1 - Manhattan
# 2 - Bronx
# 3 - Brooklyn
# 4 - Queens
# 5 - Staten Island (but there are no bike stations here)
# the numbers after that are the district numbers within the boroughs 
# See all the community districts here: https://boundaries.beta.nyc/?map=cd


cd <- st_read("NYC_Community_Districts.shp") %>%
  st_transform(4326) %>%
  mutate(
    borough  = floor(BoroCD / 100),
    district = BoroCD %% 100
  )           # keep only real land CDs

#-------------------------------------------------------------
# spatial join to get the community district of start stations

start_join <- st_join(start_sf, cd["BoroCD"], left = TRUE)


# spatial join to get the community district of end stations


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

################################################################################


## District level bikeflow network

make_district_network_edgelist <- function(data) {
  data %>%
    filter(!is.na(start_BoroCD) & !is.na(end_BoroCD)) %>%
    count(start_BoroCD, end_BoroCD, name = "weight")
}

### Freeing up the memory from unused stuff
gc()


### Do every edgelist with weights for all the different specifications
### Weekdays vs. weekends
weekday_edgelist <- make_district_network_edgelist(tripdata_cd_weekday)

weekend_edgelist <- make_district_network_edgelist(tripdata_cd_weekend)



### Mornings vs. evenings

morning_edgelist <- make_district_network_edgelist(tripdata_cd_morning)

afternoon_edgelist <- make_district_network_edgelist(tripdata_cd_afternoon)

evening_edgelist <- make_district_network_edgelist(tripdata_cd_evening)



#############
##### Let's compare the pairs of weighted and directed 
##### networks of morning-evening and weekday-weekend

### Weekday graph
weekday_graph <- graph_from_data_frame(weekday_edgelist, directed = T)

## Making sure that the network is weighted and all the nodes are included
V(weekday_graph)$name
E(weekday_graph)$weight

### Describing the weekday network (in-degree and out-degree strength, PageRank
### betweenness and closeness centrality)

statistics_weekday <- data.frame(
  node = V(weekday_graph)$name,
  weight_in_degree  = strength(weekday_graph,  mode = "in",  weights = E(weekday_graph)$weight),
  weight_out_degree = strength(weekday_graph,  mode = "out", weights = E(weekday_graph)$weight),
  PageRank    = page_rank(weekday_graph, weights = E(weekday_graph)$weight)$vector,
  betweenness   = betweenness(weekday_graph, weights = 1 / E(weekday_graph)$weight),
  closeness_in  = closeness(weekday_graph, mode = "in",  weights = E(weekday_graph)$weight),
  closeness_out = closeness(weekday_graph, mode = "out", weights = E(weekday_graph)$weight)
)

  
### Weekend graph
weekend_graph <- graph_from_data_frame(weekend_edgelist, directed = T)

V(weekend_graph)$name
E(weekend_graph)$weight
?betweenness

statistics_weekend <- data.frame(
  node = V(weekend_graph)$name,
  weight_in_degree  = strength(weekend_graph,  mode = "in",  weights = E(weekend_graph)$weight),
  weight_out_degree = strength(weekend_graph,  mode = "out", weights = E(weekend_graph)$weight),
  PageRank    = page_rank(weekend_graph, weights = E(weekend_graph)$weight)$vector,
  betwenness   = betweenness(weekend_graph, weights = 1 / E(weekend_graph)$weight),
  closeness_in  = closeness(weekend_graph, mode = "in",  weights = E(weekend_graph)$weight),
  closeness_out = closeness(weekend_graph, mode = "out", weights = E(weekend_graph)$weight)
)


##### Comparison of weekday vs. weekend

### Correlation between centrality measures
# PageRank
cor(statistics_weekday$PageRank, statistics_weekend$PageRank)
### 0.98, meaning that the structure of the rides are consistent during weekdays and weekends

# Betweenness
cor(statistics_weekday$betweenness, statistics_weekend$betwenness)
### 0.94, meaning that riders cross stations very similarly during their destinations

# In/Out-degree correlation
cor(statistics_weekday$weight_in_degree, statistics_weekend$weight_in_degree)
cor(statistics_weekday$weight_out_degree, statistics_weekend$weight_out_degree)
### 0.97 for both


###

### Comparison on the network level
# Density
edge_density(weekday_graph)
### 0.791 there are more edges during the weekday than the weekend
edge_density(weekend_graph)
### 0.74


# Reciprocity
reciprocity(weekday_graph, ignore.loops = F)
### 0.94
reciprocity(weekend_graph, ignore.loops = F)
### 0.93

### There is no directional bias, almost all the edges are used back and forth

# Assortativity
assortativity_degree(weekday_graph)
assortativity_degree(weekend_graph)
### Very close to 0

### Ratio of self-loops
loops_weekday <- sum(E(weekday_graph)$weight[which_loop(weekday_graph)])
loops_weekend <- sum(E(weekend_graph)$weight[which_loop(weekend_graph)])

loop_ratio_weekday <- loops_weekday / sum(E(weekday_graph)$weight)
loop_ratio_weekend <- loops_weekend / sum(E(weekend_graph)$weight)

loop_ratio_weekday
loop_ratio_weekend
### The ratio of self-loops (indicating in-district rides) are both around 0.38


## within the same distric bike flow
intra_weekday <- weekday_edgelist[start_BoroCD == end_BoroCD, sum(weight)]
### 1373764 rides within districts during the weekdays
intra_weekend <- weekend_edgelist[start_BoroCD == end_BoroCD, sum(weight)]
### 435651 rides within the districts during the weekends

#### Within-district rides are 3 times more frequent compared to weekends
#### (still higher when accounting for the difference in days)


## cross district bikeflow
inter_weekday <- weekday_edgelist[start_BoroCD != end_BoroCD, sum(weight)]
### 2192250 rides across districts during weekdays
inter_weekend <- weekend_edgelist[start_BoroCD != end_BoroCD, sum(weight)]
### 726707 rides across districts during weekends

#### Cross-district rides are around 3 times more frequent compared to weekends
#### (still higher when accounting for the difference in days)


#### <- Higher per-day average rides for the weekdays

## share of trips that stay inside the same district
share_intra_weekday <- intra_weekday / (intra_weekday + inter_weekday)
### around 38.5% of rides during the weekdays were within-district rides

share_intra_weekend <- intra_weekend / (intra_weekend + inter_weekend)
### around 37.4% of rides during the weekends were within-district rides

### Ratio of rides within districts don't change fo weekdays and weekends

## strongest inter district flows
top_inter_weekday <- weekday_edgelist[start_BoroCD != end_BoroCD][order(-weight)][1:15]
top_inter_weekend <- weekend_edgelist[start_BoroCD != end_BoroCD][order(-weight)][1:15]

#### Highest inter-district flows are between the central districts of Manhattan (102,103,104,105...)




### Morning vs. evening
### Weekday graph
morning_graph <- graph_from_data_frame(morning_edgelist, directed = T)

## Making sure that the network is weighted and all the nodes are included
V(morning_graph)$name
E(morning_graph)$weight

### Describing the weekday network (in-degree and out-degree strength, PageRank
### betweenness and closeness centrality)

statistics_morning <- data.frame(
  node = V(morning_graph)$name,
  weight_in_degree  = strength(morning_graph,  mode = "in",  weights = E(morning_graph)$weight),
  weight_out_degree = strength(morning_graph,  mode = "out", weights = E(morning_graph)$weight),
  PageRank    = page_rank(morning_graph, weights = E(morning_graph)$weight)$vector,
  betweenness   = betweenness(morning_graph, weights = 1 / E(morning_graph)$weight),
  closeness_in  = closeness(morning_graph, mode = "in",  weights = E(morning_graph)$weight),
  closeness_out = closeness(morning_graph, mode = "out", weights = E(morning_graph)$weight)
)

### Easier way to look at the highest values
top_contenders <- function(statistics, category){
  statistics %>%
    arrange(desc(category)) %>%
    slice_head(n = 10)
}

### Weekend graph
evening_graph <- graph_from_data_frame(evening_edgelist, directed = T)

V(evening_graph)$name
E(evening_graph)$weight
?strength

statistics_evening <- data.frame(
  node = V(evening_graph)$name,
  weight_in_degree  = strength(evening_graph,  mode = "in",  weights = E(evening_graph)$weight),
  weight_out_degree = strength(evening_graph,  mode = "out", weights = E(evening_graph)$weight),
  PageRank    = page_rank(evening_graph, weights = E(evening_graph)$weight)$vector,
  betwenness   = betweenness(evening_graph, weights = 1 / E(evening_graph)$weight),
  closeness_in  = closeness(evening_graph, mode = "in",  weights = E(evening_graph)$weight),
  closeness_out = closeness(evening_graph, mode = "out", weights = E(evening_graph)$weight)
)


##### Comparison of morning vs. evening


### Correlation between centrality measures
# PageRank
cor(statistics_morning$PageRank, statistics_evening$PageRank)
### 0.58, meaning there is some variance between the mobility structures of morning and evening rides

# Betweenness
cor(statistics_morning$betweenness, statistics_evening$betwenness)
### 0.76, showing a rather consistent betweenness measure, so the shortest paths of 
### evenings and mornings include similar districts


# In/Out-degree correlation
cor(statistics_morning$weight_in_degree, statistics_evening$weight_in_degree)
### 0.87 
cor(statistics_morning$weight_out_degree, statistics_evening$weight_out_degree)
### 0.98

### Shows that during the evening and morning, people go from the same place to the same place
### (can make the story of going from home to work in the morning, and from home to nightlife centers in the evening)


### Comparison on the network level
# Density
edge_density(morning_graph)
### 0.635
edge_density(evening_graph)
### 0.734

### People use more routes between stations than in the morning

# Reciprocity
reciprocity(morning_graph, ignore.loops = F)
### 0.887
reciprocity(evening_graph, ignore.loops = F)
### 0.917

### No directional bias (the edges are used in both directions)


# Assortativity
assortativity_degree(morning_graph)
### 0.02
assortativity_degree(evening_graph)
### -0.028

### Very low assortativity (no preference of only similar nodes connecting)

### Ratio of self-loops
loops_morning <- sum(E(morning_graph)$weight[which_loop(morning_graph)])
loops_evening <- sum(E(evening_graph)$weight[which_loop(evening_graph)])

loop_ratio_morning <- loops_morning / sum(E(morning_graph)$weight)
loop_ratio_evening <- loops_evening / sum(E(evening_graph)$weight)

loop_ratio_morning
### 0.383
loop_ratio_evening
### 0.375

### Basically same loops are present

## within the same district bike flow
intra_morning <- morning_edgelist[start_BoroCD == end_BoroCD, sum(weight)]
### 393267 rides within the same districts in the mornings
intra_evening <- evening_edgelist[start_BoroCD == end_BoroCD, sum(weight)]
### 492241 rides within the same districts in the evenings


## cross district bikeflow
inter_morning <- morning_edgelist[start_BoroCD != end_BoroCD, sum(weight)]
### 632280 rides across the same districts in the mornings
inter_evening <- evening_edgelist[start_BoroCD != end_BoroCD, sum(weight)]
### 820035 rides across the same districts in the evenings

## share of trips that stay inside the same district
share_intra_morning <- intra_morning / (intra_morning + inter_morning)
### 0.375
share_intra_evening <- intra_evening / (intra_evening + inter_evening)
### 0.383

### For both times of day, inter-district rides are more frequent compared to in-district rides
### and during evenings, the bikes are more frequently used both for in-district
### and cross-district rides


## strongest inter district flows
top_inter_morning <- morning_edgelist[start_BoroCD != end_BoroCD][order(-weight)][1:15]
top_inter_evening <- evening_edgelist[start_BoroCD != end_BoroCD][order(-weight)][1:15]










### Interpretations:
### high-indegree == Many people arrive here (job-dense areas, rich night-life, like Manhattan)
### high-outdegree == Many people start from here (residential areas, where most people live)
### self-loops == How much people cycle inside the districts (ratio of local cyclers vs. cross-district ones)
### Betweenness == People go through these districts (they don't start or finish here, but cross it, its a central district)
### PageRank == Looks at how central the neighbors are of a give district
### Reciprocity == Low reciprocity means a directional bias (only going from home to work but not vice versa)


### Creating the directed network graphs for the Infomap
do_the_graph_from_edgelist_Infomap <- function(edgelist_with_weights) {
  graph_from_data_frame(edgelist_with_weights, directed = T)
}

### same but in a function for the Louvain (CANNOT be directed)

do_the_graph_from_edgelist_Louvain <- function(edgelist_with_weights) {
  graph_from_data_frame(edgelist_with_weights, directed = F)
}



###   
plot_district_network <- function(g, title, link_color, node_color) {
  ggraph(g, layout = "fr") + 
    geom_edge_link(
      aes(width = weight, alpha = weight),
      end_cap = circle(3, 'mm'),
      colour = link_color
    ) +
    geom_node_point(size = 4, color = node_color) +
    geom_node_text(aes(label = name), repel = TRUE, size = 4) +
    scale_edge_width(range = c(0.3, 3)) +
    theme_minimal() +
    labs(
      title = title,
      edge_width = "weight")
}



########## Shwoing the robustness of Infomap method by implementing Louvain as well
#### Create the undirected graphs for the Louvain method
weekday_graph_Louvain <- do_the_graph_from_edgelist_Louvain(weekday_edgelist)

### Make sure the weights are indeed for the edges
E(weekday_graph_Louvain)$weight


weekend_graph_Louvain <- do_the_graph_from_edgelist_Louvain(weekend_edgelist)

plot_district_network(weekday_graph_Louvain, title = "AAA", link_color = "red", node_color = "blue")
### Make sure the weights are indeed for the edges
E(weekend_graph_Louvain)$weight

#### Run Louvain method on the graphs
set.seed(123)
louvain_weekday <- cluster_louvain(weekday_graph_Louvain, weights = E(weekday_graph_Louvain)$weight)
louvain_weekend <- cluster_louvain(weekend_graph_Louvain, weights = E(weekend_graph_Louvain)$weight)

#### Extract memberships
nodes <- V(weekday_graph_Louvain)$name  # choose canonical order

mem_louvain_weekday <- membership(louvain_weekday)[nodes]
mem_louvain_weekend <- membership(louvain_weekend)[nodes]

### Now compare them according to Adjusted Rand Index (ARI), 
### Normalized Mutual Information (NMI) and Variation of Information (VI)

# ARI (between 0 and 1, with 1 meaning that the communities are identical)
ari_me_week_louvain <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "adjusted.rand")
### After including central park
## 0.7724


# NMI (between 0 and 1, with 1 meaning that the communities are identical)
nmi_me_week_louvain <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "nmi")
### After including central park
## 0.8936

# VI (between 0 and 1, showing the variance in information between the 2 memberships, if 0=identical communities)
vi_me_week_louvain  <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "vi")
### After including central park
## 0.3429

### Shows some variation between the communities of weekday and weekend



### Memberships (6 communities)
### We observe some variance between the communities of weekdays and weekends 
mem_louvain_weekday
mem_louvain_weekend
### Bronx (2XX), Brooklyn (3XX) and Queens (4XX) are very clearly overlapped by the bike station communities for both
### Weekday Manhattan: Financial District (101,102,103) create their own community (for obvious reasons, work)
### Midtown and Hell's Kitchen area (104, 105, 106) are their own community as well
### From Central Park to the Bronx, it is considered one community
### Weekend Manhattan: South of Central Park is its own community and to the North + Bronx is another
### Apart from these, communities are very similar to the borough structure
### Community 4 creates its own little cluster in Central Brooklyn (301, 304, 405) (FIND SOME EXPLANATION FOR THIS!!!)



#### Modularity comparison
modularity_louvain_weekday <- modularity(weekday_graph_Louvain, mem_louvain_weekday, 
                                         weights = E(weekday_graph_Louvain)$weight)
## 0.517

modularity_louvain_weekend <- modularity(weekend_graph_Louvain, mem_louvain_weekend, 
                                         weights = E(weekend_graph_Louvain)$weight)

## 0.5104

## Modular graph with positive modularity, optimized structure, which can be a good 
## explanation to why it overlaps the community districts so much 


#### Mornings vs. evenings (optionally vs.afternoon)
morning_edgelist <- make_district_network_edgelist(tripdata_cd_morning)

afternoon_edgelist <- make_district_network_edgelist(tripdata_cd_afternoon)

evening_edgelist <- make_district_network_edgelist(tripdata_cd_evening)


#### Create the undirected graphs for the Louvain method
morning_graph_Louvain <- do_the_graph_from_edgelist_Louvain(morning_edgelist)
E(morning_graph_Louvain)$weight



afternoon_graph_Louvain <- do_the_graph_from_edgelist_Louvain(afternoon_edgelist)
E(afternoon_graph_Louvain)$weight


evening_graph_Louvain <- do_the_graph_from_edgelist_Louvain(evening_edgelist)
E(evening_graph_Louvain)$weight

plot_district_network(morning_graph_Louvain, title = "AAA", link_color = "red", node_color = "blue")


#### Run Louvain method on the graphs
set.seed(123)
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
ari_me_day_louvain <- compare(mem_louvain_morning, mem_louvain_evening, method = "adjusted.rand")
### After including central park
## 0.9731

# NMI
nmi_me_day_louvain <- compare(mem_louvain_morning, mem_louvain_evening, method = "nmi")
### After including central park
## 0.9675

# VI
vi_me_day_louvain  <- compare(mem_louvain_morning, mem_louvain_evening, method = "vi")
### After including central park
## 0.0967

### Shows almost identical communities between morning and evening 

### Memberships (6 communities)

mem_louvain_morning
mem_louvain_evening
## In the morning, it is very similar to the weekday specification (Financial District,
### Hell's Kitchen and Chinatown, North Manhattan closely connected to the whole of Bronx)
### In the Evening South of Central Park, then North of Central Park + Bronx, then
### each borough being its own cluster with Central Brooklyn being its own little island

#### Modularity comparison
modularity_louvain_morning <- modularity(morning_graph_Louvain, mem_louvain_morning, 
                                         weights = E(morning_graph_Louvain)$weight)
### 0.5172

modularity_louvain_afternoon <- modularity(afternoon_graph_Louvain, mem_louvain_afternoon, 
                                           weights = E(afternoon_graph_Louvain)$weight)
### 


modularity_louvain_evening <- modularity(evening_graph_Louvain, mem_louvain_evening, 
                                         weights = E(evening_graph_Louvain)$weight)

### 0.5055

## Modular graph with positive modularity, optimized structure, which can be a good 
## explanation to why it overlaps the community districts so much 





#### INFOMAP ####
### Weekdays vs. weekends

#### Create the directed graph for the Infomap method
weekday_graph_Infomap <- do_the_graph_from_edgelist_Infomap(weekday_edgelist)

weekend_graph_Infomap <- do_the_graph_from_edgelist_Infomap(weekend_edgelist)

#### Run Infomap method on the graphs
infomap_weekday <- cluster_infomap(weekday_graph_Infomap, e.weights = E(weekday_graph_Infomap)$weight)
infomap_weekend <- cluster_infomap(weekend_graph_Infomap, e.weights = E(weekend_graph_Infomap)$weight)

#### Extract memberships
nodes <- V(weekday_graph_Infomap)$name  # choose canonical order

mem_infomap_weekday <- membership(infomap_weekday)[nodes]
mem_infomap_weekend <- membership(infomap_weekend)[nodes]

### Now compare them according to Adjusted Rand Index (ARI), 
###Normalized Mutual Information (NMI) and Variation of Information (VI)

# ARI
ari_me_week_info <- compare(mem_infomap_weekday, mem_infomap_weekend, method = "adjusted.rand")
### After including central park
## 0.8393

# NMI
nmi_me_week_info <- compare(mem_infomap_weekday, mem_infomap_weekend, method = "nmi")
### After including central park
## 0.8778


# VI
vi_me_week_info  <- compare(mem_infomap_weekday, mem_infomap_weekend, method = "vi")
### After including central park
## 0.3202

### Again, there is some variation between the community

### Memberships (only 4 communities this time)

mem_infomap_weekday
mem_infomap_weekend

### For the weekday, due to workplace travels into Manhattan (in this case South of Central Park, including Central Park), 
### there is 1 community, then those who work in North Manhattan but live in the Bronx create another community,
### with the 3rd and 4th community directly overlapping Brooklyn and Queens (with 405 being a more central district for Brooklyn and Queens
### in this case belonging to the community of Brooklyn)
### For the weekend, Almost the whole of Manhattan creates its own community, except for the 
### directly adjacent districts to the Bronx, this can indicate a less workplace oriented
### travel pattern (e.g. Central Park becomes a more popular destination)
### and riders remaining largely inside their boroughs, not travelling far 

#### Mornings vs. evenings

#### Create the directed graphs for the Infomap method
morning_graph_Infomap <- do_the_graph_from_edgelist_Infomap(morning_edgelist)

afternoon_graph_Infomap <- do_the_graph_from_edgelist_Infomap(afternoon_edgelist)

evening_graph_Infomap <- do_the_graph_from_edgelist_Infomap(evening_edgelist)

#### Run Infomap method on the graphs
infomap_morning <- cluster_infomap(morning_graph_Infomap, e.weights = E(morning_graph_Infomap)$weight)

infomap_afternoon <- cluster_infomap(afternoon_graph_Infomap, e.weights = E(afternoon_graph_Infomap)$weight)

infomap_evening <- cluster_infomap(evening_graph_Infomap, e.weights = E(evening_graph_Infomap)$weight)


#### Extract memberships
nodes <- V(morning_graph_Infomap)$name  # choose canonical order

mem_infomap_morning <- membership(infomap_morning)[nodes]

mem_infomap_afternoon <- membership(infomap_afternoon)[nodes]

mem_infomap_evening <- membership(infomap_evening)[nodes]

### Now compare them according to Adjusted Rand Index (ARI), 
###Normalized Mutual Information (NMI) and Variation of Information (VI)
set.seed(123)


# ARI
ari_me_day_info <- compare(mem_infomap_morning, mem_infomap_evening, method = "adjusted.rand")
### After including central park
## 0.8315


# NMI
nmi_me_day_info <- compare(mem_infomap_morning, mem_infomap_evening, method = "nmi")
### After including central park
## 0.8602


# VI
vi_me_day_info  <- compare(mem_infomap_morning, mem_infomap_evening, method = "vi")
### After including central park
## 0.3933


### Again, there is variation, we can see that the communities differ in some places

### Memberships
mem_infomap_morning
mem_infomap_evening

### Morning (4 communities) communities are the exact same layout as for weekdays, which makes sense
### (work)
### For the evening (5 communities), Manhattan is divided into 2 communities, one for South Manhattan
### and another including Central Park and the North of Manhattan 
### The Bronx has its own community (including the most Northern Manhattan district, 112)
### and the scheme is the same for Brooklyn as Queens as before
### This shows that during the evenings, everyone tends to stay close in their boroughs

### are the networks scale free

# 1. compute degrees
deg_morning <- degree(morning_graph, mode = "all")

# 2. remove zero-degree nodes (power-law cannot fit them)
deg_morning <- deg_morning[deg_morning > 0]

# 3. fit power-law
fit_morning <- power.law.fit(deg_morning)

fit_morning

# 1. compute degrees
deg_evening <- degree(evening_graph, mode = "all")

# 2. remove zero-degree nodes (power-law cannot fit them)
deg_evening <- deg_evening[deg_evening > 0]

# 3. fit power-law
fit_evening <- power.law.fit(deg_evening)

# 4. print results
fit_evening


# 1. compute degrees
deg_weekday <- degree(weekday_graph, mode = "all")

# 2. remove zero-degree nodes
deg_weekday <- deg_weekday[deg_weekday > 0]

# 3. fit power-law
fit_weekday <- power.law.fit(deg_weekday)

# 4. print results
fit_weekday



# 1. compute degrees
deg_weekend <- degree(weekend_graph, mode = "all")

# 2. remove zero-degree nodes
deg_weekend <- deg_weekend[deg_weekend > 0]

# 3. fit power-law
fit_weekend <- power.law.fit(deg_weekend)

# 4. print results
fit_weekend


###plot for it if it is not a straight line then it is not scale free

hist(degree(morning_graph), breaks = 50, freq = TRUE,
     main = "Morning Degree Distribution (log-log)",
     xlab = "Degree", ylab = "Count")

plot(tabulate(degree(morning_graph)), log = "xy",
     type = "p", col = "blue")
## confirmed: not scale free

## Power law vs lognormal vs truncated power law

test_distribution <- function(g) {
  deg <- degree(g, mode = "all")
  deg <- deg[deg > 0]
  
  list(
    powerlaw        = power.law.fit(deg),
    lognormal       = fitdistrplus::fitdist(deg, "lnorm"),
    exponential     = fitdistrplus::fitdist(deg, "exp"),
    truncated_pl    = poweRlaw::displ$new(deg)
  )
}

res_morning <- test_distribution(morning_graph)
res_evening <- test_distribution(evening_graph)
res_weekday <- test_distribution(weekday_graph)
res_weekend <- test_distribution(weekend_graph)

res_morning

## alpha is absurdly high--> not scale free network

deg <- degree(morning_graph)
deg <- deg[deg > 0]
m_pl  <- displ$new(deg)
m_ln  <- dislnorm$new(deg)

# likelihood ratio: power-law vs log-normal
compare_distributions(m_pl, m_ln)


##log normal far better than powerlaw--> this is good
## standard for transportation/mobility/flow networks

### small world test

smallworld_test <- function(g) {
  g <- as.undirected(g)
  
  C  <- transitivity(g, type = "global")
  L  <- mean_distance(g, directed = FALSE)
  
  # Random graph with same number of nodes & edges
  gr <- sample_gnm(vcount(g), ecount(g))
  Cr <- transitivity(gr, type = "global")
  Lr <- mean_distance(gr, directed = FALSE)
  
  sigma <- (C / Cr) / (L / Lr)
  
  list(C = C, L = L, Cr = Cr, Lr = Lr, sigma = sigma)
}

smallworld_test(morning_graph)
smallworld_test(evening_graph)
smallworld_test(weekday_graph)
smallworld_test(weekend_graph)
### none of them are small world networks

### they are spatial flow networks
## possible types 
# Spatial interaction networks
# Gravity-like mobility networks
# Log-normal degree + spatial structure

centroids <- cd %>%
  st_centroid() %>%              # compute geometry-based centroid
  mutate(
    lon = st_coordinates(.)[,1], # extract longitude
    lat = st_coordinates(.)[,2]  # extract latitude
  ) %>%
  st_drop_geometry() %>%         # remove geometry now that coords are stored
  select(BoroCD, lon, lat)


# convert graph edges into dataframe
edge_df <- as.data.frame(as_edgelist(morning_graph)) %>%
  rename(from = V1, to = V2)

centroids$BoroCD <- as.character(centroids$BoroCD)
# attach coordinates for start and end nodes
edge_df <- edge_df %>%
  left_join(centroids, by = c("from" = "BoroCD")) %>%
  rename(lon_from = lon, lat_from = lat) %>%
  left_join(centroids, by = c("to" = "BoroCD")) %>%
  rename(lon_to = lon, lat_to = lat)

# spatial distances
edge_df$distance_km <- geosphere::distHaversine(
  edge_df[,c("lon_from", "lat_from")],
  edge_df[,c("lon_to", "lat_to")]
) / 1000


## spatial correlation with flow weights

spatial_corr <- cor(
  edge_df$distance_km,
  E(morning_graph)$weight,
  method = "spearman"
)

spatial_corr

## -0.7589
## as distance between districts increases the the flow of commuters decrease
## as expected



#################### Graphs #############

districts <- st_read("NYC_Community_Districts.shp", quiet = TRUE)

# Ensure a numeric BoroCD exists
districts <- districts %>%
  mutate(BoroCD = as.numeric(BoroCD))

# Compute borough code: floor(BoroCD / 100) (1=Manhattan,2=Bronx,3=Brooklyn,4=Queens,5=Staten Island)
districts <- districts %>%
  mutate(Boro = floor(BoroCD / 100))

# Convert polygons to WGS84 for plotting/centroids
districts_wgs <- st_transform(districts, 4326)

# Aggregate CDs into borough polygons (dissolve by Boro)
boroughs <- districts_wgs %>%
  group_by(Boro) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  arrange(Boro)

# Add readable borough names (optional)
borough_names <- tibble(
  Boro = 1:5,
  BoroughName = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")
)
boroughs <- boroughs %>% left_join(borough_names, by = "Boro")

# Compute centroids for arrow endpoints (use st_point_on_surface for robust interior points)
boro_pts <- st_point_on_surface(boroughs) %>% 
  st_centroid() %>%                # safe interior-centroids for labels/arrows
  st_transform(4326)
boro_coords <- st_coordinates(boro_pts) %>% as.data.frame() %>%
  bind_cols(Boro = boroughs$Boro, BoroughName = boroughs$BoroughName)

# ---------- 2. Compute borough-level flows ----------
# helper to aggregate an edgelist to borough pairs
agg_to_borough <- function(edgelist) {
  edgelist %>%
    # derive borough codes
    mutate(
      start_boro = floor(start_BoroCD / 100),
      end_boro   = floor(end_BoroCD / 100)
    ) %>%
    group_by(start_boro, end_boro) %>%
    summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop")
}

morning_boro <- agg_to_borough(morning_edgelist) %>% mutate(type = "Morning")
evening_boro <- agg_to_borough(evening_edgelist) %>% mutate(type = "Evening")

flows_boro <- bind_rows(morning_boro, evening_boro)

# Option: remove intra-borough flows (start == end) to focus on between-borough movement
flows_boro_inter <- flows_boro %>% filter(start_boro != end_boro)

# ---------- 3. Join coords (centroid X/Y) for plotting ----------
flows_boro_xy <- flows_boro_inter %>%
  left_join(boro_coords %>% select(Boro, X, Y), by = c("start_boro" = "Boro")) %>%
  rename(x_start = X, y_start = Y) %>%
  left_join(boro_coords %>% select(Boro, X, Y), by = c("end_boro" = "Boro")) %>%
  rename(x_end = X, y_end = Y)



# create a consistent borough name mapping
boro_names <- tibble(
  Boro = 1:5,
  BoroughName = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"),
  dest_col = c("#e41a1c", "#377eb8", "#ff7f00", "#4daf4a", "#984ea3") # red, blue, orange, green, purple
)


# join dest borough name & color
flows_boro_xy <- flows_boro_xy %>%
  left_join(boro_names, by = c("end_boro" = "Boro")) %>%
  rename(dest_boro_name = BoroughName, dest_color = dest_col)

# --- 1. Create discrete width categories for readability (you asked for levels)
# Thresholds chosen: < 1k, 1k-10k, 10k-100k, >100k (adjust if your data differs)
flows_boro_xy <- flows_boro_xy %>%
  mutate(
    weight_cat = cut(weight,
                     breaks = c(-Inf, 1000, 10000, 100000, Inf),
                     labels = c("< 1k", "1k - 10k", "10k - 100k", "> 100k"),
                     right = TRUE),
    # curvature sign: to separate opposite-direction pairs visually
    curvature = ifelse(start_boro < end_boro, 0.25, -0.25),
    # partial alpha so thick lines don't hide arrowheads - you can lower if needed
    alpha_val = case_when(
      weight_cat == "< 1k" ~ 0.7,
      weight_cat == "1k - 10k" ~ 0.85,
      weight_cat == "10k - 100k" ~ 0.95,
      weight_cat == "> 100k" ~ 0.95,
      TRUE ~ 0.8
    )
  )

# --- 2. Map the discrete categories to visual sizes (line widths)
size_map <- c("< 1k" = 0.6, "1k - 10k" = 1.2, "10k - 100k" = 2.6, "> 100k" = 5)

# --- 3. Plot: facet Morning / Evening, color by destination borough, size by weight category
p <- ggplot() +
  # base borough polygons (if you have boroughs sf)
  geom_sf(data = boroughs, fill = "grey95", color = "white", size = 0.3) +
  
  # draw a light shadow layer first (slightly thicker, grey) to give contrast and show direction
  geom_curve(
    data = flows_boro_xy,
    aes(
      x = x_start, y = y_start, xend = x_end, yend = y_end,
      curvature = curvature
    ),
    color = "grey80",
    size = 1.2,
    alpha = 0.35,
    lineend = "round"
  ) +
  
  # main colored arrows: size from weight_cat, color from destination, arrowheads visible
  geom_curve(
    data = flows_boro_xy,
    aes(
      x = x_start, y = y_start, xend = x_end, yend = y_end,
      color = dest_boro_name,
      size = weight_cat,
      alpha = alpha_val,
      curvature = curvature
    ),
    arrow = arrow(length = unit(0.28, "cm"), type = "closed"),
    lineend = "round",
    show.legend = TRUE
  ) +
  
  # borough labels (centroids)
  geom_text(data = boro_coords, aes(x = X, y = Y, label = BoroughName),
            size = 3.6, fontface = "bold", colour = "black") +
  
  # facet by morning / evening for side-by-side comparison
  facet_wrap(~type, nrow = 1) +
  
  # manual color: destination boroughs
  scale_color_manual(
    name = "Destination borough",
    values = setNames(boro_names$dest_col, boro_names$BoroughName),
    na.value = "grey50"
  ) +
  
  # map the categorical sizes to numeric linewidths
  scale_size_manual(
    name = "Trips (category)",
    values = size_map
  ) +
  
  # alpha guide suppressed — we use it just for visual effect
  guides(alpha = "none") +
  
  labs(
    title = "Borough-level Bike Flows — Morning vs Evening",
    subtitle = "Arrow color = destination borough. Arrow width ≈ number of trips (categories). Arrowheads show direction.",
    caption = "Widths: <1k | 1k–10k | 10k–100k | >100k trips (per period)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_line(color = "transparent"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  coord_sf()

# print the plot and save if you want
print(p)
ggsave("boro_flows_panel.png", plot = p, width = 14, height = 7, dpi = 300)


################ FINAL ####################
###########################################

# Improved, final tweaks: morning-left, adaptive width bins, color for all flows, zoom to exclude Staten Island.

# --- ensure destination color/name mapping exists ---
boro_names <- tibble(
  Boro = 1:5,
  BoroughName = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"),
  dest_col = c("#e41a1c", "#377eb8", "#ff7f00", "#4daf4a", "#984ea3") # red, blue, orange, green, purple
)

flows_boro_xy <- flows_boro_xy %>%
  left_join(boro_names, by = c("end_boro" = "Boro")) %>%
  rename(dest_boro_name = BoroughName, dest_color = dest_col)

# --- 1) Adaptive width categories based on quantiles (robust to scale)
# compute quantiles (exclude zeros)
w <- flows_boro_xy$weight
q <- as.numeric(quantile(w[w>0], probs = c(0.25, 0.5, 0.75), na.rm = TRUE))

# Build breaks: (-Inf, q25, q50, q75, +Inf). Labels show rounded ranges.
breaks <- c(-Inf, q[1], q[2], q[3], Inf)
labels <- c(paste0("< ", scales::comma(round(q[1]))),
            paste0(scales::comma(round(q[1])), " - ", scales::comma(round(q[2]))),
            paste0(scales::comma(round(q[2])), " - ", scales::comma(round(q[3]))),
            paste0("> ", scales::comma(round(q[3]))))

flows_boro_xy <- flows_boro_xy %>%
  mutate(
    weight_cat = cut(weight, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE),
    curvature = ifelse(start_boro < end_boro, 0.25, -0.25),
    alpha_val = case_when(
      weight_cat == labels[1] ~ 0.7,
      weight_cat == labels[2] ~ 0.85,
      weight_cat == labels[3] ~ 0.95,
      weight_cat == labels[4] ~ 0.98,
      TRUE ~ 0.85
    )
  )

# Map visually pleasing linewidths to these quantile buckets:
size_map <- setNames(c(0.6, 1.2, 2.4, 5), labels)

# Computing a bbox that excludes Staten Island (Boro == 5) for a tighter zoom
# Use boroughs sf object from earlier and drop Staten Island
boro_zoom <- boroughs %>% filter(Boro != 5)
# compute bbox and expand slightly
bb <- st_bbox(boro_zoom)
xpad <- (bb$xmax - bb$xmin) * 0.06
ypad <- (bb$ymax - bb$ymin) * 0.06
xlim <- c(bb$xmin - xpad, bb$xmax + xpad)
ylim <- c(bb$ymin - ypad, bb$ymax + ypad)

# --- 3) Plot: morning left, evening right; color = destination borough; size = category
p_final <- ggplot() +
  geom_sf(data = boroughs, fill = "grey96", color = "white", size = 0.25) +
  
  # faint shadow layer (thin, grey) for contrast so arrow shapes remain visible
  geom_curve(
    data = flows_boro_xy,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end, curvature = curvature),
    color = "grey85",
    size = 0.8,
    alpha = 0.35,
    lineend = "round"
  ) +
  
  # colored arrows for every flow (color by destination borough)
  geom_curve(
    data = flows_boro_xy,
    aes(
      x = x_start, y = y_start, xend = x_end, yend = y_end,
      color = dest_boro_name,
      size = weight_cat,
      alpha = alpha_val,
      curvature = curvature
    ),
    arrow = arrow(length = unit(0.26, "cm"), type = "closed"),
    lineend = "round",
    show.legend = TRUE
  ) +
  
  # borough labels
  geom_text(data = boro_coords %>% filter(BoroughName != "Staten Island"),
            aes(x = X, y = Y, label = BoroughName),
            size = 3.6, fontface = "bold", color = "black") +
  
  # facet Morning (left) / Evening (right) in desired order
  scale_x_continuous() + scale_y_continuous() +
  facet_wrap(~factor(type, levels = c("Morning", "Evening")), nrow = 1) +
  
  # color palette mapped to destination borough names
  scale_color_manual(
    name = "Destination borough",
    values = setNames(boro_names$dest_col, boro_names$BoroughName),
    na.value = "grey50"
  ) +
  
  # size mapping from categorical buckets defined above
  scale_size_manual(name = "Trips (category)", values = size_map) +
  
  guides(alpha = "none") +
  
  labs(
    title = "Borough-level Bike Flows — Morning vs Evening",
    subtitle = "Arrow colour = destination borough. Arrow width ≈ trips (quantile categories). Arrowheads show direction.",
    caption = paste0(
      "Width categories (quantiles): ",
      labels[1], " | ", labels[2], " | ",
      labels[3], " | ", labels[4],
      " (computed from the selected period's flows)"
    )
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.spacing = unit(2, "lines")
  ) +
  
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)

# render and save
print(p_final)
#ggsave("boro_flows_panel_final.png", p_final, width = 14, height = 7, dpi = 300)


### memberships
mem_infomap_weekday
mem_infomap_weekend

mem_infomap_morning
mem_infomap_evening


### Adding the memberships to the graph
V(weekday_graph_Infomap)$Infomap <- membership(infomap_weekday)

V(weekend_graph_Infomap)$Infomap <- membership(infomap_weekend)



V(morning_graph_Infomap)$Infomap <- membership(infomap_morning)

V(evening_graph_Infomap)$Infomap <- membership(infomap_evening)



### Preparing the graph, matching the number of districts that actually have bike stations
### with the shape file
districts <- st_read("NYC_Community_Districts.shp")

districts <- districts %>%
  mutate(BoroCD = as.numeric(BoroCD))


coords <- districts %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  bind_cols(BoroCD = districts$BoroCD)

## Getting rid of districts that have no bike stations
coords <- coords[-c(67,65,35,25,63,46,22,5,23,49,36,71,29,40,42,1,51,41,52,33,2,32,3,34,12,37,38,31),]

coords <- coords %>%
  arrange(BoroCD)



### Creating the Weekday graph with its communities
stations_sf <- st_as_sf(
  coords,
  coords = c("X", "Y"),
  crs = 4326
) %>% 
  mutate(
    community = V(weekday_graph_Infomap)$Infomap
  )

stations_with_district <- st_join(stations_sf, cd)


district_communities <- stations_with_district %>% 
  st_drop_geometry() %>%
  group_by(BoroCD.x) %>% 
  summarise(community = names(which.max(table(community))))

colnames(district_communities)[1] <- "BoroCD"

nyc_districts_comm <- cd %>%
  left_join(district_communities, by = "BoroCD")

nyc_districts_comm <- nyc_districts_comm %>% filter(!is.na(community))


ggplot(nyc_districts_comm) +
  geom_sf(aes(fill = as.factor(community)), color = "black",
          linewidth = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_family = "Times New Roman") +
  labs(fill = "Community",
       title = "New York Districts by Network Community",
       subtitle = "Infomap Community Detection Method on Weekdays") +
  theme(
    # Center title and subtitle
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12))





### Creating the Weekend graph with its communities
stations_sf <- st_as_sf(
  coords,
  coords = c("X", "Y"),
  crs = 4326
) %>% 
  mutate(
    community = V(weekend_graph_Infomap)$Infomap
  )

stations_with_district <- st_join(stations_sf, cd)


district_communities <- stations_with_district %>% 
  st_drop_geometry() %>%
  group_by(BoroCD.x) %>% 
  summarise(community = names(which.max(table(community))))

colnames(district_communities)[1] <- "BoroCD"

nyc_districts_comm <- cd %>%
  left_join(district_communities, by = "BoroCD")

nyc_districts_comm <- nyc_districts_comm %>% filter(!is.na(community))


ggplot(nyc_districts_comm) +
  geom_sf(aes(fill = as.factor(community)), color = "black",
          linewidth = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_family = "Times New Roman") +
  labs(fill = "Community",
       title = "New York Districts by Network Community",
       subtitle = "Infomap Community Detection Method on Weekend Data") +
  theme(
    # Center title and subtitle
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12))




### Creating the Morning graph with its communities
stations_sf <- st_as_sf(
  coords,
  coords = c("X", "Y"),
  crs = 4326
) %>% 
  mutate(
    community = V(morning_graph_Infomap)$Infomap
  )

stations_with_district <- st_join(stations_sf, cd)


district_communities <- stations_with_district %>% 
  st_drop_geometry() %>%
  group_by(BoroCD.x) %>% 
  summarise(community = names(which.max(table(community))))

colnames(district_communities)[1] <- "BoroCD"

nyc_districts_comm <- cd %>%
  left_join(district_communities, by = "BoroCD")

nyc_districts_comm <- nyc_districts_comm %>% filter(!is.na(community))


ggplot(nyc_districts_comm) +
  geom_sf(aes(fill = as.factor(community)), color = "black",
          linewidth = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_family = "Times New Roman") +
  labs(fill = "Community",
       title = "New York Districts by Network Community",
       subtitle = "Infomap Community Detection Method on Morning Data") +
  theme(
    # Center title and subtitle
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12))






### Creating the Weekday graph with its communities
stations_sf <- st_as_sf(
  coords,
  coords = c("X", "Y"),
  crs = 4326
) %>% 
  mutate(
    community = V(evening_graph_Infomap)$Infomap
  )

stations_with_district <- st_join(stations_sf, cd)


district_communities <- stations_with_district %>% 
  st_drop_geometry() %>%
  group_by(BoroCD.x) %>% 
  summarise(community = names(which.max(table(community))))

colnames(district_communities)[1] <- "BoroCD"

nyc_districts_comm <- cd %>%
  left_join(district_communities, by = "BoroCD")

nyc_districts_comm <- nyc_districts_comm %>% filter(!is.na(community))


ggplot(nyc_districts_comm) +
  geom_sf(aes(fill = as.factor(community)), color = "black",
          linewidth = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_family = "Times New Roman") +
  labs(fill = "Community",
       title = "New York Districts by Network Community",
       subtitle = "Infomap Community Detection Method on Evening Data") +
  theme(
    # Center title and subtitle
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12))







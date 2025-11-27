getwd()
setwd("C:/Users/User/OneDrive - Corvinus University of Budapest/Dokumentumok/Coding and shi")
setwd("/Users/ujvaribotond/Desktop/R/Applied_Network_Science/Project_work/202510-citibike-tripdata")
setwd("/Users/ujvaribotond/Desktop/R/Applied_Network_Science/Project_work")


install.packages("janitor")
install.packages("ggraph")

####################
### Do the descriptive stuff on the networks (in-degree, out-degree distribution
### ratio of self loops, centrality measures and their correlation)
### Do a very nice graph
### Community detection is good, it might even bring back the boroughs
### Focus on the InfoMap part, but for robustness, we did the Louvain as well





# all this shit is needed
library(tidyverse)
library(sf)
library(data.table)
library(janitor)
library(igraph)
library(dplyr)

# getting the data
# link :  https://s3.amazonaws.com/tripdata/index.html

### If we want to use every data for the selected month (this is for the latest, 2025 October)
tripdata1 <- fread("202510-citibike-tripdata_1.csv")
tripdata2 <- fread("202510-citibike-tripdata_2.csv")
tripdata3 <- fread("202510-citibike-tripdata_3.csv")
tripdata4 <- fread("202510-citibike-tripdata_4.csv")
tripdata5 <- fread("202510-citibike-tripdata_5.csv")
tripdata <- rbind(tripdata1, tripdata2, tripdata3, tripdata4, tripdata5)

tripdata <- janitor::clean_names(tripdata)

rm(tripdata1, tripdata2, tripdata3, tripdata4, tripdata5)


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

?st_read
#--------------------------------
# getting the community districts
#--------------------------------
#(important, must get the shape (.shp)file and not csv or other shit)
### Keep the file inside the downloaded folder!!!! (otherwise it does not work)
setwd("/Users/ujvaribotond/Desktop/R/Applied_Network_Science/Project_work")



cd <- st_read("NYC_Community_Districts_-6420008696060170624/NYC_Community_Districts.shp") %>%
  st_transform(4326)


# community district identifier is the left column, the number meanings:
# 1 - Manhattan
# 2 - Bronx
# 3 - Brooklyn
# 4 - Queens
# 5 - Staten Island (but there are no bike stations here)
# the numbers after that are the district numbers within the boroughs 
# See all the community districts here: https://boundaries.beta.nyc/?map=cd


cd <- st_read("NYC_Community_Districts_-6420008696060170624 2/NYC_Community_Districts.shp") %>%
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


### Optionally get rid of the original tripdata_cd so that we don't fry our computers
### but it might be too late at this point
rm(tripdata_cd, tripdata)
################################################################################


#lets try to get a district level bikeflow network

### Here is the same as 
###district_network <- tripdata_cd %>%
###  filter(!is.na(start_BoroCD), !is.na(end_BoroCD)) %>%
###  count(start_BoroCD, end_BoroCD, name = "trips")

### But in a function for every type of dataset (The NAs become meaningless here, since
### if there were NAs connected to the stations in the boroughs, they were filtered here)

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
?edge_density
edge_density(weekday_graph)
### 0.791 there are more edges during the weekday than the weekend
edge_density(weekend_graph)
### 0.74


# Reciprocity
?reciprocity
reciprocity(weekday_graph, ignore.loops = F)
### 0.94
reciprocity(weekend_graph, ignore.loops = F)
### 0.93

### There is no directional bias, almost all the edges are used back and forth

# Assortativity
?assortativity
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
?edge_density
edge_density(morning_graph)
### 0.635
edge_density(evening_graph)
### 0.734

### People use more routes between stations than in the morning

# Reciprocity
?reciprocity
reciprocity(morning_graph, ignore.loops = F)
### 0.887
reciprocity(evening_graph, ignore.loops = F)
### 0.917

### No directional bias (the edges are used in both directions)


# Assortativity
?assortativity
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
### Etc...

### What we can talk about in the paper:
### On the weekdays, the top in-degrees are around 3.5 times higher than for the weekend (even if we have 5 vs. 2 days, the ratios are different)
##### Main takeaway should be that these communities are overlapping the borough structure
##### of New York very much 
##### In the Infomap, North side of Manhattan (109,110,111,112 and for morning/evening 107, 108 and 109) are drawn up as their own community
##### this can be explained maybe as a job AND home dense area (morning) and a nightlife center (evenings)
##### creates its own community, but others are very clearly divided along the boroughs


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
?modularity
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

### Shows almost identical communities between morning and evening (keep in mind this is an undirected graph,
### hence it might not be the best method for transportation showings)

### Memberships (6 communities)

mem_louvain_morning
mem_louvain_evening
## In the morning, it is very similar to the weekday specification (Financial District,
### Hell's Kitchen and Chinatown, North Manhattan closely connected to the whole of Bronx)
### In the Evening South of Central Park, then North of Central Park + Bronx, then
### each borough being its own cluster with Central Brooklyn being its own little island

#### Modularity comparison
?modularity
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





#### INFOMAP 
### Weekdays vs. weekends

#### Create the directed graph for the Infomap method
weekday_graph_Infomap <- do_the_graph_from_edgelist_Infomap(weekday_edgelist)

weekend_graph_Infomap <- do_the_graph_from_edgelist_Infomap(weekend_edgelist)

#### Run Infomap method on the graphs
?cluster_infomap
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




#################### Graphs ####################################################

#### geographical mapping

# --- 1. Load shapefile ---
districts <- st_read("NYC_Community_Districts_-6420008696060170624 2/NYC_Community_Districts.shp")

districts <- districts %>%
  mutate(BoroCD = as.numeric(BoroCD))

# --- 2. Select top flows ---
topN <- 200

morning_top <- morning_edgelist %>% 
  arrange(desc(weight)) %>% 
  slice(1:topN) %>% 
  mutate(type = "Morning")

evening_top <- evening_edgelist %>% 
  arrange(desc(weight)) %>% 
  slice(1:topN) %>% 
  mutate(type = "Evening")

flows <- bind_rows(morning_top, evening_top)

# --- 3. District centroids ---
coords <- districts %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  bind_cols(BoroCD = districts$BoroCD)

# --- 4. Merge coordinates ---
flows2 <- flows %>% 
  left_join(coords, by = c("start_BoroCD" = "BoroCD")) %>% 
  rename(x_start = X, y_start = Y) %>% 
  left_join(coords, by = c("end_BoroCD" = "BoroCD")) %>% 
  rename(x_end = X, y_end = Y)

# --- 5. Remove flows where start=end (geom_curve cannot draw them) ---
flows2_clean <- flows2 %>%
  filter(!(x_start == x_end & y_start == y_end))

# Optional: tiny jitter to avoid overlapping curves
# flows2_clean <- flows2_clean %>%
#   mutate(x_start = jitter(x_start, amount = 200),
#          y_start = jitter(y_start, amount = 200))

# --- 6. Plot ---
ggplot() +
  geom_sf(data = districts, fill = "grey90", color = "white") +
  
  geom_curve(
    data = flows2_clean,
    aes(
      x = x_start, y = y_start,
      xend = x_end, yend = y_end,
      color = type,
      linewidth = weight
    ),
    curvature = 0.2,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    alpha = 0.6
  ) +
  
  scale_color_manual(values = c("Morning" = "#1f78b4", "Evening" = "#e31a1c")) +
  scale_linewidth(range = c(0.1, 2), guide = "none") +
  
  labs(
    title = "NYC Bike Flows Between Community Districts",
    subtitle = "Blue = Morning inflows toward Midtown | Red = Evening outflows",
    color = "Time of Day"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "bottom"
  ) +
  coord_sf()





#### NETWORK GRAPH...takes a shitton of time to plot so careful

library(ggraph)
library(ggplot2)

# --- 1. Detect communities for node color (optional)
community <- graph %>% 
  mutate(community = as.factor(group_infomap()))

# --- 2. Plot network with ggraph ---
ggraph(community, layout = "fr") +   # "fr" = Fruchterman-Reingold (clean!)
  
  # Edges (flows)
  geom_edge_fan(
    aes(edge_width = weight,
        edge_colour = type,
        alpha = after_stat(index)),
    show.legend = TRUE
  ) +
  
  # Nodes (districts)
  geom_node_point(size = 6, color = "grey20", fill = "white", shape = 21) +
  
  # Labels
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
  
  # Color settings
  scale_edge_colour_manual(
    values = c(
      "Morning" = "#1f78b4",
      "Evening" = "#e31a1c"
    ),
    name = "Flow Type"
  ) +
  
  scale_edge_width(range = c(0.2, 3), guide = "none") +
  
  guides(alpha = "none") +
  
  labs(
    title = "NYC Bike Flow Network Between Districts",
    subtitle = "Morning (blue) riders move into Midtown, Evening (red) riders flow outward",
    edge_colour = "Time Period"
  ) +
  
  theme_graph(
    base_family = "sans",
    title_size = 16,
    subtitle_size = 12
  )
#########################x
###########################

print(
  ggraph(community, layout = "fr") +
    geom_edge_fan(
      aes(edge_width = weight, edge_colour = type, alpha = after_stat(index)),
      show.legend = TRUE
    ) +
    geom_node_point(size = 6, color = "grey20", fill = "white", shape = 21) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
    scale_edge_colour_manual(values = c("Morning"="#1f78b4","Evening"="#e31a1c")) +
    scale_edge_width(range = c(0.2, 3), guide = "none") +
    guides(alpha = "none") +
    labs(
      title = "NYC Bike Flow Network Between Districts",
      subtitle = "Morning (blue) into Midtown, Evening (red) outward",
      edge_colour = "Time Period"
    ) +
    theme_graph(base_family = "sans", title_size = 16, subtitle_size = 12)
)




##### cord diagram
install.packages("circlize")
library(circlize)
library(dplyr)

# Prepare matrix for chord diagram
all_nodes <- sort(unique(c(
  morning_edgelist$start_BoroCD,
  morning_edgelist$end_BoroCD,
  evening_edgelist$start_BoroCD,
  evening_edgelist$end_BoroCD
)))

flow_matrix <- matrix(0, nrow = length(all_nodes), ncol = length(all_nodes))
rownames(flow_matrix) <- all_nodes
colnames(flow_matrix) <- all_nodes

# Fill matrix
for(i in 1:nrow(morning_edgelist)) {
  flow_matrix[
    as.character(morning_edgelist$start_BoroCD[i]),
    as.character(morning_edgelist$end_BoroCD[i])
  ] <- morning_edgelist$weight[i]
}

for(i in 1:nrow(evening_edgelist)) {
  flow_matrix[
    as.character(evening_edgelist$start_BoroCD[i]),
    as.character(evening_edgelist$end_BoroCD[i])
  ] <- flow_matrix[
    as.character(evening_edgelist$start_BoroCD[i]),
    as.character(evening_edgelist$end_BoroCD[i])
  ] + evening_edgelist$weight[i]
}

# Chord diagram
circos.clear()
circos.par(start.degree = 90, gap.degree = 4)

chordDiagram(
  flow_matrix,
  transparency = 0.75,
  annotationTrack = c("grid"),
  preAllocateTracks = list(track.height = 0.1)
)

title("District-to-District Bike Flows (Morning + Evening Combined)")

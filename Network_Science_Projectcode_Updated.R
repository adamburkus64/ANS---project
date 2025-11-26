getwd()
setwd("/Users/ujvaribotond/Desktop/R/Applied_Network_Science/Project_work/202510-citibike-tripdata")


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



cd <- st_read("NYC_Community_Districts_-6420008696060170624 2/NYC_Community_Districts.shp") %>%
  st_transform(4326)


# community district identifier is the left column, the number meanings:
# 1 - Manhattan
# 2 - Bronx
# 3 - Brooklyn
# 4 - Queens
# 5 - Staten Island
# the numbers after that are the district numbers within the boroughs (1-18)
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

### Easier way to look at the highest values
top_contenders <- function(statistics, category){
statistics %>%
  arrange(desc(category)) %>%
  slice_head(n = 10)
}
  
### Weekend graph
weekend_graph <- graph_from_data_frame(weekend_edgelist, directed = T)

V(weekend_graph)$name
E(weekend_graph)$weight
?strength

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

# In-degree differences (I'm not sure if Kolmogorov Smirnoff test is good for this...)
ks.test(statistics_weekday$weight_in_degree, statistics_weekend$weight_in_degree)


### Correlation between centrality measures
# PageRank
cor(statistics_weekday$PageRank, statistics_weekend$PageRank)


# Betweenness
cor(statistics_weekday$betweenness, statistics_weekend$betweenness)


# In/Out-degree
cor(statistics_weekday$weight_in_degree, statistics_weekend$weight_in_degree)
cor(statistics_weekday$weight_out_degree, statistics_weekend$weight_out_degree)


### Comparison on the network level
# Density
edge_density(weekday_graph)
edge_density(weekend_graph)

# Reciprocity
reciprocity(weekday_graph)
reciprocity(weekend_graph)

# Assortativity
assortativity_degree(weekday_graph)
assortativity_degree(weekend_graph)


### Ratio of self-loops
loops_weekday <- sum(E(weekday_graph)$weight[which_loop(weekday_graph)])
loops_weekend <- sum(E(weekend_graph)$weight[which_loop(weekend_graph)])

loop_ratio_weekday <- loops_weekday / sum(E(weekday_graph)$weight)
loop_ratio_weekend <- loops_weekend / sum(E(weekend_graph)$weight)

loop_ratio_weekday
loop_ratio_weekend



### Rank changes (FIX IT)
compare %>%
  mutate(rank_weekday = rank(-statistics_weekday$weight_in_degree),
         rank_weekend = rank(-statistics_weekend$weight_in_degree),
         diff = rank_weekday - rank_weekend) %>%
  arrange(desc(abs(diff)))


### Interpretations:
### high-indegree == Many people arrive here (job-dense areas, like Manhattan)
### high-outdegree == Many people start here (residential areas, where most people live)
### self-loops == How much people cycle inside the districts (ratio of local cyclers vs. cross-district ones)
### Betweenness == People go through these districts (they don't start otr finish here, but its a central district)
### PageRank == Looks at how central the neighbors are of a give district
### Reciprocity == Low reciprocity means a directional bias (only going from home to work but not vice versa)
### Etc...

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

# ARI
ari_me_week_louvain <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "adjusted.rand")
## 0.9024 <- Shows they are quite similar
## 0.9705
## 0.8897
### After including central park
## 0.7724


# NMI
nmi_me_week_louvain <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "nmi")
## 0.9278 <- again, showing they are quite similar
## 0.9657
### After including central park
## 0.8936

# VI
vi_me_week_louvain  <- compare(mem_louvain_weekday, mem_louvain_weekend, method = "vi")
## 0.2257 <- since not that close to 0, they do have some differences
## 0.1014
### After including central park
## 0.3429


#### Modularity comparison
?modularity
modularity_louvain_weekday <- modularity(weekday_graph_Louvain, mem_louvain_weekday, 
                                         weights = E(weekday_graph_Louvain)$weight)
## 0.517

modularity_louvain_weekend <- modularity(weekend_graph_Louvain, mem_louvain_weekend, 
                                         weights = E(weekend_graph_Louvain)$weight)






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
## 0.9324 <- Shows they are quite similar, but show some differences
## 0.9705
## 0.8897
## 0.8611
## 0.8518
### After including central park
## 0.8215
## 0.9731

# NMI
nmi_me_day_louvain <- compare(mem_louvain_morning, mem_louvain_evening, method = "nmi")
## 0.9235 <- again, showing they are similar
## 0.9657
## 0.9278
## 0.8919
## 0.8863
### After including central park
## 0.8781
## 0.9675

# VI
vi_me_day_louvain  <- compare(mem_louvain_morning, mem_louvain_evening, method = "vi")
## 0.218 <- since not that close to 0, they do have some differences
## 0.1014
## 0.2257
## 0.3271
## 0.3423
### After including central park
## 0.3730
## 0.0967

#### Modularity comparison
?modularity
modularity_louvain_morning <- modularity(morning_graph_Louvain, mem_louvain_morning, 
                                         weights = E(morning_graph_Louvain)$weight)
### 

modularity_louvain_afternoon <- modularity(afternoon_graph_Louvain, mem_louvain_afternoon, 
                                           weights = E(afternoon_graph_Louvain)$weight)
### 


modularity_louvain_evening <- modularity(evening_graph_Louvain, mem_louvain_evening, 
                                         weights = E(evening_graph_Louvain)$weight)

### 


#### INFOMAP (experimental, nem futtattam még le xd)
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
## 0.8364
### After including central park
## 0.8393

# NMI
nmi_me_week_info <- compare(mem_infomap_weekday, mem_infomap_weekend, method = "nmi")
## 0.8721
### After including central park
## 0.8778


# VI
vi_me_week_info  <- compare(mem_infomap_weekday, mem_infomap_weekend, method = "vi")
## 0.3285
### After including central park
## 0.3202




#### Mornings vs. evenings (optionally vs.afternoon)

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

# ARI
ari_me_day_info <- compare(mem_infomap_morning, mem_infomap_evening, method = "adjusted.rand")
# 1
### After including central park
## 0.8315


# NMI
nmi_me_day_info <- compare(mem_infomap_morning, mem_infomap_evening, method = "nmi")
# 1
### After including central park
## 0.8602


# VI
vi_me_day_info  <- compare(mem_infomap_morning, mem_infomap_evening, method = "vi")
# 0
### After including central park
## 0.3933



#### VERY PRETTY GRAPHS

lv <- cluster_louvain(g, weights = E(g)$weight)
im <- cluster_infomap(g, e.weights = E(g)$weight)

###
membership_louvain <- membership(lv)
membership_infomap <- membership(im)

###

V(g)$louvain <- membership(lv)
V(g)$infomap <- membership(im)

# Optional: node size based on total strength
V(g)$size <- strength(g, mode = "all", weights = E(g)$weight)


###
set.seed(123)
layout <- layout_with_fr(g)

plot(
  g,
  layout = layout,
  vertex.color = V(g)$louvain,
  vertex.size = sqrt(V(g)$size) * 2,     # scale sizes
  vertex.label.cex = 0.7,
  vertex.label.color = "black",
  edge.arrow.size = 0.3,
  main = "Louvain Community Detection"
)

memberships <- as_tbl_graph(g) %>%
  mutate(
    louvain = as.factor(V(g)$louvain),
    infomap = as.factor(V(g)$infomap),
    degree = strength(g, mode = "all", weights = E(g)$weight)
  )


set.seed(123)

ggraph(tg, layout = "fr") + 
  geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) +
  geom_node_point(aes(color = infomap, size = degree)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_brewer(palette = "Dark2") +
  theme_graph() +
  ggtitle("Infomap Communities in NYC Bike Sharing Network")


### Graph where nodes are placed according to coordinates

ggraph(tg, layout = "manual", x = distr_x, y = distr_y) +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(aes(color = louvain, size = degree)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

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
districts <- st_read("NYC_Community_Districts_-6420008696060170624 2/NYC_Community_Districts.shp")

districts <- districts %>%
  mutate(BoroCD = as.numeric(BoroCD))


coords <- districts %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  bind_cols(BoroCD = districts$BoroCD)


coords <- coords[-c(67,65,35,25, 63,46,22,5,23,49,36,71,29,40,42,1,51,41,52,33,2,32,3,34,12,37,38,31),]

coords <- coords %>%
  arrange(BoroCD)

###Creating a base graph showing the original district distribution inside the boroughs

coords$borough <- floor(coords$BoroCD / 100)

coords$borough_name <- factor(coords$borough,
                          levels = 1:4,
                          labels = c("Manhattan", "Bronx", "Brooklyn", "Queens"))

stations_sf <- st_as_sf(
  coords,
  coords = c("X", "Y"),
  crs = 4326
) %>% 
  mutate(
    community = coords$borough_name
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
       title = "New York Community Districts With Boroughs",) +
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






### Creating the Evening graph with its communities
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


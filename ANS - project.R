#Grab data from net into data frame-----

#MEGJEGYZÉS: Ne tudom melyik datat szeretnénk használni.

#Ha jól néztem a JC-vel kezdődő jóval kisebb dataframek azok New Jersey-re szólnak
#nem New York-ra

url <- "https://s3.amazonaws.com/tripdata/JC-202510-citibike-tripdata.zip"

zip_path <- tempfile(fileext = ".zip")
extract_dir <- tempdir()

download.file(url, zip_path, mode = "wb")
unzipped <- unzip(zip_path, exdir = extract_dir)

# Keep only CSV-like files
csv_candidates <- unzipped[grepl("\\.csv$", unzipped, ignore.case = TRUE)]

# Pick the largest CSV → this is the real trip data
sizes <- file.info(csv_candidates)$size
main_csv <- csv_candidates[which.max(sizes)]

library(readr)
df <- read_csv(main_csv)

df

#----- Clear data -----

library(dplyr)
library(igraph)

df <- df %>% 
  filter(!is.na(end_station_id) & !is.na(start_station_id))   #remove NA

edge_list = df %>%
  select(start_station_id, end_station_id)      #get edge list from start and end

weighted_edge_list = edge_list %>%    #weight is how many times a link between 2 nodes appear
  group_by(start_station_id, end_station_id) %>%
  summarise(weight = n())

g <- graph_from_data_frame(weighted_edge_list, directed = T)

plot(g)

# transforming to undirected
g_undir <- as_undirected(g, mode = "collapse", edge.attr.comb = list(weight = "sum"))


#ChatGPT do your thing-----
#megkértem, hogy csináljon beőle valami értelmezhetőbb vizualizációt

library(igraph)
library(ggraph)
library(ggplot2)

# Suppose g is your undirected weighted graph

# 1. Detect communities
cl <- cluster_louvain(g_undir)  # you can also use cluster_walktrap(), etc.

# 2. Add the community membership as a vertex attribute
V(g_undir)$community <- membership(cl)

# 3. Plot with ggraph
ggraph(g_undir, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.3) +
  geom_node_point(aes(color = factor(community)), size = 2) +
  theme_void()



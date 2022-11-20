library(here)
library(ggmap)
data <- read.csv(here("listings.csv"))

limits <- c(left=floor(min(data$latitude)), 
           right=floor(max(data$latitude)),
           top=-floor(max(data$longitude)),
           bottom=-floor(min(data$longitude)))

get_stamenmap(limits, zoom = 5) %>% ggmap()

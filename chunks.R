## ---- chunk-setup ----
here::i_am("projeto_modelos.Rnw")
if (!require("pacman")) install.packages("pacman", repos = "https://cran.radicaldevelop.com/")
pacman::p_load(
  here,
  knitr,
  kableExtra,
  ggplot2,
  here,
  tmap,
  dplyr,
  sf,
  cowplot,
  ggpubr,
  ggforce,
  ggridges,
  forcats,
  ggcorrplot
)
showT <- function(x, fullWidth = F, ...) {
  kable(x, align="r", ...) %>% 
    kable_styling(
      full_width = F, 
      position = "left",  
      font_size = 10, 
      latex_options = c("striped"),
      latex_table_env= ifelse(fullWidth, "RTable*", "RTable")
    )
}
extractLegend <- function(ggp){
  ggdraw() + draw_grob(get_legend(ggp), x = 1, y = 1, hjust = 1, vjust = 1)
}

opts_chunk$set(
  warning = FALSE, #TODO warnings, messages
  message = FALSE,
  results = TRUE,
  tidy.opts = list(width.cutoff = 25), 
  tidy = TRUE,
  fig.cap = "",
  fig.env = "marginfigure",
  fig.width = 4,
  fig.height = 4
)

## ---- chunk-dfLoad
df <- read.csv(here("data", "listings.csv"))
shape <- st_read(here("data", "SF Planning Neighborhood Groups Map"))
tmap_mode("plot")
shape_plot <- shape %>% ggplot()+ geom_sf() + theme(legend.position = "bottom")

## ---- chunk-dfClasses 
data.frame(
  row.names = colnames(df),
  "type" = sapply(df, class)
) %>% showT()

## ---- chunk-dfHead
#df %>% head(1) %>% t() %>% showT(T)

## ---- chunk-selectUsefulOnly
# df %>% select(-id, -host_id, -neighbourhood_group, -license, -availability_365) -> df #TODO n tenho a certeza de quais tirar tmb
# tmb n se se é preciso

## ---- chunk-plotPlace
shape_plot + 
  geom_point(data=df, aes(y=latitude, x=longitude), alpha=0.5, color="red", size=0.1)


## ---- chunk-rasterPlaces
rast <- (shape_plot + 
  stat_bin2d(
    data=df, 
    aes(x=longitude, y=latitude), 
    alpha=0.7, bins = 30, linejoin="round"
  ) +
  scale_fill_viridis_c(option="C"))
rast
## ---- chunk-rasterLegend
#rast %>% extractLegend

## ---- chunk-freqTableNeighbs
df %>% group_by(neighbourhood) %>% summarise(n=n(), freq = n/nrow(df)) %>% 
  arrange(-n) %>% head(8) %>% showT()

## ---- chunk-plotNeighbs
shape_plot +
  geom_point(data=df, aes(y=latitude, x=longitude, color=neighbourhood), alpha=0.5, size=0.1) +
  geom_point(
    data=(df %>% filter(neighbourhood == "Downtown/Civic Center")), 
  aes(y=latitude, x=longitude), color="red", alpha=1, size=0.1) +
  theme(legend.position = "none")


## ---- chunk-hullNeighbs
hull <- shape_plot +
  geom_mark_hull(data=df, aes(y=latitude, x=longitude, fill=neighbourhood), expand = 0, radius=0,alpha=0.7, linewidth=0.1)
hull + theme(legend.position = "none")

## ---- chunk-hullLegend
hull %>% get_legend() #TODO check https://stackoverflow.com/questions/33927027/how-to-extract-the-legend-labels-from-a-ggplot2-object

## ---- chunk-priceBoxPlot
pricePlot <- ggplot(data=df, aes(price))
pricebox <- pricePlot + 
  geom_boxplot() + 
  coord_flip()
pricebox

## ---- chunk-priceBoxPlotLim
upper_limit <- quantile(df$price, 0.975) + 20
pricebox + xlim(0,upper_limit)

## ---- chunk-priceHead
df %>% select(name, price) %>% arrange(-price) %>% head(7) %>% showT(T)

## ---- chunk-priceHist
pricePlot +
  geom_histogram(binwidth=25, aes(y = ..density..)) +
  geom_density(color="red") +
  xlim(0, upper_limit)

## ---- chunk-boxPriceNeighs
df %>% ggplot(aes(x=price, y = fct_reorder(neighbourhood, -price, .fun=median))) + geom_boxplot() + xlim(0, upper_limit) +
labs(y = "")

## ---- chunk-freqRoomTypes
df %>%
  group_by(room_type) %>% 
  summarise(
    n = n(), 
    freq = n()/nrow(.),
    averagePrice = mean(price)
  ) %>% showT()
## ---- chunk-freqRoomTypesWoLuxo
df %>%
  group_by(room_type) %>% 
  filter(price < upper_limit) %>%
  summarise(
    n = n(), 
    freq = n()/nrow(.),
    averagePrice = mean(price)
  ) %>% showT()

## ---- chunk-RoomTypesPrice
ggplot(df, aes(x=price, y=room_type, fill=room_type)) +
  geom_density_ridges() + xlim(0, upper_limit) +
  theme_ridges() + theme(legend.position = "none") 

## ---- chunk-CorrPlot
ggcorrplot(
  cor(df %>% select(latitude, longitude, price, reviews_per_month, availability_365, number_of_reviews_ltm), use = "complete.obs"), 
      lab = TRUE
  )


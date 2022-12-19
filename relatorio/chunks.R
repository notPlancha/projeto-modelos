## ---- chunk-setup ----
here::i_am("README.md")
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
      full_width = fullWidth, 
      font_size = 10, 
      latex_options = c("striped")
    )
}
extractLegend <- function(ggp){
  ggdraw() + draw_grob(get_legend(ggp), x = 1, y = 1, hjust = 1, vjust = 1)
}

## ---- chunk-temp
R.version$version.string

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
rast %>% extractLegend

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
ggplot(data=df, aes(price)) +
  geom_boxplot() + 
  coord_flip()

## ---- chunk-priceBoxPlotLim
upper_limit <- quantile(df$price, 0.975) + 20
ggplot(data=df, aes(price)) +
  geom_boxplot() + 
  coord_flip() +
  xlim(0,upper_limit)


## ---- chunk-priceHead
df %>% select(name, price) %>% arrange(-price) %>% head(7) %>% showT(T)

## ---- chunk-priceHist
ggplot(data=df, aes(price)) +
  geom_histogram(binwidth=25, aes(y = ..density..)) +
  geom_density(color="red") +
  xlim(0, upper_limit)

## ---- chunk-boxPriceNeighs
df %>% ggplot(aes(y=price, x = forcats::fct_reorder(neighbourhood, price, .fun=median))) + geom_boxplot() + coord_cartesian(ylim=c(0, upper_limit)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(x = "")

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

## ---- chunk-priceHexes
shape_plot +
  stat_summary_hex(data = df, aes(x=longitude, y=latitude, z= log(price)), alpha=0.8) +
  scale_fill_viridis_c() +
  theme(legend.position = "right")

## ---- chunk-readyDf

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
  ggcorrplot,
  tidymodels,
  themis,
  MLmetrics,
  lmtest,
  tseries
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
validP <- function(p.value, wantH0=TRUE, alpha=0.05){
  if (p.value < alpha){
    return (ifelse(wantH0, paste("p <", alpha), paste("p =", p.value)))
  }else{
    return (ifelse(wantH0, paste("p =", p.value), paste("p >", alpha)))
  }
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
    averagePrice = mean(price),
    sd = sd(price),
    min = min(price),
    max = max(price)
  ) %>% showT()
## ---- chunk-freqRoomTypesWoLuxo
df %>%
  group_by(room_type) %>% 
  filter(price < upper_limit, price > 0) %>%
  summarise(
    n = n(), 
    freq = n()/nrow(.),
    averagePrice = mean(price),
    sd = sd(price),
    min = min(price),
    max = max(price)
  ) %>% showT()

## ---- chunk-RoomTypesPrice
ggplot(df, aes(x=price, y=room_type, fill=room_type)) +
  geom_density_ridges() + xlim(0, upper_limit) +
  theme_ridges() + theme(legend.position = "none") 

## ---- chunk-CorrPlot
df %>% 
select(price, latitude, longitude,minimum_nights, number_of_reviews, calculated_host_listings_count, availability_365) %>% 
cor(use = "complete.obs") %>% 
ggcorrplot(lab = TRUE, type = "lower")

## ---- chunk-priceHexes
shape_plot +
  stat_summary_hex(data = df, aes(x=longitude, y=latitude, z= log(price)), alpha=0.8) +
  scale_fill_viridis_c() +
  theme(legend.position = "right")


## ---- chunk-readyDf
recipeUpSample <- recipe(price ~ neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews + calculated_host_listings_count + availability_365, data=df) %>%
  step_upsample(room_type, skip=FALSE, over_ratio = 0.363) %>% 
  step_dummy(neighbourhood, room_type) %>%
  prep()
recipeNoUpSample <- recipe(price ~ neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews + calculated_host_listings_count + availability_365, data=df) %>%
step_dummy(neighbourhood, room_type) %>%
prep()
dfAll <- bake(recipeUpSample, df %>% filter(price > 0))
dfAll %>% group_by(isHotel = room_type_Hotel.room, isPrivateRoom = room_type_Private.room, isSharedRoom = room_type_Shared.room) %>% summarise(n=n(), freq=n/nrow(dfAll), averagePrice = mean(price)) %>% showT()

## ---- chunk-splitDf
set.seed(123)
dfSplit <- initial_split(df, prop = 0.8, strata = room_type)
dfTrain <- training(bake(recipeUpSample, dfSplit))
dfTest <- testing(bake(recipeNoUpSample, dfSplit))
cbind(
  "train" = c(nrow(dfTrain), nrow(dfTrain)/nrow(dfAll)),
  "test" = c(nrow(dfTest), nrow(dfTest)/nrow(dfAll))
) %>% showT()

## ---- chunk-fit0
fit0 <- lm(price ~ ., dfAll)
fit0 %>% glance() %>% select(
  R2=r.squared, 
  AIC=AIC) %>% mutate(
  MAPE=MAPE(dfAll$price, predict(fit0, dfAll)),
  "Breusch-Pagan"=validP(bptest(fit0)$p.value, FALSE),
  "Breusch-Godfrey"=validP(bgtest(fit0)$p.value, FALSE),
  "Jarque-Bera"=validP(jarque.bera.test(fit0$residuals)$p.value, TRUE)
) %>% t() %>% showT()
plot(fit0, 1)
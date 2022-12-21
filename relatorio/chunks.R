## ---- chunk-setup ----
here::i_am("README.md")
if (!require("pacman")) install.packages("pacman", repos = "https://cran.radicaldevelop.com/")
pacman::p_load(
  here,
  knitr,
  kableExtra,
  ggplot2,
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
  Metrics,
  Fgmutils,
  lmtest,
  tseries, # Jarque-Bera
  conflicted,
  Fgmutils, # rRMSE
  car, # crPlot
  MASS, # rlm
  stats # nls
)
conflict_prefer("here", "here")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("mape", "Metrics")
showT <- function(x, fullWidth = F, ...) {
  kable(x, align="r", ...) %>% 
    kable_styling(
      full_width = fullWidth, 
      font_size = 10, 
      latex_options = c("striped")
    )
}
validP <- function(p.value, wantH0=TRUE, alpha=0.05){
  if (p.value < alpha){
    return (ifelse(wantH0, paste("p <", alpha), paste("p =", p.value %>% format(digits=3))))
  }else{
    return (ifelse(wantH0, paste("p =", p.value %>% format(digits=3)), paste("p >", alpha)))
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


## ---- chunk-rasterPlaces
rast <- (shape_plot + 
  stat_bin2d(
    data=df, 
    aes(x=longitude, y=latitude), 
    alpha=0.7, bins = 30, linejoin="round"
  ) +
  scale_fill_viridis_c(option="C"))
rast

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
df %>% select(name, price) %>% arrange(-price) %>% head(7) %>% showT(TRUE)

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
recipeUpSample <- recipe(price ~ neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews + calculated_host_listings_count + availability_365, data=df %>% filter(price > 0, neighbourhood != "Golden Gate Park")) %>%
  step_upsample(room_type, skip=FALSE, over_ratio = 0.363) %>% 
  step_dummy(neighbourhood, room_type) %>%
  prep()
recipeNoUpSample <- recipe(price ~ neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews + calculated_host_listings_count + availability_365, data=df %>% filter(price > 0, neighbourhood != "Golden Gate Park")) %>%
step_dummy(neighbourhood, room_type) %>%
prep()
dfAll <- bake(recipeUpSample, df %>% filter(price > 0, neighbourhood != "Golden Gate Park"))
dfAll %>% group_by(isHotel = room_type_Hotel.room, isPrivateRoom = room_type_Private.room, isSharedRoom = room_type_Shared.room) %>% summarise(n=n(), freq=n/nrow(dfAll), averagePrice = mean(price)) %>% showT()

## ---- chunk-splitDf
set.seed(123)
dfSplit <- initial_split(df %>% filter(price > 0, neighbourhood != "Golden Gate Park"), prop = 0.8, strata = room_type)
dfTrain <- training(dfSplit) %>% bake(recipeUpSample, .)
dfTest <- testing(dfSplit) %>% bake(recipeNoUpSample, .)

## ---- chunk-testModel
testModel <- function(fit, dfAlll, desc, transPrice = (\(x) x )){
  predicted <- predict(fit, dfAlll) 
  fit %>% glance() %>% select(
    R2=r.squared, 
    AIC=AIC) %>% mutate(
    R2 = formatC(R2, digits=3),
    AIC = formatC(AIC, digits=3),
    MAPE=mape(dfAlll$price %>% transPrice(), predicted) %>% 
      format(digits=3),
    rRMSE=rrmse(dfAlll$price %>% transPrice(), predicted) %>% 
      format(digits=3),
    "Breusch-Pagan"=validP(bptest(fit)$p.value, FALSE),
    "Breusch-Godfrey"=validP(bgtest(fit)$p.value, FALSE),
    "Jarque-Bera"=validP(jarque.bera.test(fit$residuals)$p.value, TRUE)
  ) %>% mutate(Model=desc) %>% relocate(Model)
}

## ---- chunk-fit0
fit0 <- lm(price ~ ., dfAll)
testModel(fit0, dfAll, "fit0(price ~ .)") %>% t() %>% showT()
plot(fit0, 1)

## ----chunk-fitTrans
fit0.cube <- lm(price^3 ~ ., dfAll)
fit0.squared <- lm(price^2 ~ ., dfAll)
fit0.root <- lm(sqrt(price) ~ ., dfAll)
fit0.log <- lm(log(price) ~ ., dfAll)
fit0.invSqrt <- lm(1/sqrt(price) ~ ., dfAll)
fit0.inv <- lm(1/price ~ ., dfAll)
fit0.invSquared <- lm(1/price^2 ~ ., dfAll)
fit0.invCube <- lm(1/price^3 ~ ., dfAll)
rbind(
  testModel(fit0.cube, dfAll, "cube", (\(x) x^3 )),
  testModel(fit0.squared, dfAll, "squared", (\(x) x^2 )),
  testModel(fit0.root, dfAll, "root", sqrt),
  testModel(fit0.log, dfAll, "log", log),
  testModel(fit0.invSqrt, dfAll, "invSqrt", (\(x) 1/sqrt(x))),
  testModel(fit0.inv, dfAll, "inv", (\(x) 1/x)),
  testModel(fit0.invSquared, dfAll, "invSqdr", (\(x) 1/x^2)),
  testModel(fit0.invCube, dfAll, "invCube", (\(x) 1/x^3))
  ) %>% t() %>% showT(TRUE)

## ---- chunk-fitTransOut
fit0.log.outOfSample <- lm(log(price) ~ ., dfTrain)
fit0.invSqrt.outOfSample <- lm(1/sqrt(price) ~ ., dfTrain)
rbind(
  testModel(fit0.log, dfAll, "log", log),
  testModel(fit0.invSqrt, dfAll, "invSqrt", (\(x) 1/sqrt(x))),
  testModel(fit0.log.outOfSample, dfTest, "logOut", log),
  testModel(fit0.invSqrt.outOfSample, dfTest, "invSqrtOut", (\(x) 1/sqrt(x)))
) %>% t() %>% showT(F)

## ---- chunk-crPlots
if(FALSE){
  crPlots(fit0.log)
}

## ---- chunk-fitTransMN
fit1.noMN <- lm(log(price) ~ . - minimum_nights, dfAll)
fit1 <- lm(log(price) ~ ., dfAll)
fit1.MNsq <- lm(log(price) ~ ., dfAll %>% mutate(mnsqd = minimum_nights^2, mncube = minimum_nights^3))
rbind(
  testModel(fit1.noMN, dfAll, "noMN", log),
  testModel(fit1, dfAll, "withMN", log),
  testModel(fit1.MNsq, dfAll %>% mutate(mnsqd = minimum_nights^2, mncube = minimum_nights^3), "MNsq", log)
) %>% t() %>% showT(F)

## ---- chunk-fitWLS
dfAllMN <- dfAll %>% mutate(mnsqd = minimum_nights^2, mncube = minimum_nights^3)
pfit1 <- lm(log(price) ~ ., dfAllMN)$fitted.values
fit2 <- lm(log(price) ~ ., dfAllMN)
fit2.sqdrN <- lm(log(price) ~ ., dfAllMN, weights=1/((1:nrow(dfAll))^2))
fit2.rootN <- lm(log(price) ~ ., dfAllMN, weights=1/((1:nrow(dfAll))^0.5))
fit2.rootPred <- lm(log(price) ~ ., dfAllMN, weights=(1/pfit1^0.5))
fit2.sqdrPred <- lm(log(price) ~ ., dfAllMN, weights=(1/pfit1^2))
rbind(
  testModel(fit2, dfAllMN, "noW", log),
  testModel(fit2.sqdrN, dfAllMN, "sqdrN", log),
  testModel(fit2.rootN, dfAllMN, "rootN", log),
  testModel(fit2.rootPred, dfAllMN, "rootPred", log),
  testModel(fit2.sqdrPred, dfAllMN, "sqdrPred", log)
) %>% t() %>% showT(F)

## ---- chunk-fit3
#fit3.rlm <- rlm(log(price) ~ ., dfAllMN)
#fit3.nls <- nls(log(price) ~ ., dfAllMN)
#fit3.glmGaussian <- glm(log(price) ~ ., dfAllMN, family=gaussian)
#fit3.glmGamma <- glm(log(price) ~ ., dfAllMN, family=Gamma(link="log"))
#rbind(
#  testModel(fit3.rlm, dfAllMN, "rlm", log),
#  testModel(fit3.nls, dfAllMN, "nls", log),
#  testModel(fit3.glmGaussian, dfAllMN, "glmGaussian", log),
#  testModel(fit3.glmGamma, dfAllMN, "glmGamma", log)
#) %>% t() %>% showT(F)

## ---- chunk-residPlot
plot(fit2.sqdrPred, 1)

## ---- chunk-fitWLSOut
dfTrainMN <- dfTrain %>% 
  mutate(mnsqd = minimum_nights^2, mncube = minimum_nights^3)
dfTestMN <- dfTest %>% 
  mutate(mnsqd = minimum_nights^2, mncube = minimum_nights^3)
pfit1.outOfSample <- lm(log(price) ~ ., dfTrainMN)$fitted.values
fit2.sqdrN.outOfSample <- lm(log(price) ~ ., dfTrainMN, 
  weights=1/((1:nrow(dfTrain))^2))
fit2.rootN.outOfSample <- lm(log(price) ~ ., dfTrainMN, 
  weights=1/((1:nrow(dfTrain))^0.5))
fit2.sqdrPred.outOfSample <- lm(log(price) ~ ., dfTrainMN, 
  weights=(1/pfit1.outOfSample^2))
rbind(
  testModel(fit2.sqdrN, dfAllMN, "sqdrN", log),
  testModel(fit2.sqdrN.outOfSample, dfTestMN, "sqdrNOut", log),
  testModel(fit2.rootN, dfAllMN, "rootN", log),
  testModel(fit2.rootN.outOfSample, dfTestMN, "rootNOut", log),
  testModel(fit2.sqdrPred, dfAllMN, "sqdrPred", log),
  testModel(fit2.sqdrPred.outOfSample, dfTestMN, "sqdrPredOut", log)
) %>% t() %>% showT(F)

## ---- chunk-coefTable
options(digits=3)
fit<- lm(log(price) ~ ., dfAllMN, weights=1/((1:nrow(dfAll))^0.5))
fit %>% tidy() %>% mutate(estimate = format(estimate, scientific=T), p.value = format(p.value, scientific=T)) %>% showT()
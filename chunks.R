## ---- chunk-setup ----
if (!require("pacman")) install.packages("pacman", repos = "https://cran.radicaldevelop.com/")
pacman::p_load(
  here,
  knitr,
  kableExtra,
  ggplot2,
  here,
  ggplot2,
  tmap,
  dplyr
)
showT <- function(x, fullWidth = F, ...) {
  kable(x, align="c", ...) %>% 
    kable_styling(
      full_width = F, 
      position = "left",  
      font_size = 10, 
      latex_options = c("striped"),
      latex_table_env= ifelse(fullWidth, "RTable*", "RTable")
    )
}

show
opts_chunk$set(
  warning = TRUE, 
  message = TRUE,
  tidy.opts = list(width.cutoff = 40), 
  tidy = TRUE,
  fig.cap = " ",
  fig.env = "marginfigure",
  fig.width = 4,
  fig.height = 4
)

## ---- chunk-listings ----
listings <- read.csv(here("data", "listings.csv"))
nrow(listings)

## ---- chunk-listingsClasses ----
data.frame(
  row.names = colnames(listings),
  "type" = sapply(listings, class)
) %>% showT()

## ---- chunk-listingsHead ----
#listings %>% head(1) %>% t() %>% showT(T)


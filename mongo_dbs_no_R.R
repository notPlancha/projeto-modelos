install.packages("mongolite")


library(mongolite)
library(tidyverse)
library(timechange)
library(lubridate)
library(ggplot2)

connection_string = "mongodb://localhost:27017/imdb"
movies_collection = mongo(collection = "movies", db = "usamovies", url = connection_string)
movies_collection$count()

movies_collection$find(query ='{"year":1990}', limit = 5,
                       fields = '{"year":true, "title":true}')

movies_collection$iterate()$one()

yd#names(ydf) <- c("year", "Count")
f <- movies_collection$aggregate('[{"$group":{"_id":"$year", "Count":{"$sum":1}}}]')
df <- as.data.frame(ydf)

ggplot(df, aes(x=reorder(`_id`, Count), y=Count)) + 
         geom_bar(stat = "identity", color='yellow', fill='#FFC300') + 
         geom_text(aes(label=Count), color="red") + 
         coord_flip() + 
         xlab("Title Type")

#ydf <- ydf[order(ydf[,"_id"]),]


tournaments_collection = mongo(collection = "players", db = "attp")
tournaments_collection$distinct("country")


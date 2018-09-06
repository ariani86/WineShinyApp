library(dplyr)
library(NLP)
library(tm)
library(wordcloud)
library(memoise)
library(shiny)
library(ggplot2)
library(stringr)
library(SnowballC)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(plotly)
# library(googleCharts)

wine.df <- read.csv("./data/winemag-data-130k-v3.csv",fileEncoding = "latin1")

wine.df$vintage_year <- as.numeric(wine.df$vintage_year)

wine.df$vintage_year[as.numeric(wine.df$vintage_year)>2018] <- 2018
wine.df$vintage_year[as.numeric(wine.df$vintage_year)<1934] <- median(wine.df$vintage_year[!is.na(wine.df$vintage_year)])

Price_mean_all = mean(wine.df$price, na.rm = TRUE)
Points_mean_all= mean(wine.df$points, na.rm = TRUE)
plot(wine.df$points, wine.df$price)
core = cor(wine.df$points, wine.df$price, use = "complete.obs")

randowRows = function(df, n){
  return (df[sample(nrow(df),n),])
}

winepart.df = randowRows(wine.df, 15000)
winesub.df = randowRows(wine.df, 1000)


cleantable <- winepart.df %>%
  select(
    Country = country,
    Province = province,
    Region1 = region_1,
    Region2 = region_2,
    Winery = winery,
    Points = points,
    Price = price,
    Description = description,
    Designation = designation,
    Taster_name = taster_name,
    Taster_twitter_handle = taster_twitter_handle,
    Title = title,
    Variety = variety,
    VintageYear = vintage_year
  )

vtable <- winesub.df %>%
  select(
    Country = country,
    Province = province,
    Region1 = region_1,
    Region2 = region_2,
    Winery = winery,
    Points = points,
    Price = price,
    Description = description,
    Designation = designation,
    Taster_name = taster_name,
    Taster_twitter_handle = taster_twitter_handle,
    Title = title,
    Variety = variety,
    VintageYear = vintage_year
  )

#text = winesub.df$description
getTermMatrix <- memoise(function(text) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  text = paste0(text,collapse = ' ')
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"),"drink","palate","offers","shows","wine", "the", "where", "when","finish","and", "but","flavors","flavor","aroma","aromas","taste","tastes","nose","note","notes"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  v = sort(rowSums(m), decreasing = TRUE)
})
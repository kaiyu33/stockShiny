#install.packages("dygraphs")

library(dygraphs)
dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))


  dygraph(nhtemp, main = "New Haven Temperatures") %>% 
    dyShading(from = "1920-1-1", to = "1930-1-1") %>%
    dyShading(from = "1940-1-1", to = "1950-1-1")
library(jsonlite)
library(tidyverse)

system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20220219&lang=en" ')

# (c)
# converting to the list
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# (d)
mydf %>% class()
mydf$date %>% class()

# (e)
mydf %>% head()

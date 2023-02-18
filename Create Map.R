library(tidyverse)
library(magrittr)
library(readxl)
library(RSelenium)

### download exported library from Goodreads ---------

books <- read_csv("/Users/alexisathens/Documents/Personal/Reading/goodreads_library_export-end-of-2022.csv")

# just keep books that are marked as 'read'
books %<>% filter(!is.na(`Date Read`))

# select and rename relevant columns
books %<>% 
  select(id = `Book Id`, year = `Original Publication Year`, title = Title, author = Author, isbn = ISBN)


### set up RSelenium for web scraping ---------



### scrape Wikipedia for author's country of origin ---------



### get world map and country info ---------



### match country names ---------



### color world map based on read count ---------



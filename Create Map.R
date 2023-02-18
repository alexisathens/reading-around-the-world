library(tidyverse)
library(magrittr)
library(readxl)
library(RSelenium)

### download exported library from Goodreads ---------
# download data by going to My Books > on side bar under Tools, select Import and export > click Export Library

books <- read_csv("/Users/alexisathens/Documents/Personal/Reading/goodreads_library_export-end-of-2022.csv")

# just keep books that are marked as 'read'
books %<>% filter(!is.na(`Date Read`))

# select and rename relevant columns
books %<>% 
  select(id = `Book Id`, year = `Original Publication Year`, title = Title, author = Author, isbn = ISBN)


### set up RSelenium for web scraping ---------

# close docker container from before (skip if first run)
system('docker stop firefox')
system('docker rm firefox')

# set up docker container
system('docker run --platform linux/amd64 --name firefox -v /dev/shm:/dev/shm -d -p 4567:4444 -p 5901:5900 selenium/standalone-firefox:latest')

# check new container open
system('docker ps -a')

# open a virtual network 
system('open vnc://127.0.0.1:5901')

# specify the remote driver
remDr <- remoteDriver(port = 4567L, browser = "firefox")

# open up a firefox window in the VNC
remDr$open()

# navigate to Wikipedia
remDr$navigate('https://www.wikipedia.org/')



### scrape Wikipedia for author's country of origin ---------

# search for search box element
search_box <- remDr$findElement(using = 'id', value = 'searchInput')

# enter author name into search bar and go
search_box$sendKeysToElement(list(books$author[5], key = 'enter'))

# give time for page to load
remDr$setTimeout(type = "page load", milliseconds = 10000)

# get URL of author webpage
author_url <- remDr$getCurrentUrl()[[1]]

# parse URL, just keeping author extension (last element)
author_short <- tail(str_split(author_url, "/")[[1]], 1)

# use XML route
remDr$navigate(paste0("https://en.wikipedia.org/wiki/Special:Export/", author_short))


# next step: how to get "birth_place" from XML:
# https://en.wikipedia.org/wiki/Special:Export/Viet_Thanh_Nguyen

test <- remDr$findElement(using = 'xpath', 'birth_place')

test <- remDr$findElement(using = 'xpath', '//mediawiki/page/revision/text')

remDr$findElement(using = 'xpath', 'mediawiki')

remDr$findElement(using = "link text", "birth_place")

mediawiki > page > revision > text

mediawiki/page/revision/text


# get 'Born' info from side bar
# hard because there is no id - just class that is reused multiple times
# born_box <- remDr$findElements(using = "class", value = "infobox-label")
# born_text <- born_box$getElementAttribute("innerHTML")[[1]]



## use XML route instead??
https://en.wikipedia.org/wiki/Special:Export/Viet_Thanh_Nguyen
birth_place = [[Buôn Ma Thuột|Ban Mê Thuột]], [[South Vietnam]]


### get world map and country info ---------



### match country names ---------



### color world map based on read count ---------



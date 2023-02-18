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



### scrape Wikipedia for author's URL ---------

# initialize column to store author URL
books$url <- NULL

# define buffer to not overload website
buffer_sec <- 2
Sys.sleep(buffer_sec)


## need error catching!


# Douglas Stuart - ambiguous
# Isaac Fitzgerald - no side bar


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



### use Wikipedia JSON to find author's country of origin ---------

# ok JSON route
json_url <- paste0("https://en.wikipedia.org//w/api.php?action=query&format=json&prop=revisions&titles=", author_short,
                   "&formatversion=2&rvprop=content&rvslots=*")
  
library(RJSONIO)

info <- fromJSON(json_url)

# get content - the giant text mass
content <- info$query$pages[[1]]$revisions[[1]]$slots$main[["content"]]

content_split <- str_split(content, "\\n\\| ")[[1]] # split on new lines

birth_place <- content_split[which(str_detect(content_split, "birth_place"))] # search for birth_place keyword

# clean up a bit to just get string
birth_place <- str_remove(birth_place, "birth_place")
birth_place <- str_remove(birth_place, "=")
birth_place <- str_remove_all(birth_place, "\\[\\[")
birth_place <- str_remove_all(birth_place, "\\]\\]")
birth_place <- trimws(birth_place)
# good enough for now


# store for author


### get world map and country info ---------



### match country names ---------



### color world map based on read count ---------



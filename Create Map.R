library(tidyverse)
library(magrittr)
library(readxl)
#library(RSelenium)
library(RJSONIO) # for reading Wiki JSON files

### download exported library from Goodreads ---------
# download data by going to My Books > on side bar under Tools, select Import and export > click Export Library

books <- read_csv("/Users/alexisathens/Documents/Personal/Reading/goodreads_library_export-end-of-2022.csv")

# just keep books that are marked as 'read'
books %<>% filter(!is.na(`Date Read`))

# select and rename relevant columns
books %<>% 
  select(id = `Book Id`, year = `Original Publication Year`, title = Title, author = Author, isbn = ISBN)


### set up RSelenium for web scraping ---------

# # close docker container from before (skip if first run)
system('docker stop firefox')
system('docker rm firefox')

# set up docker container
system('docker run --platform linux/amd64 --name firefox -v /dev/shm:/dev/shm -d -p 4567:4444 -p 5901:5900 selenium/standalone-firefox:latest')

# check new container open
# system('docker ps -a')

# open a virtual network
system('open vnc://127.0.0.1:5901')

# specify the remote driver
remDr <- remoteDriver(port = 4567L, browser = "firefox")

# open up a firefox window in the VNC
remDr$open()



### first pass using simple Wiki extension for author JSON ------------------

## assume simple Wikipedia extension

# there's a standard format for each author's Wikipedia extension, which is their full name with spaces replaced by underscores
books$basic_url <- str_replace_all(books$author, " ", "_")
# assume this is the case. if it doesn't work, we'll find the author manually later

# also initialize birth_place column because this is what we want to figure out
books$birth_place <- NA

# note that there are a few different breaking points:
# (1) no Wikipedia page, i = 22
# (2) ambiguous Wiki page, i = 6
# (3) Wiki page but with no side bar info, i = 1
# (4) side bar but no birth_place
# strategy is to skip all of these cases and fill in manually


for(i in 1:nrow(books)){
  
  json_url <- paste0("https://en.wikipedia.org//w/api.php?action=query&format=json&prop=revisions&titles=", 
                     books$basic_url[i], "&formatversion=2&rvprop=content&rvslots=*")
  
  
  # first check if this is a bad link
  bad_link <- NULL
  
  try(info <- fromJSON(json_url), silent = TRUE)
  
  if(!is.null(bad_link)){
    # if this is a bad link, skip to the next author and continue
    next
  }
  
  # get main content block (giant text mass)
  mass <- info$query$pages[[1]]$revisions[[1]]$slots$main[["content"]]
  
  if(is.null(mass)){ # if bad link again
    next
  }
  
  # check for field called birth_place
  birth_avail <- str_detect(mass, "birth_place")
  
  if(!birth_avail){
    # if this field isn't available, skip to the next author and continue
    next
  }
  
  ## clean up birth_place if it is available
  mass_split <- str_split(mass, "\\n\\|")[[1]] # split on new lines
  
  birth_place <- mass_split[which(str_detect(mass_split, "birth_place"))] # search for birth_place keyword
  
  # clean up a bit to just get string
  birth_place <- str_remove(birth_place, "birth_place")
  birth_place <- str_remove(birth_place, "=")
  birth_place <- str_remove_all(birth_place, "\\[\\[")
  birth_place <- str_remove_all(birth_place, "\\]\\]")
  birth_place <- trimws(birth_place)
  # good enough for now
  
  if(birth_place == ""){
    # if birth_place field is present but empty, skip for author
    next
  }
  
  # finally, track birth place string for author
  books$birth_place[i] <- birth_place
  
}


mean(!is.na(books$birth_place)) # about 2/3 of authors found

books$birth_place_parsed <- NA

# parse strings, taking last chunk as country
for(i in 1:nrow(books)){
  # parse first on comma
  temp <- trimws(tail(str_split(books$birth_place, ",")[[i]], 1))
  
  # then on |, if needed
  if(str_detect(temp, "\\|") & !is.na(temp)){
    temp <- trimws(tail(str_split(temp, "\\|")[[1]], 1))
  }
  
  # fill in parsed version
  books$birth_place_parsed[i] <- temp
}



## next: use RSelenium to check if author url was wrong

missing_authors <- books %>% filter(is.na(birth_place)) %>% distinct(author)



### scrape Wikipedia for missing author's URL ---------


## NEED TO TEST NEXT!

# initialize column to store author URL
books$fixed_url <- NA

# define buffer to not overload website
buffer_sec <- 1

## loop through all books and get author's Wiki URL
for(i in 1:nrow(missing_authors)){

  # send message to console
  cat(paste0("**** Retrieving info for author ", i, " of ", nrow(missing_authors), " ****  \n")); flush.console()

  # navigate to Wikipedia
  remDr$navigate('https://www.wikipedia.org/')
  Sys.sleep(buffer_sec)

  # search for search box element
  search_box <- remDr$findElement(using = 'id', value = 'searchInput')
  Sys.sleep(buffer_sec)

  # enter author name into search bar and go
  search_box$sendKeysToElement(list(missing_authors[i], key = 'enter'))
  Sys.sleep(buffer_sec)

  # give time for page to load
  remDr$setTimeout(type = "page load", milliseconds = 10000)

  # check if a disambiguation page
  ambiguous <- NULL

  try(ambiguous <- remDr$findElement(using = "link text", value = "disambiguation"), silent = TRUE)

  if(!is.null(ambiguous)){
    # if it's an ambiguous page, skip to the next author and continue
    next
  }

  # get URL of author webpage
  author_url <- remDr$getCurrentUrl()[[1]]
  Sys.sleep(buffer_sec)

  # parse URL, just keeping author extension (last element)
  author_short <- tail(str_split(author_url, "/")[[1]], 1)

  # store URL in df for all author instances
  books$fixed_url[which(books$author[i] == books$author)] <- author_short

}







## need error catching!

# Douglas Stuart - ambiguous
# Isaac Fitzgerald - no side bar

# checking if country in the list exists
suppressMessages({
  try(elem <- remDr$findElement(using = "link text", value = this_country_name), silent = TRUE)
})

if(is.null(elem)){ # if the country isn't on the WTO website, skip it

  next # next skips the current iteration of the loop (skips to the next c)
  # see: https://www.datamentor.io/r-programming/break-next/

}


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







books %>% 
  filter(is.na(birth_place)) %>% 
  distinct(author)

books$birth_place
# take behind last comma





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



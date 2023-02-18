library(tidyverse)
library(magrittr)
library(readxl)
library(RJSONIO) # for reading Wiki JSON files
library(sf) # for handling shape files

### download exported library from Goodreads ---------
# download data by going to My Books > on side bar under Tools, select Import and export > click Export Library

books <- read_csv("/Users/alexisathens/Documents/Personal/Reading/goodreads_library_export-end-of-2022.csv")

# just keep books that are marked as 'read'
books %<>% filter(!is.na(`Date Read`))

# select and rename relevant columns
books %<>% 
  select(id = `Book Id`, year = `Original Publication Year`, title = Title, author = Author, isbn = ISBN)


### scrape author home country from Wiki JSON ---------------------------------

## assume simple Wikipedia extension

# there's a standard format for each author's Wikipedia extension, which is their full name with spaces replaced by underscores
books$basic_url <- str_replace_all(books$author, " ", "_")
# assume this is the case. if it doesn't work, we'll find the author manually later

# also initialize birth_place column because this is what we want to figure out
books$birth_place <- NA

# note that there are a few different breaking points, such as:
# no Wikipedia page, ambiguous Wiki page, Wiki page but with no side bar info, side bar but no birth_place, etc.
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


## clean up country names slightly
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



### do manual country search for missing authors ----------------------------

## next: get list of missing authors and fill in manually

missing_authors <- books %>% filter(is.na(birth_place)) %>% distinct(author) %>% pull(author)

# tip: print missing authors to console, use option key stroke to organize, and then group by country

books %<>% 
  mutate(birth_place_manual = case_when(
    author %in% c("Isaac Fitzgerald", "Dan Schilling", "Cal Newport", "Marguerite Roza", "Emily Nagoski", 
                  "Meg Jay", "Helaine Olen", "J.D. Vance", "Nancy Foner", "Alfred Lansing", "Oren Cass",
                  "Stanley D. Frank", "Susan Cain", "Kerry Patterson", "Joel Best", "Spencer Johnson",
                  "Timothy Ferriss", "Kim Malone Scott", "A. Poulin Jr.", "United Nations", "Torrey Peters",
                  "Melissa Febos", "Nicholas Carr", "Finn Murphy", "Lori Gottlieb", "Paul Kalanithi", "Andy Hunt",
                  "Robert T. Kiyosaki", "Kurt Vonnegut Jr.", "James Clear", "Lee Airton", "Matthew Desmond",
                  "William Finnegan") ~ "United States",
    author %in% c("J.K. Rowling", "Alex Rawlings", "Oliver Burkeman", "Rob Hopkins", "Greg McKeown",
                  "Douglas   Stuart", "Mary Wollstonecraft Shelley") ~ "United Kingdom",
    author %in% c("Sohn Won-Pyung", "Cho Nam-Joo") ~ "South Korea",
    author %in% c("Gabriel García Márquez") ~ "Colombia",
    author %in% c("Abhijit V. Banerjee") ~ "India",
    author %in% c("Ernesto Che Guevara") ~ "Argentina",
    author %in% c("Sönke Ahrens") ~ "Germany",
    author %in% c("Viktor E. Frankl") ~ "Austria",
    author %in% c("Hyeonseo Lee") ~ "North Korea",
    author %in% c("Rory Carroll") ~ "Ireland",
    author %in% c("Wade Davis") ~ "Canada",
    author %in% c("Walpola Rahula") ~ "Sri Lanka",
    
    
    # template: author %in% c() ~ ""
    
    TRUE ~ NA_character_
  ))

          
          



### harmonize author home country variable -----------------------------------

## manual harmonization of Wiki scrapes
scraped_places <- books %>% filter(!is.na(birth_place_parsed)) %>% distinct(birth_place_parsed) %>% pull()

# only do these if they don't match later (have to update/standardize some country names)
books %<>% 
  mutate(birth_place_fixed = case_when(
    birth_place_parsed %in% c("U.S.", "Wisconsin", "Los Angeles", "US", "Massachusetts", "U.S", "New York", "Idaho", 
                              "Pennsylvania") ~ "United States",
    birth_place_parsed %in% c("England", "UK", "Scotland") ~ "United Kingdom",
    birth_place_parsed %in% c("South Vietnam") ~ "Vietnam",
    birth_place_parsed %in% c("México") ~ "Mexico",
    birth_place_parsed %in% c("German Empire") ~ "Germany",
    birth_place_parsed %in% c("Russian Empire") ~ "Russia",
    birth_place_parsed %in% c("Ottoman Empire") ~ "Lebanon",
    birth_place_parsed %in% c("Khmer Republic") ~ "Cambodia",
    birth_place_parsed %in% c("Austria-Hungary") ~ "Czech Republic",
    birth_place_parsed %in% c("French Algeria") ~ "Algeria",
    birth_place_parsed %in% c("British India") ~ "India",
    birth_place_parsed %in% c("British Nigeria") ~ "Nigeria",
    
    # some exceptionally screwed up ones
    str_detect(birth_place_parsed, "Walter\\}\\}") ~ "United States",
    str_detect(birth_place_parsed, "name=fn1\\}\\}|Northamptonshire") ~ "United Kingdom",
    
    # template: birth_place_parsed %in% c() ~ "",
    
    TRUE ~ birth_place_parsed
  ))

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             


### get world map and match country info ---------

# download publicly available world map from IPUMS: https://international.ipums.org/international/gis.shtml

map <- st_read("/Users/alexisathens/Documents/Personal/R/reading-around-the-world/World Map/IPUMSI_world_release2020/world_countries_2020.shp")

country_names <- map$CNTRY_NAME
country_names <- data.frame(country_name = country_names) # convert to df


## check which names don't map over, go back and fix above

# manual names first
manual_names <- books %>% distinct(birth_place_manual) %>% drop_na() %>% pull()
setdiff(manual_names, country_names$country_name)

# then parsed names
auto_names <- books %>% distinct(birth_place_fixed) %>% drop_na() %>% pull()
setdiff(auto_names, country_names$country_name)


## condense into single country field and clean up

books %<>% 
  select(-birth_place) %>% # drop original messed up field
  mutate(birth_place = birth_place_manual) %>% # fill with manual first, then auto
  mutate(birth_place = ifelse(is.na(birth_place), birth_place_fixed, birth_place))

# double check no missing countries
books %>% filter(is.na(birth_place))

# drop intermediary fields
books %<>% select(id:author, birth_place)

# finally, aggregate to country level!
country_reads <- books %>% 
  group_by(birth_place) %>% 
  count() %>% 
  ungroup() %>% 
  rename(books = n)



### color world map based on read count ---------

## check out basic world map

ggplot() + geom_sf(data = map) # takes FOREVER


## aggregate data based on country level and join to map df





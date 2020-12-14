# @Tania Guerrero 
#Getting the data from ZooBorns website 
# just for fun and maybe to have a condensed database that can become a shiny app 

# Installing rvest 
#install.packages("rvest")
library(rvest)
library(readr)
library(tidyr)
library(dplyr)
library(tidyselect)
library(stringr)

### the ZooBorns data 
options <- read_html("https://www.zooborns.com/zooborns/") %>% 
  html_nodes(".module-list-item") %>% 
  html_text()

#options <- read_html("https://www.zooborns.com/zooborns/")  
#parse_character(zooborns) 

##How to filter this better? 
#maybe with extract2 from magtrittr (info @ ?html_nodes)
animals <- options[3:169]
tail(animals)
zoos <- options[170:371]
tail(zoos)

zooborns <- read_html("https://www.zooborns.com/zooborns/") %>% 
  html_nodes(".module-list-item") %>% 
  html_children()

href <- html_attr(zooborns, "href") #Parse this with tidy fxs 
#Do the same for each zoo

#I guess I would need to loop for each of these hrefs 
content <- read_html("https://www.zooborns.typepad.com/zooborns/aardvark/") %>% 
  html_nodes(".entry-title") %>% 
  html_text()

dates <- read_html("https://www.zooborns.typepad.com/zooborns/aardvark/") %>% 
  html_nodes(".entry-date") %>% 
  html_text()
#to filter the actual birth dates I could also use the thingie for characters to filter for month: 
#smth like creating a vector with the month names and then ask to find them 
#apparently the node to extract the text is "entry-body font-entrybody"

#or just keep the year from the entry-date.  

#Loop for html   
#First let's clean the hrefs for both animals and zoos 

#href <- href[!is.na(href)]
href <- na.omit(href[3:742])
tail(href)
#We use just a few links to validate: 
test_animals <- href[5:7]
test_zoos <- href[170:175]

births <- sapply(test_animals, function(scrap) {
  yo <- read_html(scrap) %>% 
    html_nodes(".entry-title") %>% 
    html_text()
})

###yay it works! :) 
#let's repeat it for dates  
dates <- sapply(test_animals, function(scrap) {
  yo <- read_html(scrap) %>% 
    html_nodes(".entry-date") %>% 
    html_text()
})

##and Zoos
zoo_births <- sapply(test_zoos, function(scrap) {
  yo <- read_html(scrap) %>% 
    html_nodes(".entry-title") %>% 
    html_text()
})


dates_zoos <- sapply(test_zoos, function(scrap) {
  yo <- read_html(scrap) %>% 
    html_nodes(".entry-date") %>% 
    html_text()
})

#### Extracting and merging the information by animal, dates and zoos
t1 <- unlist(births$`https://www.zooborns.com/zooborns/aye-aye`)
t2 <- unlist(dates$`https://www.zooborns.com/zooborns/aye-aye`)

#THIS WORKS! THIS RECAPITULATES ALL STEPS ABOVE SO I'D ONLY NEED TO REPEAT IT 
#WITH THE FULL DATA AND MIGHT BE POSSIBLE TO PIPE IT AND/OR LOOP IT 
zoo_tbl <- as_tibble(unlist(births)) %>%   #WHATS THE EQUIVALENT OF UNLIST IN DYPLR?
  mutate(bday = unlist(dates)) %>% 
  rename(long_names = value)


#Substring animals to make them all singulars 
animals <- gsub(".{-1}$", "", animals)
tail(animals)
#now correct the irregular singulars
animals <- str_replace(animals, "Wolve", "Wolf") #this messes up with wolverine #maybe I can fix it with regex
animals <- str_replace(animals, "Shee", "Sheep")
?str_replace
animals
lnames <- zoo_tbl$long_names
#zoo_tbl %>% 
#str_replace(lnames, "Anteater", "tamandua")

zoo_tbl %>% 
  mutate(new_names = if_else(str_detect(lnames, "Anteater"), 
                             "Anteater", lnames)) %>% 
  mutate(new_names = 
           if_else(str_detect(lnames, "Tamandua"), 
                   "Anteater", new_names)) %>% 
  mutate(new_names = 
           if_else(str_detect(lnames, "Armadillo"), 
                   "Armadillo", new_names)) %>% 
  mutate(new_names = 
           if_else(str_detect(lnames, "Aye-Aye"), 
                   "Aye-Aye", new_names)) %>% 
  mutate(new_names = 
           if_else(str_detect(lnames, "Aye-aye"), 
                   "Aye-Aye", new_names)) %>% 
  mutate(new_names = 
           if_else(str_detect(lnames, "AYE-AYE"), 
                   "Aye-Aye", new_names)) -> zoo_tbl
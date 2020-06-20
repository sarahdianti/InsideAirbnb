library(tidyverse)
library(rvest)
library(xml2)

# Input link to an object, then read
url <- "http://insideairbnb.com/get-the-data.html"
html_list <- read_html(url)

# 
city_table <- html_list %>% 
  html_nodes("table")

# This code is performed to analyse the 2nd city from the link, Antwerp
for (number_of_city in 2) {
  table <- city_table[[number_of_city]]%>%
    html_table()
  
  # Get all links to the file for Antwerp city
  table <- city_table[[number_of_city]]%>%
    html_table()
  all_links <- city_table[[number_of_city]] %>% 
    html_nodes("a") %>% html_attr("href")
  table$link <- all_links
  
  # Tidy up column names
  colnames(table) <- gsub(" ","_",tolower(colnames(table)))
  colnames(table) <- gsub("/","_",colnames(table))
  
  # Format file upload date in English version
  table$date_compiled <- as.Date(table$date_compiled,format = "%d %b,%Y")
  
  # Get the latest Review file
  review_data <- table %>% 
    arrange(desc(date_compiled)) %>% 
    filter(grepl("Detailed Review",description)) %>%  
    top_n(1) 
  
  # Get the latest Listing file
  listing_data <- table %>% 
    arrange(desc(date_compiled)) %>% 
    filter(grepl("Detailed Listings",description))%>%
    top_n(1)  # Get only the latest Listing file
  
  # Get all Calendar files available
  calendar_data <- table %>% 
    arrange(desc(date_compiled)) %>% 
    filter(grepl("Detailed Calendar",description))
  
  # Get the city name
  this_city <- tolower(listing_data$country_city[1])
  
  # Get the folder name
  city_folder <-paste0("temp/",this_city) 
  
  # Create parent folder and let dir.exist to find whether subfolder exists
  dir.create("temp") 
  if(!dir.exists(city_folder)){
    
    print(paste0("Now we are creating the folder for:",city_folder))
    
    dir.create(city_folder)
    
    # Download csv.gz files for Listing, Review and Calendar
    download.file(url = listing_data$link[1],destfile = paste0(city_folder,"/listings.csv.gz"))
    download.file(url = review_data$link[1],destfile = paste0(city_folder,"/reviews.csv.gz"))
    for (number_of_calendar in 1:nrow(calendar_data)) {
      download.file(url = calendar_data$link[number_of_calendar],
                    destfile = paste0(city_folder,"/",calendar_data$date_compiled[number_of_calendar],
                                      ".","calendar.csv.gz"))
    }
  }
}
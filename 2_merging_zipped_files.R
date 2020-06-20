# Antwerp placed in the second from city_table
# See 1_web_crawling_and_download
city_index <- 2 
table <- city_table[[city_index]] %>%
  html_table()
this_city <- antwerp
temp_city_folder <-paste0("temp/",this_city)

# Read downloaded Listing and Review files in R
listings_df <- readr::read_csv( paste0(temp_city_folder,"/listings.csv.gz"))
reviews_df <- readr::read_csv( paste0(temp_city_folder,"/reviews.csv.gz"))
listings_df <- listings_df %>% rename(listing_id = id)
reviews_df <- reviews_df %>% rename(review_id = id)

# Join Listing and Review data frame using ID
# Relational databases
all_data_df <- listings_df %>% 
  left_join(reviews_df)

# Delete unused data frame to clean memory
#rm(listings_df)
#rm(reviews_df)
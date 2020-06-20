# The processed textual data of airbnb can now be use for getting insights
# For example,find dominant words per aggregation category
# In this case, the aggregation category is neighborhood in Antwerp

# 1. Get the top 10 tokens per neighborhood (top 5) for the review
# Get the top 5 neighborhood name 
top_5_neighborhood <- all_data_df %>% 
  select(listing_id,neighbourhood_cleansed) %>%
  unique(.) %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(total =n()) %>% 
  arrange(desc(total)) %>% 
  top_n(5)

# Get listing_id and tokens in comments for these neighborhoods
top_5_neighborhood_listing <- all_data_df %>% 
  select(listing_id,neighbourhood_cleansed) %>% 
  filter(neighbourhood_cleansed %in% top_5_neighborhood$neighbourhood_cleansed) %>% 
  unique(.)
neighbourhood_tokens_comments<- tokens_all_comments_tf_idf %>% 
  right_join(top_5_neighborhood_listing) %>%  
  group_by(neighbourhood_cleansed,word) %>% 
  summarise(total=sum(n)) %>% 
  arrange(desc(total)) 

# Get the top 10 tokens per neighborhood for review
for(neighb in 1:nrow(top_5_neighborhood)){
  print(paste0("For neighbourhood: ",top_5_neighborhood$neighbourhood_cleansed[neighb]))
  toprint <- neighbourhood_tokens_comments %>% ungroup() %>% 
    filter(neighbourhood_cleansed == top_5_neighborhood$neighbourhood_cleansed[neighb]) %>% 
    top_n(10,total) %>% 
    select(-total) %>% 
    mutate(rank = row_number())
  
  print(toprint)
}


# 2. Get the top 10 tokens per neighborhood (top 5) for description
# Get tokens in description for these neighborhoods
neighbourhood_tokens_description <- tokens_all_description_tf_idf %>% 
  right_join(top_5_neighborhood_listing) %>%  
  group_by(neighbourhood_cleansed,word) %>% 
  summarise(total=sum(n)) %>% 
  arrange(desc(total)) 

# Get the top 10 tokens per neighborhood for description
for(neighb in 1:nrow(top_5_neighborhood)){
  print(paste0("For neighbourhood: ",top_5_neighborhood$neighbourhood_cleansed[neighb]))
  toprint <- neighbourhood_tokens_description %>% ungroup() %>% 
    filter(neighbourhood_cleansed == top_5_neighborhood$neighbourhood_cleansed[neighb]) %>% 
    top_n(10,total) %>% 
    select(-total) %>% 
    mutate(rank = row_number())
  
  print(toprint)
}

# 3. Get the top 10 tokens per neighborhood (top 5) for transport
# Get tokens in transit for these neighborhoods
neighbourhood_tokens_transport <- tokens_transport_tf_idf %>% 
  right_join(top_5_neighborhood_listing) %>%  
  group_by(neighbourhood_cleansed,word) %>% 
  summarise(total=sum(n)) %>% 
  arrange(desc(total)) 

# Get the top 10 tokens per neighborhood for transport
for(neighb in 1:nrow(top_5_neighborhood)){
  print(paste0("For neighbourhood: ",top_5_neighborhood$neighbourhood_cleansed[neighb]))
  toprint <- neighbourhood_tokens_transport %>% ungroup() %>% 
    filter(neighbourhood_cleansed == top_5_neighborhood$neighbourhood_cleansed[neighb]) %>% 
    top_n(10,total) %>%   
    select(-total) %>% 
    mutate(rank = row_number())
  
  print(toprint)
}

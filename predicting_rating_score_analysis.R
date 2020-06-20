# The processed textual data of airbnb can now be use for getting insights
# For example, what variable can be extracted from the text
# that can be related with the rating score?

# This file will answer these questions:
# 1. Is readibility of property description an important predictor of the rating?
# 2. Is mentioning the host name is important?

# Extract top words per rating
rating_categories <- all_data_df %>% 
  group_by(listing_id) %>% 
  summarise(avg_rating = mean(review_scores_rating)) %>% ungroup()
hist(rating_categories$avg_rating,breaks = 50)

# Find the levels that we want to aggregate the words in 
quan <- quantile(rating_categories$avg_rating,na.rm = T)

# Assign level in a rating group
# Use 75% as cut off
rating_categories$rating_category <- ifelse(rating_categories$avg_rating<quan[4],1,2) 

# Extract words in reviews
ratings_categories_tokens_reviews <- tokens_all_comments %>% 
  left_join(rating_categories) %>% 
  group_by(rating_category,word) %>% 
  summarise(total =sum(n))

print("When referring to comments: ")
ratings_categories_tokens_reviews %>% 
  filter(rating_category==1) %>% arrange(desc(total)) %>% top_n(10)
ratings_categories_tokens_reviews %>% 
  filter(rating_category==2) %>% arrange(desc(total)) %>% top_n(10)


# Extract words in description
ratings_categories_tokens_description <- tokens_all_description %>% 
  left_join(rating_categories) %>% 
  group_by(rating_category,word) %>% 
  summarise(total =sum(n))

print("When referring to description: ")
ratings_categories_tokens_description %>% 
  filter(rating_category==1) %>% arrange(desc(total)) %>% top_n(10)
ratings_categories_tokens_description %>% 
  filter(rating_category==2) %>% arrange(desc(total)) %>% top_n(10)


# Extract words in transit
ratings_categories_tokens_transport <- tokens_all_comments %>% 
  left_join(rating_categories) %>% 
  group_by(rating_category,word) %>% 
  summarise(total =sum(n))

print("When referring to access to transit: ")
ratings_categories_tokens_transport %>% 
  filter(rating_category==1) %>% arrange(desc(total)) %>% top_n(10)
ratings_categories_tokens_transport %>% 
  filter(rating_category==2) %>% arrange(desc(total)) %>% top_n(10)


# 1. Is readibility of property description an important predictor of the rating?

# use readibility packages to score each description
library(qdap)
library(quanteda)
library(koRpus)
all_listings <- all_data_df %>%  
  select(listing_id,description,price)%>%
  unique(.) 

# Remove non-English words
all_listings <- all_listings %>% 
  mutate(review_language = cld2::detect_language(description,plain_text = TRUE)) %>%
  filter(review_language == "en")

readability_all <- data.frame()
for(i in 1:nrow(all_listings)){
  readability_h <- data.frame() 
  this_text <- iconv(all_listings$description[i])
  this_text <- removeNumbers(this_text)  # tm package
  this_text <- removePunctuation(this_text)
  tryCatch(readability_h <- flesch_kincaid(this_text),error=function(e){
    cat("Error parsing")
  })  
  if(!is.null(readability_h$Readability)){
    readability_h <- readability_h$Readability
    readability_h$listing_id <- all_listings$listing_id[i]
    readability_all <- bind_rows(readability_all,readability_h) 
  }
  
  print(i)
}
readability_des <- readability_all%>%
  select(listing_id,word.count,syllable.count,FK_grd.lvl,FK_read.ease)%>%
  left_join(all_listings)%>%
  left_join(rating_categories)


# 2. Is mentioning the host name is important?

library(ggplot2)
all_data_df$host_name_mentioned <- NA

# Use grepl to find whether comments include host name
for(i in 1:nrow(all_data_df)){
  tryCatch(check_h <- as.numeric(grepl(all_data_df$host_name[i],
                                       all_data_df$comments[i],
                                       ignore.case = T)),error=function(e){
                                         cat("ignore this one")
                                       })  
  all_data_df$host_name_mentioned[i] <- check_h
}

ggplot(subset(all_data_df,!is.na(host_name_mentioned)),
       aes(x=factor(host_name_mentioned),y=review_scores_rating))+geom_boxplot()

t.test(all_data_df$review_scores_rating~factor(all_data_df$host_name_mentioned))
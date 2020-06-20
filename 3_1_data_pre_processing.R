# Data from comments column is transformed 
# from raw into understandable format
# to be useful for further analysis

# Limit the review by removing too short and too long characters
all_data_df <- all_data_df %>% 
  filter(!(is.na(comments)))
all_data_df$review_length_chars <- nchar(all_data_df$comments)

# Check min, max, mean and quantile statistics
# To decide the limitation
max(all_data_df$review_length_chars)
min(all_data_df$review_length_chars)
mean(all_data_df$review_length_chars)
quantile_review <- quantile(all_data_df$review_length_chars)
all_data_df <- all_data_df%>% 
  arrange(desc(review_length_chars))
all_data_df <- all_data_df %>% 
  filter(between(review_length_chars,quantile_review[2],1000))

# Review the spread using histogram
hist(all_data_df$review_length_chars,breaks = 800)

# Limit the number of reviews included in each listing
to_remove_listing <- all_data_df %>% 
  group_by(listing_id) %>% 
  summarise(total = n()) %>% 
  filter(total<=5) %>% 
  pull(listing_id)

# Delete property listings with review less than 5
all_data_df <- all_data_df %>% 
  filter(!(listing_id %in% to_remove_listing)) 

# Remove digits and punctuations in comments
all_data_df$comments <- gsub('[[:digit:]]+',' ', all_data_df$comments)
all_data_df$comments <- gsub('[[:punct:]]+',' ', all_data_df$comments)

# Filter non-English reviews
library(cld2)
all_data_df <- all_data_df %>% 
  mutate(review_language = cld2::detect_language(comments,plain_text = TRUE))%>%
  filter(review_language == "en")

# Tokenise using parallelism
# Remove stop words using dictionaries and customised stop words
library(tidytext)
library(lexicon)
data(stopwords)
data(sw_fry_1000) #Fry's 1000 Most Commonly Used English Words
split_size <- 10000
tokens_list <- split(all_data_df,
                     rep(1:ceiling(nrow(all_data_df)
                                   /split_size), 
                         each=split_size,
                         length.out=nrow(all_data_df)))

# Create customised stop words
# Selection of stop words decided by contextual analysis
# All stop words transformed into lower case and combined into one object
tokens_all_comments <- data.frame()
property_name <- strsplit(unique(tolower(all_data_df$property_type))," ")
room_name <- as.character(unique(tolower(all_data_df$room_type)))
neighbourhood_name <-as.character(unique(tolower(all_data_df$neighbourhood)))
neighbourhood <- as.character(unique(tolower(all_data_df$neighbourhood_cleansed)))
host_name <- as.character(unique(tolower(all_data_df$host_name)))
cus_stopwords <- c("airbnb",neighbourhood_name,neighbourhood,host_name,this_city)

# Tokenisation and stop words removal
for(i in 1:length(tokens_list)){
  tokens_h <- tokens_list[[i]] %>% 
    unnest_tokens(word,comments) 
  
  tokens_h$word <-  tolower(tokens_h$word)
  
  tokens_h <- tokens_h %>%
    count(word,listing_id) %>%
    anti_join(stop_words) %>%
    filter(!(word %in% c(sw_fry_1000,cus_stopwords)))
  
  tokens_all_comments <- bind_rows(tokens_all_comments,tokens_h)
  
  print(i)
}

library(textstem)

# Lemmatize Words 
tokens_all_comments$word <- lemmatize_words(tokens_all_comments$word,
                                            dictionary = lexicon::hash_lemmas)

# Calculate token length
# Remove too short and too long tokens
tokens_all_comments$token_length <- nchar(tokens_all_comments$word)
tokens_all_comments %>% 
  group_by(token_length) %>% 
  summarise(total =n()) 
tokens_all_comments <- tokens_all_comments %>% 
  filter(between(token_length,3,15))  

# Inspect the descending distributions
tokens_all_comments %>% group_by(token_length) %>% 
  summarise(total =n()) %>% 
  arrange(desc(token_length))  

# Clean tokens by stop words removal 
tokens_all_comments <- tokens_all_comments%>%
  filter(!(word %in% cus_stopwords))

# Calculate TF-IDF
# Word as term, listing as document
tokens_all_comments_tf_idf <- tokens_all_comments %>% 
  bind_tf_idf(word,listing_id,n)

# Find TF-IDF cut-off value
# Filter important words using left and right trim
avg_comments_tf_idf <- mean(tokens_all_comments_tf_idf$tf_idf)
tokens_all_comments_tf_idf <- tokens_all_comments_tf_idf %>% 
  filter(between(tf_idf,avg_comments_tf_idf/8,2*avg_comments_tf_idf)) %>%
  arrange(desc(tf_idf))
hist(tokens_all_comments_tf_idf$tf_idf,breaks = 800)

# Cast tokens to a DTM (document-term matrix)
library(tm)
reviews_dtm <- tokens_all_comments%>% 
  cast_dtm(listing_id,word,n)

# Find 10 most important words
tokens_all_comments %>% 
  group_by(word) %>% 
  summarise(total =n()) %>% 
  arrange(desc(total)) %>% top_n(10)
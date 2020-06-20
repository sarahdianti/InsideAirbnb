# Data about property description is transformed 
# from raw into understandable format
# to be useful for further analysis

# Assume that description of property is the combination of
# summary, space, neighborhood_overview and notes column
# Columns listed above combined into one column and tokenise 
all_description <- all_data_df %>% 
  select(listing_id,summary,space,description,neighborhood_overview,notes) %>%
  unite("full_description",c("summary","space","description","neighborhood_overview","notes")) %>%
  unnest_tokens(sentence,full_description,token = "sentences")%>%
  unique(.)

all_description$sentence <- gsub("n't"," not",all_description$sentence)


# Remove digits and punctuations in sentences
all_description$sentence <- gsub('[[:digit:]]+',' ', all_description$sentence)
all_description$sentence <- gsub('[[:punct:]]+',' ', all_description$sentence)

# Filter non-English reviews
all_description<- all_description %>% 
  mutate(review_language = cld2::detect_language(sentence,plain_text = TRUE)) %>%
  filter(review_language == "en")

# Tokenisation and stop words removal
tokens_list <- split(all_description, 
                     rep(1:ceiling(nrow(all_description)
                                   /split_size), 
                         each=split_size,
                         length.out=nrow(all_description)))

tokens_all_description <- data.frame()

for(i in 1:length(tokens_list)){
  tokens_h <- tokens_list[[i]] %>% 
    unnest_tokens(word,sentence)
  
  tokens_h$word <- tolower(tokens_h$word)
  
  tokens_h<- tokens_h %>%
    count(word,listing_id) %>%
    anti_join(stop_words)%>%
    filter(!(word %in% cus_stopwords))
  
  print(i)
  
  tokens_all_description <- bind_rows(tokens_all_description,tokens_h)
}

# Lemmatize Words 
tokens_all_description$word   <- lemmatize_words(tokens_all_description$word,
                                                 dictionary = lexicon::hash_lemmas)

# Calculate token length
# Remove too short and too long tokens
tokens_all_description$token_length <- nchar(tokens_all_description$word)
tokens_all_description%>% 
  group_by(token_length) %>% 
  summarise(total =n()) 
tokens_all_description <- tokens_all_description %>% 
  filter(between(token_length,3,15)) 

# Inspect the descending distributions
tokens_all_description %>% 
  group_by(token_length) %>% 
  summarise(total =n()) %>% 
  arrange(desc(token_length))  

# Clean tokens by stop words removal 
tokens_all_description <- tokens_all_description %>%
  filter(!(word %in% cus_stopwords))

# Calculate TF-IDF
# Word as term, listing as document
tokens_all_description_tf_idf <- tokens_all_description %>% 
  bind_tf_idf(word,listing_id,n)  

#boxplot(tokens_all_description_tf_idf$tf_idf)

# Find TF-IDF cut-off value
# Filter important words using left and right trim
avg_description_tf_idf <- mean(tokens_all_description_tf_idf$tf_idf)
tokens_all_description_tf_idf <- tokens_all_description_tf_idf %>% 
  filter(between(tf_idf,avg_description_tf_idf/8,2*avg_description_tf_idf))%>%
  arrange(desc(tf_idf))
hist(tokens_all_description_tf_idf$tf_idf,breaks = 800)

# Cast tokens to a DTM (document-term matrix)
description_dtm <- tokens_all_description %>% 
  cast_dtm(listing_id,word,n)

# Find 10 most important words
tokens_all_description %>% 
  group_by(word) %>% 
  summarise(total =n()) %>% 
  arrange(desc(total)) %>% top_n(10)
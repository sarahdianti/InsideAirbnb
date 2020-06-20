# Data from transit column is transformed 
# from raw into understandable format
# to be useful for further analysis

transport <- all_data_df %>% 
  select(listing_id,transit) %>%
  filter(!(is.na(transit)))

# Remove digits and punctuations in transit column
transport$transit <- gsub('[[:digit:]]+',' ', transport$transit)
transport$transit <- gsub('[[:punct:]]+',' ', transport$transit)

# Filter non-English reviews
transport <- transport %>%
  mutate(review_language = cld2::detect_language(transit,plain_text = TRUE))%>%
  filter(review_language == "en")

# Tokenise using parallelism
# Remove stop words using dictionaries and customised stop words
tokens_list <- split(transport, 
                     rep(1:ceiling(nrow(transport)
                                   /split_size), 
                         each=split_size,
                         length.out=nrow(transport)))

tokens_transport <- data.frame()
for(i in 1:length(tokens_list)){
  tokens_h <- tokens_list[[i]] %>% 
    unnest_tokens(word,transit) 
  
  tokens_h$word <- tolower(tokens_h$word)
  
  tokens_h <- tokens_h %>%
    count(word,listing_id) %>%
    anti_join(stop_words)%>%
    filter(!(word %in% cus_stopwords))
  
  print(i)
  
  tokens_transport <- bind_rows(tokens_transport,tokens_h)
}

# Lemmatize Words 
tokens_transport$word   <- lemmatize_words(tokens_transport$word,
                                           dictionary = lexicon::hash_lemmas)

# Calculate token length
# Remove too short and too long tokens
tokens_transport$token_length <- nchar(tokens_transport$word)
tokens_transport%>% 
  group_by(token_length) %>% 
  summarise(total =n()) 
tokens_transport<- tokens_transport %>% 
  filter(between(token_length,3,15))  

# Inspect the descending distributions
tokens_transport %>% 
  group_by(token_length) %>% 
  summarise(total =n()) %>% 
  arrange(desc(token_length))

# Language detection to ensure only English tokens are kept
tokens_transport <- tokens_transport %>% 
  mutate(tokens_language = cld2::detect_language(word,plain_text = TRUE))
tokens_transport <- tokens_transport %>% 
  filter(tokens_language == "en")

# Clean tokens by stop words removal 
tokens_transport <- tokens_transport%>%
  filter(!(word %in% cus_stopwords))

# Calculate TF-IDF
# Word as term, listing as document
tokens_transport_tf_idf <- tokens_transport %>% 
  bind_tf_idf(word,listing_id,n)

# boxplot(tokens_transport_tf_idf$tf_idf)

# Find TF-IDF cut-off value
# Filter important words using left and right trim
avg_transport_tf_idf <- mean(tokens_transport_tf_idf$tf_idf)
tokens_transport_tf_idf <- tokens_transport_tf_idf %>% 
  filter(between(tf_idf,avg_transport_tf_idf/8,2*avg_transport_tf_idf))%>%
  arrange(desc(tf_idf))
hist(tokens_transport_tf_idf$tf_idf,breaks = 800)

# Cast tokens to a DTM (document-term matrix)
transport_dtm <- tokens_transport %>% 
  cast_dtm(listing_id,word,n)

# Find 10 most important words
tokens_transport %>% 
  group_by(word) %>% 
  summarise(total =n()) %>% 
  arrange(desc(total)) %>% top_n(10)
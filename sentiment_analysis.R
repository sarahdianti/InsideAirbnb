library(tidyr)
library(tidytext)

# On listing level:
# Calculate average ratings for each property
# Because one property has more than one rating
all_listings <- all_data_df %>%  
  select(listing_id,description,price,review_scores_rating) %>%
  unique(.)

# Determine that one property only have one price
listing_id_price <- all_listings %>% 
  select(listing_id,price) %>% 
  mutate(price = as.numeric(gsub("\\$|,","",price))) 

listing_id_rating <- all_listings %>%
  group_by(listing_id) %>% 
  select(listing_id,review_scores_rating)

tokens_all_comments <- tokens_all_comments%>%left_join(all_listings)%>%
  select(word,listing_id,review_scores_rating)

# Calculate sentiment using Bing Liu sentiment lexicon
# Sentiment for words is either positive or negative
tokens_all_comments %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment,listing_id) %>% spread(sentiment,n) %>%
  mutate(bing_liu_sentiment = positive-negative) %>%
  select(listing_id,bing_liu_sentiment) -> bing_liu_sentiment_listing

bing_liu_sentiment_listing <- bing_liu_sentiment_listing %>%
  left_join(listing_id_price)


# Calculate sentiment using NRC emotion lexicon
# NRC categorise words in binary fashion with eight basic emotions
tokens_all_comments %>% inner_join(get_sentiments("nrc")) %>% 
  count(sentiment,listing_id) %>% spread(sentiment,n)  -> emotions_nrc

emotions_nrc <- emotions_nrc %>% 
  left_join(listing_id_price) %>% 
  mutate(sentiment_nrc = positive-negative)

# Calculate sentiment using Afinn dictinary
# Afinn rate words for valence with value between -5 to +5
tokens_all_comments %>% inner_join(get_sentiments("afinn")) %>% 
  group_by(listing_id) %>%
  summarise(sentiment_affin = sum(value)) -> sentiment_affin

sentiment_affin <- sentiment_affin %>% 
  left_join(listing_id_price)


# combine each polarity together with listing_id and price
# Combine each polarity together with listing_id and price
all_together_sentiments <- bing_liu_sentiment_listing %>% 
  left_join(emotions_nrc) %>%
  left_join(sentiment_affin) %>%
  left_join(listing_id_rating)%>%
  na.omit()

# Remove listings with price of zero
all_together_sentiments <- all_together_sentiments %>%
  filter(price > 0)


## Regression Analysis

# 1. Find the effect of sentiment on price
# Consider sentiment as independent variable

# Bing Liu sentiment
model1 <- lm(log(price)~bing_liu_sentiment, 
             data=all_together_sentiments)
plot(model1)
summary(model1)

# NRC sentiment
model2 <- lm(log(price)~sentiment_nrc,
             data=all_together_sentiments)
summary(model2)

# Afinn sentiment
model3 <- lm(log(price)~sentiment_affin,
             data=all_together_sentiments)
summary(model3)

# Compare overall fit of different models using stargazer
stargazer::stargazer(model1,model2,model3,type = "text")


# 2. Find the effect of sentiment on rating
# Consider sentiment as independent variable

# Bing Liu sentiment
model_1 <- lm(log(review_scores_rating)~bing_liu_sentiment, 
              data=all_together_sentiments)
plot(model_1)
summary(model_1)

# NRC sentiment
model_2 <- lm(log(review_scores_rating)~sentiment_nrc,
              data=all_together_sentiments)
summary(model_2)

#Afinn sentiment
model_3 <- lm(log(review_scores_rating)~sentiment_affin,
              data=all_together_sentiments)
summary(model_3)

# Compare overall fit of different models using stargazer
stargazer::stargazer(model1,model2,model3,type = "text")
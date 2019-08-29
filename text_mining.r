#Lyric Analysis with NLP & Machine Learning with R

library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2)#creative visualizations


prince_orig <- read.csv("prince_raw_data.csv", stringsAsFactors = FALSE)
names(prince_orig)
head(prince_orig)

# Dataset Description(Which will be considered for the analysis)
# 1. X--Row number
# 2. Text--Actual lyrics
# 3. Song
# 4. year
# 5. peak--placement on the Billboard charts
# 6.US.Pop and US.R.B are peak chart positions for the US (Pop and R&B charts)

prince <- prince_orig %>% 
  select(lyrics = text, song, year, album, peak, 
         us_pop = US.Pop, us_rnb = US.R.B)
glimpse(prince)


str(prince[1,]$lyrics)

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

prince$lyrics <- sapply(prince$lyrics, fix.contractions)


# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
prince$lyrics <- sapply(prince$lyrics, removeSpecialChars)


# convert everything to lower case
prince$lyrics <- sapply(prince$lyrics, tolower)

#get facts about the full dataset
summary(prince)

#Since target is to look for song trends across time we will create buckets for years decade wise

#create the decade column
prince <- prince %>%
  mutate(decade = 
           ifelse(prince$year %in% 1978:1979, "1970s", 
                  ifelse(prince$year %in% 1980:1989, "1980s", 
                         ifelse(prince$year %in% 1990:1999, "1990s", 
                                ifelse(prince$year %in% 2000:2009, "2000s", 
                                       ifelse(prince$year %in% 2010:2015, "2010s", 
                                              "NA"))))))

#create the chart level column
prince <- prince %>%
  mutate(chart_level = 
           ifelse(prince$peak %in% 1:10, "Top 10", 
                  ifelse(prince$peak %in% 11:100, "Top 100", "Uncharted")))


#create binary field called charted showing if a song hit the charts at all
prince <- prince %>%
  mutate(charted = 
           ifelse(prince$peak %in% 1:100, "Charted", "Uncharted"))

#save the new dataset to .csv
write.csv(prince, file = "prince_new.csv")


songs_charted<-prince %>%
  filter(decade != "NA") %>%
  group_by(decade, charted) %>%
  summarise(number_of_songs = n())


ggplot(data=songs_charted)+geom_col(aes(x=decade,y=number_of_songs,fill=charted))

# From the graph we can know that the most active decade was 1980

charted_songs_over_time <- prince %>%
  filter(peak > 0) %>%
  group_by(decade, chart_level) %>%
  summarise(number_of_songs = n())

ggplot(data=charted_songs_over_time)+geom_col(aes(x=decade,y=number_of_songs,fill=chart_level))

# His most prolific decade was 1980. Most number of songs and most number of songs in top 10

#look at the full data set at your disposal
prince %>%
  group_by(decade, chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = chart_level), stat = "identity")


# Formated table

library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function

#quick peek of the songs that hit No. 1 on the charts
prince %>%
  filter(peak == "1") %>%
  select(year, song, peak) %>%
  arrange(year) %>%
  mutate(year = color_tile("lightblue", "lightgreen")(year)) %>%
  mutate(peak = color_tile("lightgreen", "lightgreen")(peak)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Prince's No. 1 Songs") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)


# Text Mining
#list of superfluous words that need to be removed manually:
undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

#unnest_tokens is used for tokenization
# stop words used to remove unwanted words 
# sample of stop words
# words with less than 3 alphabets also removed
head(sample(stop_words$word, 15), 15)

prince_words_filtered <- prince %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)



#Word Count

full_word_count <- prince %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,chart_level) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

#Top words

prince_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>% mutate(word = reorder(word, n))%>%ggplot() +
  geom_col(aes(word, n),fill='red')+coord_flip()


#wordcloud
prince_words_counts <- prince_words_filtered %>%
  count(word, sort = TRUE) 
  
wordcloud2(prince_words_counts[1:300, ], size = .5)


#TF-IDF

popular_tfidf_words <- prince %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)%>%
  count(chart_level, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, chart_level, n)
head(popular_tfidf_words)


top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf))

tfidf_words_decade <- prince %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(!word %in% undesirable_words & decade != 'NA') %>%
  filter(nchar(word) > 3) %>%
  count(decade, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, decade, n) %>%
  arrange(desc(tf_idf))



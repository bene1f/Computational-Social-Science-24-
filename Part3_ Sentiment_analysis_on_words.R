#Sentiment Analysis for each group
library(ggplot2)
library(stringr)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tidytext)
library(tidyverse)
library(quanteda) # for working with corpora
library(quanteda.textplots) # for plotting 'keyness'
library(readtext) # for getting documents and their info into a data frame
library(quanteda.textstats)
#require(quanteda.corpora)
require(ggplot2)
library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(readr)
library(utils)
library(tuber)
library(stringr)
library(dplyr)
library(stringi)
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
library(quanteda)
library(stm)


#lgbt wordcloud


lgbt <- subset(comments, Video == "LGBTQ")
# create corpus
corpusl <- Corpus(VectorSource(lgbt$textNoEmoji))
corpusl <- tm_map(corpusl, content_transformer(tolower)) #all lowercase
corpusl <- tm_map(corpusl, removeNumbers) #remove numbers
corpusl <- tm_map(corpusl, removeWords,c(tm::stopwords('en'), 'face', 'apple')) #remove english stopwords 
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), '-', ' ') #replace dashes with spaces
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), '–', ' ')
corpusl <- tm_map(corpusl, removePunctuation) #remove punctuation
corpusl <- tm_map(corpusl, stripWhitespace) #remove white spaces
#commentsSampled$Comment[40]
#content(corpus[[40]])
tdml <- TermDocumentMatrix(corpusl) 
ml <- as.matrix(tdml) #matrix
dfl <- data.frame(text = sapply(corpusl, as.character), stringsAsFactors = FALSE) #data.frame
vl <- sort(rowSums(ml),decreasing=TRUE) #term frequencies
dl <- data.frame(word = names(vl),freq=vl)
# wordcloud

wordcloud(words = dl$word, freq = dl$freq, min.freq = 15,
          max.words = 150, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))


#Bar plot for sentiment analysis

wordTokensl <- dfl %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% # delete words with numbers 
  filter(nchar(word) > 1) # delete words shorter than two letters
#load(file = 'sentiments_lexicons.Rdata') #load lexicons
bing =get_sentiments("bing")
#apply sentiment polarity to words
wordSentimentl <- wordTokensl %>% 
  inner_join(bing, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 
#aggregate them
word_sent_topl <- wordSentimentl %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 
#plot them, most frequent words by sentiment in BING

# Modify the color palette
colors <- c("#800020", "#00008B", "#7570b3") # Change to your preferred colors
# Plot with modified settings
lgbtqplot1= ggplot(word_sent_topl, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +  # geom_col() is equivalent to geom_bar(stat = "identity")
  scale_fill_manual(values = colors) +  # Set custom colors
  facet_grid(~sentiment, scales = 'free_x') +
  theme_minimal() +  # Change theme to minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),  # Adjust text angle and size
        strip.text = element_text(size = 10),  # Adjust facet label size
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  labs(title = "LGBT Group")  # Set plot title
data_sentl <- wordTokensl %>% 
  inner_join(bing) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n) %>% 
  mutate(diff = positive - negative) 
scoreSentimentl <- data.frame(t(data_sentl))
colnames(scoreSentimentl) <- 'value'
t(scoreSentimentl)
print(lgbtqplot1)


#Same for transgender data, wordcloud 



transgender <- subset(comments, Video == "Transgender")
# create corpus
corpusl <- Corpus(VectorSource(transgender$textNoEmoji))

corpusl <- tm_map(corpusl, content_transformer(tolower)) #all lowercase
corpusl <- tm_map(corpusl, removeNumbers) #remove numbers
corpusl <- tm_map(corpusl, removeWords,c(tm::stopwords('en'), 'face', 'apple')) #remove english stopwords 
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), '-', ' ') #replace dashes with spaces
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), '–', ' ')
corpusl <- tm_map(corpusl, removePunctuation) #remove punctuation
corpusl <- tm_map(corpusl, stripWhitespace) #remove white spaces

#commentsSampled$Comment[40]
#content(corpus[[40]])

tdml <- TermDocumentMatrix(corpusl) 
ml <- as.matrix(tdml) #matrix
dfl <- data.frame(text = sapply(corpusl, as.character), stringsAsFactors = FALSE) #data.frame
vl <- sort(rowSums(ml),decreasing=TRUE) #term frequencies
dl <- data.frame(word = names(vl),freq=vl)

# wordcloud

wordcloud(words = dl$word, freq = dl$freq, min.freq = 15,
          max.words = 150, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))

#Bar plot for sentiment analysis

wordTokensl <- dfl %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% # delete words with numbers 
  filter(nchar(word) > 1) # delete words shorter than two letters
#load(file = 'sentiments_lexicons.Rdata') #load lexicons
bing =get_sentiments("bing")
#apply sentiment polarity to words
wordSentimentl <- wordTokensl %>% 
  inner_join(bing, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 
#aggregate them
word_sent_topl <- wordSentimentl %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

#plot them, most frequent words by sentiment in BING

# Modify the color palette
colors <- c("#800020", "#00008B", "#7570b3") # Change to your preferred colors
# Plot with modified settings
transplot1=ggplot(word_sent_topl, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +  # geom_col() is equivalent to geom_bar(stat = "identity")
  scale_fill_manual(values = colors) +  # Set custom colors
  facet_grid(~sentiment, scales = 'free_x') +
  theme_minimal() +  # Change theme to minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),  # Adjust text angle and size
        strip.text = element_text(size = 10),  # Adjust facet label size
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  labs(title = "Transgender Group")  # Set plot title
data_sentl <- wordTokensl %>% 
  inner_join(bing) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n) %>% 
  mutate(diff = positive - negative) 
scoreSentimentl <- data.frame(t(data_sentl))
colnames(scoreSentimentl) <- 'value'
t(scoreSentimentl)
print(transplot1)

##racism, sentiment analysis, wordcloud

blackpeople <- subset(comments, Video == "BlackPeople")


# create corpus
corpusl <- Corpus(VectorSource(blackpeople$textNoEmoji))

corpusl <- tm_map(corpusl, content_transformer(tolower)) #all lowercase
corpusl <- tm_map(corpusl, removeNumbers) #remove numbers
corpusl <- tm_map(corpusl, removeWords,c(tm::stopwords('en'), 'face', 'apple')) #remove english stopwords 
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), '-', ' ') #replace dashes with spaces
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), '–', ' ')
corpusl <- tm_map(corpusl, removePunctuation) #remove punctuation
corpusl <- tm_map(corpusl, stripWhitespace) #remove white spaces

#commentsSampled$Comment[40]
#content(corpus[[40]])

tdml <- TermDocumentMatrix(corpusl) 
ml <- as.matrix(tdml) #matrix
dfl <- data.frame(text = sapply(corpusl, as.character), stringsAsFactors = FALSE) #data.frame
vl <- sort(rowSums(ml),decreasing=TRUE) #term frequencies
dl <- data.frame(word = names(vl),freq=vl)

# wordcloud
wordcloud(words = dl$word, freq = dl$freq, min.freq = 15,
          max.words = 150, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))

#Bar plot for sentiment analysis

wordTokensl <- dfl %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% # delete words with numbers 
  filter(nchar(word) > 1) # delete words shorter than two letters
#load(file = 'sentiments_lexicons.Rdata') #load lexicons
bing =get_sentiments("bing")
#apply sentiment polarity to words
wordSentimentl <- wordTokensl %>% 
  inner_join(bing, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 
#aggregate them
word_sent_topl <- wordSentimentl %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

#plot them, most frequent words by sentiment in BING

# Modify the color palette
colors <- c("#800020", "#00008B", "#7570b3") # Change to your preferred colors
# Plot with modified settings
bpplot1=ggplot(word_sent_topl, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +  # geom_col() is equivalent to geom_bar(stat = "identity")
  scale_fill_manual(values = colors) +  # Set custom colors
  facet_grid(~sentiment, scales = 'free_x') +
  theme_minimal() +  # Change theme to minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),  # Adjust text angle and size
        strip.text = element_text(size = 10),  # Adjust facet label size
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  labs(title = "Black People")  # Set plot title
data_sentl <- wordTokensl %>% 
  inner_join(bing) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n) %>% 
  mutate(diff = positive - negative) 
scoreSentimentl <- data.frame(t(data_sentl))
colnames(scoreSentimentl) <- 'value'
t(scoreSentimentl)
print(bpplot1)

#gender
gender <- subset(comments, Video == "Women")

# Assuming 'racism$textNoEmoji' is your character vector
corpusl <- Corpus(VectorSource(gender$textNoEmoji))

# Preprocessing steps
corpusl <- tm_map(corpusl, content_transformer(tolower)) # Convert to lowercase
corpusl <- tm_map(corpusl, removeNumbers) # Remove numbers
corpusl <- tm_map(corpusl, removeWords, c(stopwords('en'), 'face', 'apple')) # Remove English stopwords 
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), "-|–", " ") # Replace dashes with spaces
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), "'s", "") # Remove 's
corpusl <- tm_map(corpusl, removePunctuation) # Remove punctuation
corpusl <- tm_map(corpusl, stripWhitespace) # Strip whitespace

# Now 'corpusl' contains the preprocessed text data


#commentsSampled$Comment[40]
#content(corpus[[40]])

tdml <- TermDocumentMatrix(corpusl) 
ml <- as.matrix(tdml) #matrix
dfl <- data.frame(text = sapply(corpusl, as.character), stringsAsFactors = FALSE) #data.frame
vl <- sort(rowSums(ml),decreasing=TRUE) #term frequencies
dl <- data.frame(word = names(vl),freq=vl)

# wordcloud
wordcloud(words = dl$word, freq = dl$freq, min.freq = 15,
          max.words = 150, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))

wordTokensl <- dfl %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% # delete words with numbers 
  filter(nchar(word) > 1) # delete words shorter than two letters

#load(file = 'sentiments_lexicons.Rdata') #load lexicons

bing =get_sentiments("bing")
#apply sentiment polarity to words
wordSentimentl <- wordTokensl %>% 
  inner_join(bing, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 

#aggregate them
word_sent_topl <- wordSentimentl %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

#plot them, most frequent words by sentiment in BING
# Modify the color palette
colors <- c("#800020", "#00008B", "#7570b3") # Change to your preferred colors
# Plot with modified settings
genderplot1=ggplot(word_sent_topl, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +  # geom_col() is equivalent to geom_bar(stat = "identity")
  scale_fill_manual(values = colors) +  # Set custom colors
  facet_grid(~sentiment, scales = 'free_x') +
  theme_minimal() +  # Change theme to minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),  # Adjust text angle and size
        strip.text = element_text(size = 10),  # Adjust facet label size
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  labs(title = "Women")  # Set plot title
data_sentl <- wordTokensl %>% 
  inner_join(bing) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n) %>% 
  mutate(diff = positive - negative) 
scoreSentimentl <- data.frame(t(data_sentl))
colnames(scoreSentimentl) <- 'value'
t(scoreSentimentl)
print(genderplot1)


#now i do the same but for speakers 
disability <- subset(comments, Video == "Disability")

# Assuming 'racism$textNoEmoji' is your character vector
corpusl <- Corpus(VectorSource(disability$textNoEmoji))

# Preprocessing steps
corpusl <- tm_map(corpusl, content_transformer(tolower)) # Convert to lowercase
corpusl <- tm_map(corpusl, removeNumbers) # Remove numbers
corpusl <- tm_map(corpusl, removeWords, c(stopwords('en'), 'face', 'apple')) # Remove English stopwords 
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), "-|–", " ") # Replace dashes with spaces
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), "'s", "") # Remove 's
corpusl <- tm_map(corpusl, removePunctuation) # Remove punctuation
corpusl <- tm_map(corpusl, stripWhitespace) # Strip whitespace

# Now 'corpusl' contains the preprocessed text data


#commentsSampled$Comment[40]
#content(corpus[[40]])

tdml <- TermDocumentMatrix(corpusl) 
ml <- as.matrix(tdml) #matrix
dfl <- data.frame(text = sapply(corpusl, as.character), stringsAsFactors = FALSE) #data.frame
vl <- sort(rowSums(ml),decreasing=TRUE) #term frequencies
dl <- data.frame(word = names(vl),freq=vl)

# wordcloud
wordcloud(words = dl$word, freq = dl$freq, min.freq = 15,
          max.words = 150, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))

wordTokensl <- dfl %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% # delete words with numbers 
  filter(nchar(word) > 1) # delete words shorter than two letters

#load(file = 'sentiments_lexicons.Rdata') #load lexicons

bing =get_sentiments("bing")
#apply sentiment polarity to words
wordSentimentl <- wordTokensl %>% 
  inner_join(bing, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 

#aggregate them
word_sent_topl <- wordSentimentl %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

#plot them, most frequent words by sentiment in BING
# Modify the color palette
colors <- c("#800020", "#00008B", "#7570b3") # Change to your preferred colors
# Plot with modified settings
displot1=ggplot(word_sent_topl, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +  # geom_col() is equivalent to geom_bar(stat = "identity")
  scale_fill_manual(values = colors) +  # Set custom colors
  facet_grid(~sentiment, scales = 'free_x') +
  theme_minimal() +  # Change theme to minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),  # Adjust text angle and size
        strip.text = element_text(size = 10),  # Adjust facet label size
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  labs(title = "Disability Group")  # Set plot title
data_sentl <- wordTokensl %>% 
  inner_join(bing) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n) %>% 
  mutate(diff = positive - negative) 
scoreSentimentl <- data.frame(t(data_sentl))
colnames(scoreSentimentl) <- 'value'
t(scoreSentimentl)
print(displot1)


##now the same for black women 
bwomen <- subset(comments, Video == "BlackWomen")

# Assuming 'racism$textNoEmoji' is your character vector
corpusl <- Corpus(VectorSource(bwomen$textNoEmoji))

# Preprocessing steps
corpusl <- tm_map(corpusl, content_transformer(tolower)) # Convert to lowercase
corpusl <- tm_map(corpusl, removeNumbers) # Remove numbers
corpusl <- tm_map(corpusl, removeWords, c(stopwords('en'), 'face', 'apple')) # Remove English stopwords 
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), "-|–", " ") # Replace dashes with spaces
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), "'s", "") # Remove 's
corpusl <- tm_map(corpusl, removePunctuation) # Remove punctuation
corpusl <- tm_map(corpusl, stripWhitespace) # Strip whitespace

# Now 'corpusl' contains the preprocessed text data


#commentsSampled$Comment[40]
#content(corpus[[40]])

tdml <- TermDocumentMatrix(corpusl) 
ml <- as.matrix(tdml) #matrix
dfl <- data.frame(text = sapply(corpusl, as.character), stringsAsFactors = FALSE) #data.frame
vl <- sort(rowSums(ml),decreasing=TRUE) #term frequencies
dl <- data.frame(word = names(vl),freq=vl)

# wordcloud
wordcloud(words = dl$word, freq = dl$freq, min.freq = 15,
          max.words = 150, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))

wordTokensl <- dfl %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% # delete words with numbers 
  filter(nchar(word) > 1) # delete words shorter than two letters

#load(file = 'sentiments_lexicons.Rdata') #load lexicons

bing =get_sentiments("bing")
#apply sentiment polarity to words
wordSentimentl <- wordTokensl %>% 
  inner_join(bing, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 

#aggregate them
word_sent_topl <- wordSentimentl %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

#plot them, most frequent words by sentiment in BING
# Modify the color palette
colors <- c("#800020", "#00008B", "#7570b3") # Change to your preferred colors
# Plot with modified settings
bwomenplot1=ggplot(word_sent_topl, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +  # geom_col() is equivalent to geom_bar(stat = "identity")
  scale_fill_manual(values = colors) +  # Set custom colors
  facet_grid(~sentiment, scales = 'free_x') +
  theme_minimal() +  # Change theme to minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),  # Adjust text angle and size
        strip.text = element_text(size = 10),  # Adjust facet label size
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  labs(title = "Black Women")  # Set plot title
data_sentl <- wordTokensl %>% 
  inner_join(bing) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n) %>% 
  mutate(diff = positive - negative) 
scoreSentimentl <- data.frame(t(data_sentl))
colnames(scoreSentimentl) <- 'value'
t(scoreSentimentl)
print(bwomenplot1)

#now for control 

control <- subset(comments, Video == "Control")

# Assuming 'racism$textNoEmoji' is your character vector
corpusl <- Corpus(VectorSource(control$textNoEmoji))

# Preprocessing steps
corpusl <- tm_map(corpusl, content_transformer(tolower)) # Convert to lowercase
corpusl <- tm_map(corpusl, removeNumbers) # Remove numbers
corpusl <- tm_map(corpusl, removeWords, c(stopwords('en'), 'face', 'apple')) # Remove English stopwords 
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), "-|–", " ") # Replace dashes with spaces
corpusl <- tm_map(corpusl, content_transformer(str_replace_all), "'s", "") # Remove 's
corpusl <- tm_map(corpusl, removePunctuation) # Remove punctuation
corpusl <- tm_map(corpusl, stripWhitespace) # Strip whitespace

# Now 'corpusl' contains the preprocessed text data


#commentsSampled$Comment[40]
#content(corpus[[40]])

tdml <- TermDocumentMatrix(corpusl) 
ml <- as.matrix(tdml) #matrix
dfl <- data.frame(text = sapply(corpusl, as.character), stringsAsFactors = FALSE) #data.frame
vl <- sort(rowSums(ml),decreasing=TRUE) #term frequencies
dl <- data.frame(word = names(vl),freq=vl)

# wordcloud
wordcloud(words = dl$word, freq = dl$freq, min.freq = 15,
          max.words = 150, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))

wordTokensl <- dfl %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% # delete words with numbers 
  filter(nchar(word) > 1) # delete words shorter than two letters

#load(file = 'sentiments_lexicons.Rdata') #load lexicons

bing =get_sentiments("bing")
#apply sentiment polarity to words
wordSentimentl <- wordTokensl %>% 
  inner_join(bing, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 

#aggregate them
word_sent_topl <- wordSentimentl %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

#plot them, most frequent words by sentiment in BING
# Modify the color palette
colors <- c("#800020", "#00008B", "#7570b3") # Change to your preferred colors
# Plot with modified settings
controlplot1=ggplot(word_sent_topl, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +  # geom_col() is equivalent to geom_bar(stat = "identity")
  scale_fill_manual(values = colors) +  # Set custom colors
  facet_grid(~sentiment, scales = 'free_x') +
  theme_minimal() +  # Change theme to minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),  # Adjust text angle and size
        strip.text = element_text(size = 10),  # Adjust facet label size
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  labs(title = "Control Group")  # Set plot title
data_sentl <- wordTokensl %>% 
  inner_join(bing) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n) %>% 
  mutate(diff = positive - negative) 
scoreSentimentl <- data.frame(t(data_sentl))
colnames(scoreSentimentl) <- 'value'
t(scoreSentimentl)
print(controlplot1)

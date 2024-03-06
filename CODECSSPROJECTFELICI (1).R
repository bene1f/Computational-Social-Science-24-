#Needed library
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

#YouTubeAPI to get data 

#OQuwz64qsBM -lgbt
#AcARiiK5uY -transgender
#9EBkS2kE7uk -gender
#7TXEZ4tP06c -control
#36m1o-tM05g -disability
#sKvMxZ284AA -black women

#I use tuber and my credential from YouTubeAPI to access data directly from you tube
library(tuber)
yt_oauth(app_id = "649130249162-7ubohdck416q965m94inof4od9c16pqv.apps.googleusercontent.com",
         app_secret = "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", token = "")

#I use the option get_all_comments to get twe comments from each video
Control <- get_all_comments('7TXEZ4tP06c')
LGBTQ <- get_all_comments('OQuwz64qsBM')
Transgender <- get_all_comments('XAcARiiK5uY')
BlackPeople <- get_all_comments('oIZDtqWX6Fk')
Women <- get_all_comments('9EBkS2kE7uk')
Disability <- get_all_comments('36m1o-tM05g')
BlackWomen <- get_all_comments('sKvMxZ284AA') 

LGBTQ <- cbind('LGBTQ', LGBTQ$textOriginal)
colnames(lgbt) <- c('Video', 'Comments')

Transgender <- cbind('Transgender', Transgender$textOriginal)
colnames(Transgender) <- c('Video', 'Comments')

BlackPeople <- cbind('BlackPeople', BlackPeople$textOriginal)
colnames(BlackPeople) <- c('Video', 'Comments')

Women <- cbind('Women', Women$textOriginal)
colnames(Women) <- c('Video', 'Comments')

Control <- cbind('Control', Control$textOriginal)
colnames(Control) <- c('Video', 'Comments')

Disability <- cbind('Disability', Disability$textOriginal)
colnames(Disability) <- c('Video', 'Comments')

BlackWomen <- cbind('BlackWomen', BlackWomen$textOriginal)
colnames(BlackWomen) <- c('Video', 'Comments')

#now I bind all the small dataset to have a comprehensive one
comments = rbind(Control, Disability, BlackWomen, Transgender, LGBTQ, Women, BlackPeople)

#In this part I eliminate Emoji
library(stringr)
library(dplyr)
library(stringi)

comments=read.csv("C:/Users/ra47jam/Desktop/comments.csv")

#With the frollowing code you look at the last version of emoji
readLines("https://www.unicode.org/Public/emoji/latest/emoji-test.txt",
          encoding="UTF-8") %>%
  stri_subset_regex(pattern = "^[^#]") %>%
  stri_subset_regex(pattern = ".+") -> emoji

#then you can extract the emoji character
emoji %>%
  stri_extract_all_regex(pattern = "# *.{1,2} *") %>%
  stri_replace_all_fixed(pattern = c("*", "#"),
                         replacement = "",
                         vectorize_all=FALSE) %>%
  stri_trim_both() -> emoji.chars

#then you can extract the emoji description
emoji %>%
  stri_extract_all_regex(pattern = "#.*$") %>%
  stri_replace_all_regex(pattern = "^#.*?E\\d+\\.\\d+\\s+",
                         replacement = " ") -> emoji.descriptions

#finally you can replace emoji characters with their descriptions so that in the following phase you can take into account the meaning of the emoji
cNOe = stri_replace_all_regex(comments$Comments, 
                              pattern = emoji.chars,
                              replacement = emoji.descriptions,
                              vectorize_all=FALSE)

comments$textNoEmoji = cNOe

#Sentiment Analysis for each group
#lgbt wordcloud
library(tm)
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
library(wordcloud)
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
library(ggplot2)
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

library(tm)

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
library(wordcloud)
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
library(ggplot2)
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
library(tm)

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
library(wordcloud)
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
library(ggplot2)
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

library(tm)
library(stringr)

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
library(wordcloud)
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
library(ggplot2)
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

library(tm)
library(stringr)

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
library(wordcloud)
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
library(ggplot2)
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

library(tm)
library(stringr)

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
library(wordcloud)
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
library(ggplot2)
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

library(tm)
library(stringr)

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
library(wordcloud)
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
library(ggplot2)
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

#sentiment analyisis with comments not with words 
#transgender
### 2.8 **qdap polarity**
library(qdap)
library(ggplot2)
library(ggthemes)

#### polarity

pol<-polarity(transgender$textNoEmoji)
transplot2= ggplot(pol$all, aes(x=polarity ,
                    y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Transgender") 

data5 <- transgender
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(transplot2)

#lgbt
library(qdap)
library(ggplot2)
library(ggthemes)
#### polarity
pol<-polarity(lgbt$textNoEmoji)
lgbtplot2=ggplot(pol$all, aes(x=polarity ,
                    y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "LGBT") 

data5 <- lgbt
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(lgbtplot2)

#racism 
#### polarity
pol<-polarity(blackpeople$textNoEmoji)
racplot2=ggplot(pol$all, aes(x=polarity ,
                    y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Black People") 

data5 <- blackpeople
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(racplot2)

#gender

pol<-polarity(gender$textNoEmoji)
genplot2=ggplot(pol$all, aes(x=polarity ,
                    y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Women") 

data5 <- gender
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(genplot2)

#disability 
pol<-polarity(disability$textNoEmoji)
displot2=ggplot(pol$all, aes(x=polarity ,
                    y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Disability") 
data5 <- disability
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(displot2)


#black women 
pol<-polarity(bwomen$textNoEmoji)
bwplot2=ggplot(pol$all, aes(x=polarity ,
                    y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Black Women") 
data5 <- bwomen
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(bwplot2)

#control 
pol<-polarity(control$textNoEmoji)
controlplo2=ggplot(pol$all, aes(x=polarity ,
                    y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Control") 

data5 <- control
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(controlplo2)

##topic modelling for each group of topics 
#gender
library(quanteda)
library(stm)

#racism 

corpus.text.year <- corpus(blackpeople, text_field = "textNoEmoji")
corpus.text.year$"textNoEmoji" <- gsub("\\S+@\\S+|\\d+", "", corpus.text.year$"textNoEmoji")
#corpus.text.year <- corpus_group(corpus.text.year, groups = "Text")

para_corp <- corpus_reshape(corpus.text.year, to = "paragraphs") 
head(summary(para_corp))
length(para_corp)
table(ntoken(para_corp)) 
para_corp <- corpus_subset(para_corp, ntoken(para_corp) > 2)
para_corp <- para_corp %>%
  tokens(remove_punct = TRUE) %>%
  tokens(remove_symbols=TRUE) %>%
  tokens(remove_numbers = TRUE) %>%
  tokens(remove_separators = TRUE) %>%
  tokens(remove_url=TRUE) %>%
  tokens(split_hyphens = FALSE) %>%
  tokens(split_tags = FALSE) %>%
  tokens_remove(c("@darrenrooftop8328", "@shweppes742", "@name2l", "@starsinmyeyes__", "Ted", "waa", "1st", "#39","lily", "lilly", "TED","Talks", "ted", "talks", "TALKS", "s", "t","r","--", "re", "ze","didn", "can", padding = TRUE)) %>%
  tokens_remove(stopwords("english")) 


dfm_topic<-
  dfm(para_corp)

mod <- stm::stm(dfm_topic, K = 9, seed = 12345)


topic_labs <- labelTopics(mod)
str(topic_labs)


topic_labs <- apply(topic_labs$frex, 1, paste, collapse = "-")
topic_labs


racismplot3= plot(mod, type = "labels", labeltype = "frex")

#lgbt

corpus.text.year <- corpus(lgbt, text_field = "textNoEmoji")
corpus.text.year$"textNoEmoji" <- gsub("\\S+@\\S+|\\d+", "", corpus.text.year$"textNoEmoji")
#corpus.text.year <- corpus_group(corpus.text.year, groups = "Text")

para_corp <- corpus_reshape(corpus.text.year, to = "paragraphs") 
head(summary(para_corp))
length(para_corp)
table(ntoken(para_corp)) 
para_corp <- corpus_subset(para_corp, ntoken(para_corp) > 2)
para_corp <- para_corp %>%
  tokens(remove_punct = TRUE) %>%
  tokens(remove_symbols=TRUE) %>%
  tokens(remove_numbers = TRUE) %>%
  tokens(remove_separators = TRUE) %>%
  tokens(remove_url=TRUE) %>%
  tokens(split_hyphens = FALSE) %>%
  tokens(split_tags = FALSE) %>%
  tokens_remove(c("@darrenrooftop8328", "@shweppes742", "@name2l", "@starsinmyeyes__", "Ted", "waa", "1st", "#39","lily", "lilly", "TED","Talks", "ted", "talks", "TALKS", "s", "t","r","--", "re", "ze","didn", "can", padding = TRUE)) %>%
  tokens_remove(stopwords("english")) 


dfm_topic<-
  dfm(para_corp)

mod <- stm::stm(dfm_topic, K = 9, seed = 12345)
topic_props <- mod$theta # topic probabilities for each document
head(topic_props)

labelTopics(mod)


topic_labs <- labelTopics(mod)
str(topic_labs)


topic_labs <- apply(topic_labs$frex, 1, paste, collapse = "-")
topic_labs


lgbtplot3=plot(mod, type = "labels", labeltype = "frex")


#transgender

corpus.text.year <- corpus(transgender, text_field = "textNoEmoji")
corpus.text.year$"textNoEmoji" <- gsub("\\S+@\\S+|\\d+", "", corpus.text.year$"textNoEmoji")
#corpus.text.year <- corpus_group(corpus.text.year, groups = "Text")

para_corp <- corpus_reshape(corpus.text.year, to = "paragraphs") 
head(summary(para_corp))
length(para_corp)
table(ntoken(para_corp)) 
para_corp <- corpus_subset(para_corp, ntoken(para_corp) > 2)
para_corp <- para_corp %>%
  tokens(remove_punct = TRUE) %>%
  tokens(remove_symbols=TRUE) %>%
  tokens(remove_numbers = TRUE) %>%
  tokens(remove_separators = TRUE) %>%
  tokens(remove_url=TRUE) %>%
  tokens(split_hyphens = FALSE) %>%
  tokens(split_tags = FALSE) %>%
  tokens_remove(c("@darrenrooftop8328", "@shweppes742", "@name2l", "@starsinmyeyes__", "Ted", "waa", "1st", "#39","lily", "lilly", "TED","Talks", "ted", "talks", "TALKS", "s", "t","r","--", "re", "ze","didn", "can", padding = TRUE)) %>%
  tokens_remove(stopwords("english")) 


dfm_topic<-
  dfm(para_corp)

mod <- stm::stm(dfm_topic, K = 9, seed = 12345)
topic_props <- mod$theta # topic probabilities for each document
head(topic_props)

labelTopics(mod)


topic_labs <- labelTopics(mod)
str(topic_labs)


topic_labs <- apply(topic_labs$frex, 1, paste, collapse = "-")
topic_labs


transplot3=plot(mod, type = "labels", labeltype = "frex")
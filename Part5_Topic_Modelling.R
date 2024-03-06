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

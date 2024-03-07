#In this part I eliminate Emoji
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
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
yt_oauth(app_id = "",
         app_secret = "", token = "")

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

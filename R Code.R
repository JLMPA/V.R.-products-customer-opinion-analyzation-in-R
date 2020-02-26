###############################################################
######Querying Twitter for shares, like Trump tweets###########
###############################################################
#install the necessary packages
#install.packages("twitteR")
#install.packages("tm")

library(twitteR)
library(tm)
library(tidyverse)
library(tidytext)
library(dplyr)
library(twitteR)
library(tm)
library(tidyr)
library(reshape2)
library(scales)
library(wordcloud)
library(ggplot2)
library(reshape2)


#necessary file for Windows
setwd("C:/Users/jlmpa/OneDrive/Desktop/Rrrrr/Text Analytics/TwitterAPI")
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'IdIjTPwk9TpswUmirlecPubDJ'
consumer_secret <- 'o2nI9PuXsKdzsrlrmRJL2lmMyL7gS5nvjmAGtrEm3UJQyF4DLJ'
access_token <- '32834962-fdncrgeKkkaGY38zRWAnex1VHq0K9QH7CzBfPQWRs'
access_secret <- 'EPSe8NQFayP1V2vunqUygZTPqfjyY23DFbJNCANgtavFn'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

vr_oculus <- twitteR::searchTwitter('#vr + #oculus', n = 1000, since = '2018-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(vr_oculus)

vr_steam <- twitteR::searchTwitter('#vr + #steam', n = 1000, since = '2018-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(vr_steam)

vr_ps4 <- twitteR::searchTwitter('#vr + #ps4', n = 1000, since = '2018-06-01', retryOnRateLimit = 1e3)
f = twitteR::twListToDF(vr_ps4)

##setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)##

setwd("C:/Users/jlmpa/OneDrive/Desktop/Rrrrr/Text Analytics")

write.csv(d, "vroculusdf.csv")

vr_oculus <- read.csv(file = "vroculusdf.csv")
mydf_oculus <- data_frame(line=1:1000, text = as.character(vr_oculus$text))
tidy_oculusdf <- mydf_oculus %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_oculusdf %>%
  count(word, sort=TRUE)  

write.csv(e, "vrsteamdf.csv")

vr_steam <- read.csv(file = "vrsteamdf.csv")
mydf_steam <- data_frame(line=1:504, text = as.character(vr_steam$text))
tidy_steamdf <- mydf_steam %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_steamdf %>%
  count(word, sort=TRUE)

write.csv(f, "vrps4df.csv")

vr_ps4 <- read.csv(file = "vrps4df.csv")
mydf_ps4 <- data_frame(line=1:268, text = as.character(vr_ps4$text))
tidy_ps4df <- mydf_ps4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_steamdf %>%
  count(word, sort=TRUE)


frequency <- bind_rows(mutate(tidy_steamdf, common="steam"),
                       mutate(tidy_oculusdf, common= "Oculus"),
                       mutate(tidy_ps4df, common= "ps4")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(common, word) %>%
  group_by(common) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(common, proportion) %>%
  gather(common, proportion, `Oculus`, `ps4`)

ggplot(frequency, aes(x=proportion, y=`steam`, 
                      color = abs(`steam`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~common, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "steam", x=NULL)



####word cloud####

##for oculus##
tidy_oculus <- d %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))
tidy_oculus
tidy_oculus %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
tidy_oculus %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=500,
                   fixed.asp = TRUE,
                   title.size = 1)

##for steam##
tidy_steam <- e %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))
tidy_steam
tidy_steam %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
tidy_steam %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=500,
                   fixed.asp = TRUE,
                   title.size = 1)

##for ps4##
tidy_ps4 <- f %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))
tidy_ps4
tidy_ps4 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
tidy_ps4 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=500,
                   fixed.asp = TRUE,
                   title.size = 1)


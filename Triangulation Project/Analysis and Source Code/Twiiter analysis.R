# Load and install needed libraries
library(textdata)
library(XML)
library(tm)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(twitteR)
library(plotly)
library(scales)
library(GGally)
library(devtools)
library(roxygen2)

create("elfTri")


# Import Twitter dataset
twitter.data <- read_xlsx("Squid Game Twitter data 1.xlsx")
head(twitter.data)
name <- "Twitter_Activity"
colnames(twitter.data)[3] <- name
head(twitter.data)




## Frequent Terms ##

# removed unnecessary columns and rows. 
twitter.data <- twitter.data[,-8:-11]
head(twitter.data)
# Search for NAs
match(NA, twitter.data$Comments)
twitter.data <- na.omit(twitter.data)




# Use tidyverse and tidytext to find frequent terms associated with Squid Games 
token.data <- twitter.data %>% unnest_tokens(word, Comments) %>% anti_join(stop_words)
token.data <- token.data %>% count(word) %>% arrange(desc(n))
token.data




# Create customs stop words
custom.stop.words <- tribble(
  ~word, ~lexicon,
  "http", "CUSTOM", # Common URL term
  "twd", "CUSTOM", # Within top 15, I don't know what twd means, Better to leave it out I suppose.
  "t.co", "CUSTOM", # IDK what this term is but it's a common one regarding twitter data that is not useful.
  "de", "CUSTOM", # Also in top 15, I don't think it's useful.
  "13", "CUSTOM",
  "16", "CUSTOM",
  "im", "CUSTOM" # Not needed
)
# Bind custom to original
new.stop.words <- stop_words %>% bind_rows(custom.stop.words)
# Apply new stop words
token.data <- twitter.data %>% unnest_tokens(word, Comments) %>% anti_join(new.stop.words)
token.data




# Visualize top 15 most frequent terms with a graph
graph.token <- token.data %>% count(word) %>% arrange(desc(n))%>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + 
  geom_col(aes(fill = word)) + labs( y = "Frequency", x = "Terms", title = "Top 15 Most Frequent Terms") + coord_flip() + 
  theme(legend.position = "none")
graph.token






## Sentiment Analysis ##

# Use sentiment data package to find out what overall sentiment is of the data collected
sentiment.data <- token.data %>% inner_join(get_sentiments("nrc"))
sentiment.data
sentiment.data %>% count(sentiment) %>% arrange(desc(n)) # positive seems to be the most but it's still all out of context

# Visualize the terms associated with negative and positive
sentiment.count <- token.data %>% inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("positive", "negative")) %>% count(word, sentiment) %>% 
  group_by(sentiment) %>% top_n(10, n) %>% ungroup() %>% mutate(
    word2 = fct_reorder(word, n)
  )

sentiment.count %>% arrange(desc(n))

Graph.sentiment <- ggplot(sentiment.count, aes(x = word2, y = n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scale = "free") +
  coord_flip()  + labs(y = "Frequency", x = "Terms")

Graph.sentiment














### Graphing ###



# Tidy up Min and Max for graph : Engagement
twitter.data[which.max(twitter.data$Engagement),] # major outlier skewed graph
match("bigbertha12", twitter.data$Username)
twitter.data <- twitter.data[-160,]
rownames(twitter.data) <- NULL

mean(twitter.data$Engagement)


# Tidy up Min and Max for graph : Retweets
twitter.data[which.max(twitter.data$Twitter_Activity),] # another major outlier
match("danaiguriratwd", twitter.data$Username)
twitter.data <- twitter.data[-704,]
rownames(twitter.data) <- NULL

mean(twitter.data$Retweet)


# Tidy up Min and Max for graph : Impressions
twitter.data[which.max(twitter.data$Impressions),]
match("naaaaaaateb", twitter.data$Username)
twitter.data <- twitter.data[-644,]
rownames(twitter.data) <- NULL

mean(twitter.data$Impressions)



# Bubble Chart/Graph for Engagement, Impressions, and retweets
Bubble <- ggplot(twitter.data, aes(x = Twitter_Activity, y = Engagement)) +
  geom_point(aes(size = Impressions), alpha = 0.3, color = "blue") +
  xlim(0, 5000) + ylim(100, 2000)  + scale_size(range = c(2, 10), limits = c(0,25000)) +
  labs(title = "Squid Game through Peirce's sign theory")

Bubble # Overview

Bubble2 <- ggplot(twitter.data, aes(x = Twitter_Activity, y = Engagement)) +
  geom_point(aes(size = Impressions), alpha = 0.3, color = "blue") +
  xlim(0, 1000) + ylim(100, 1000)  + scale_size(range = c(2, 10), limits = c(0,25000)) +
  labs(title = "Squid Game through Peirce's sign theory") 

Bubble2 # Zoomed in on Corner

Bubble3 <- ggplot(twitter.data, aes(x = Twitter_Activity, y = Engagement)) +
  geom_point(aes(size = Impressions), alpha = 0.3) +
  xlim(0, 500) + ylim(100, 700)  + scale_size(range = c(2, 10), limits = c(0,25000)) +
  labs(title = "Squid Game through Peirce's sign theory")

Bubble3 # Zoomed in more on Corner

### Analysis ###

# High Retweet seems to be to have a lower engagement level overall and there also seems to be less 
# amount of people who get a high number of re tweets. Some likes also seem to be small associated with high retweet.
# maybe it was controversial and was retweeted for discussion.

# A higher engagement seems to associate with less retweets. Most engagement seems to center around 
# y = 600ish and below and x = 375ish and below. 

# I don't see a clear pattern for likes other than a larger percentage of large likes seem
# to congregate towards the middle where retweet and engagement meet up.






### BAR GRAPH ### 

# dataframe to keep it more simple for me
focus <- twitter.data[, c(3:5)]

retweetSum <- colSums(focus[,1])
retweetSum

ImpresSums <- colSums(focus[,2])
ImpresSums

EngagSum <- colSums(focus[,3])
EngagSum

allSums <- c(retweetSum, ImpresSums, EngagSum)
names <- c("Twitter_Activity", "Impressions", "Engagement")

barDF <- data.frame(allSums, names)


# First graph shows all total but Retweets is 30+ million thus skewing the graph
Bar <- ggplot(barDF, aes(x = names, y = allSums, fill = names)) + geom_col() +
  labs(x = "Variable Types", y = "Total Sums", title = "Squid Game through Peirce's sign theory") +
  scale_y_continuous(label = comma) + theme(legend.position = "none")
Bar

# Sceond graph limits the y - axis to 3,000,000 in order to see other variables.
Bar2 <- ggplot(barDF, aes(x = names, y = allSums, fill = names)) + geom_col() +
  labs(x = "Variable Types", y = "Total Sums", title = "Squid Game through Peirce's sign theory") +
  scale_y_continuous(limits=c(0,3000000), oob = rescale_none, labels = comma) + theme(legend.position = "none")
Bar2


### Analysis ###

# For some reason or another retweeting seems to be way more popular instead of liking a tweet. I use twitter 
# infrequently so I can't say too much on the subject but an educated guess by me would be that people only want
# to like something if they want to add it to their liked section on their twitter page. To use as a "bookmark" 
# so to speak. So that they can find whatever that they liked quickly rather than being cluttered with other stuff 
# the kinda liked but liked it anyways. I believe the vast difference between retweets and likes is causing the 
# engagement variable to be what it is, small. 

# You also don't need to like something to retweet it. Example would be someone posted a very controversial post
# e.g., a racist post, and the person rewteeting wants to show other people this controversial tweet as a
# "hey look what this guy said about ____ ".







### LINE GRAPH ###

# remove top ten Retweets for graph
RetweetOrder <- twitter.data %>% arrange(desc(Twitter_Activity))
line.df <- twitter.data

match("tvbuddiestoo", line.df$Username)
line.df <- line.df[-737,]
rownames(line.df) <- NULL

match("thestarwarsguru", line.df$Username)
line.df <- line.df[-516,]
rownames(line.df) <- NULL

match("jiornavyluna", line.df$Username)
line.df <- line.df[-638,]
rownames(line.df) <- NULL

match("tivo", line.df$Username)
line.df <- line.df[-74,]
rownames(line.df) <- NULL

match("samie_fetterolf", line.df$Username)
line.df <- line.df[-658,]
rownames(line.df) <- NULL

match("yvette_xc", line.df$Username)
line.df <- line.df[-473,]
rownames(line.df) <- NULL

match("adamda", line.df$Username)
line.df <- line.df[-4,]
rownames(line.df) <- NULL

match("shill4health", line.df$Username)
line.df <- line.df[-758,]
rownames(line.df) <- NULL

match("madfashionista", line.df$Username)
line.df <- line.df[-73,]
rownames(line.df) <- NULL

match("gillianjacobs", line.df$Username)
line.df <- line.df[-83,]
rownames(line.df) <- NULL

line.df %>% arrange(desc(Retweet))

line <- ggplot(line.df, aes(x = Impressions, y = Engagement, color = Twitter_Activity)) + geom_line(size = 1) +
  labs(title = "Squid Game through Peirce's sign theory") + scale_color_continuous(high = "black", low = "orange")
line

line2 <- line + scale_x_continuous(limits = c(0, 20000))
line2

# Number of Data entries minus amount of data points where retweets >= 1000
908 - 138
round(770 / 908 * 100) # total % of data points under 1000 retweets

line3 <- line + scale_x_continuous(limits = c(0, 3000)) + 
  scale_color_continuous(high = "black", low = "orange", limits = c(0, 1000))
line3

line4 <- line + scale_x_continuous(limits = c(0, 1250)) + 
  scale_color_continuous(high = "black", low = "orange", limits = c(0, 1000))
line4



### Analysis ###

# From the overview grpah #1 as the impressions climbed to soaring numbers the engagement was next to non-exsistant
# Same goes form the rewtees albit it's hard to see from so zoomed out. What you can see is the high concentration 
# of Engagement centers around 3000ish and where less impressions were at is also the highest number of retweet concentrated

# you can also see that the very highest level of engagement, there seems to be very little retweets and the likes
# never reach past 1,000. I believe this to be the "silent majority" where the more of the average person will be engaging more
# in lower retweet and like count as this seems to be the most available.

# My take is that, from experience, if you come across a popular post i.e., big like and retweet count, you cannot
# engage with it all that much as there would be so many comments your's will just be lost in the sea. But at a lower 
# count there's better engagement because your comment can be seen and interacted more.













### CORRELAGRAM ###


# Add Pierce title
ggpairs(focus, title="Squid Game through Peirce's sign theory") 



### Analysis ###

# As per my other graphs you can see that 1.) there is extreme outliars with each category. This proves
# my idea of a 'silent majority' as each category plotted by itself you can see the area portrayed does not 
# go very much out at all. Thus the larger grouping has relatively small counts.














graph.sentiment <- function(x) {
  custom.stop.words <- tribble(
    ~word, ~lexicon,
    "http", "CUSTOM", # Common URL term
    "twd", "CUSTOM", # Within top 15, I don't know what twd means, Better to leave it out I suppose.
    "t.co", "CUSTOM", # IDK what this term is but it's a common one regarding twitter data that is not useful.
    "de", "CUSTOM", # Also in top 15, I don't think it's useful.
    "13", "CUSTOM",
    "16", "CUSTOM" # Not needed
  )
  
  # Bind custom to original
  new.stop.words <- stop_words %>% bind_rows(custom.stop.words)
  # Apply new stop words
  y <- x %>% unnest_tokens(word, Comments) %>% anti_join(new.stop.words)
  
  
  z <- y %>% inner_join(get_sentiments("nrc")) %>% 
    filter(sentiment %in% c("positive", "negative")) %>% count(word, sentiment) %>% 
    group_by(sentiment) %>% top_n(10, n) %>% ungroup() %>% mutate(
      word2 = fct_reorder(word, n))
  
  graph <- ggplot(z, aes(x = word2, y = n, fill = sentiment)) + 
    geom_col(show.legend = FALSE) +facet_wrap(~sentiment, scale = "free") +
    coord_flip() + labs(y = "Frequency", x = "Terms")
  
  return(graph)
}

graph.sentiment(twitter.data)









graph.token <- function(x) {
  custom.stop.words <- tribble(
    ~word, ~lexicon,
    "http", "CUSTOM", # Common URL term
    "twd", "CUSTOM", # Within top 15, I don't know what twd means, Better to leave it out I suppose.
    "t.co", "CUSTOM", # IDK what this term is but it's a common one regarding twitter data that is not useful.
    "de", "CUSTOM", # Also in top 15, I don't think it's useful.
    "13", "CUSTOM",
    "16", "CUSTOM",
    "im", "CUSTOM" # Not needed
  )
  
  # Bind custom to original
  new.stop.words <- stop_words %>% bind_rows(custom.stop.words)
  # Apply new stop words
  y <- x %>% unnest_tokens(word, Comments) %>% anti_join(new.stop.words)
  
  # ggplot graph 15 most common
  z <- y %>% count(word) %>% arrange(desc(n))%>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + 
    geom_col(aes(fill = word)) + labs( y = "Frequency", x = "Terms", title = "Top 15 Most Frequent Terms") + coord_flip() + 
    theme(legend.position = "none")
  
  return(z)
}

graph.token(twitter.data)









graph.Pierce.trio <- function(x) {
  
  
  
  
  
  
  
}
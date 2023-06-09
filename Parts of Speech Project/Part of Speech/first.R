setwd("~/Work/Professor Friedman Project/Part of Speech")
# libraries
library(readxl)
library(writexl) # summary stats table
library(tidyverse)
library(tidytext) # text cleaning
library(SnowballC) # word stemming
library(udpipe) # word lemmatizing
library(lubridate) # datetime conversion
library(topicmodels) # r-squared calculation
library(stm) # dtm model matrix 
library(scales)
library(waffle)
library("forcats")
library("GGally")
#devtools::install_github('jeromefroe/circlepackeR')
#library(circlepackeR)
#library(data.tree)
#library(knitr) 

## Data Prep

# load pre-trained english model from UDpipe pkg
#m_eng <- udpipe::udpipe_download_model(language = 'english-ewt')
m_eng <- udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')

# read in articles
articles <- read_excel('Citation summary.xlsx', col_names = TRUE)

# check for NAs
colSums(is.na(articles)) # 7 missing content types

# add words to existing 'stop' df
my_words <- c("'","'r","2d","3d","'","’r","'s","’re","10","2.0","2000","2014",
              "acm","we’re","ti","'s")
lexicon <- rep('custom', times=length(word))
mystops <- data.frame(my_words, lexicon)
names(mystops) <- c('word','lexicon')
# final stop word list
stop_words <- stop_words %>%
  bind_rows(mystops)

# create tidy corpus
tidy_articles <- articles %>%
  mutate(line = row_number()) %>%
  mutate(Type = replace(Type, Type == 'Article', 'Journal Article')) %>%
  drop_na() %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  anti_join(stop_words, by = 'word') %>%
  filter(str_length(word) >= 3)
tidy_articles

# stem article tokens
tidy_articles <- tidy_articles %>%
  mutate(word = wordStem(word))

# save tidy corpus for outside use
#write_xlsx(tidy_articles, 'article_corpus.xlsx', col_names = TRUE)

# tokenize, parse, and POS tag article titles
article_semantics <- udpipe_annotate(m_eng, x = tidy_articles$word) %>%
  as.data.frame() %>%
  # store relevant data
  select(token, upos, xpos) %>% # remove numerical tokens found in df
  rename(pos = upos) %>%
  unnest_tokens(word, token) # assert punctuation is removed


# unique(article_semantics$word)
# which(article_semantics$word == "r")
# article_semantics <- article_semantics[-1339,]
# rownames(article_semantics) <- NULL

# compare stemmed vs lemmatized topics
#comparison <- data.frame(tidy_articles$word, article_semantics$lemma) %>%
#  rename(stemmed = tidy_articles.word, lemma = article_semantics.lemma)
#print(comparison, n = nrow(comparison))

# add POS to main df
tidy_articles <- tidy_articles %>%
  mutate(word = article_semantics$word) %>%
  mutate(pos = article_semantics$pos) %>%
  filter(str_length(word) > 2) # ensure non-words are removed prior to analysis

# convert Year to numeric
tidy_articles <- tidy_articles %>%
  mutate(Year = as.numeric(Year)) 



tidy_articlesC <- tidy_articles %>% filter(Type == "Conference") 
tidy_articlesC$Type <- gsub('Conference', 'Conference Paper', tidy_articlesC$Type)

tidy_articlesCA <- tidy_articles %>% filter(Type == "Conference Article")
tidy_articlesCA$Type <- gsub(' ', '', tidy_articlesCA$Type)
tidy_articlesCA$Type <- gsub('ConferenceArticle', 'Conference Paper', tidy_articlesCA$Type)

tidy_articlesTemp <- tidy_articles %>% filter(Type %in% c("Book", "Book chapter", "Journal Article"))

tidy_articles <- data.frame(rbind(tidy_articlesC, tidy_articlesTemp, tidy_articlesCA))

# group years into 7 year intervals
# tidy_articles <- tidy_articles %>% 
#   mutate(Year = cut(Year, seq(1987, 2024, 7))) %>%
#   #levels(tidy_articles$Year)
#   # check for missing dates
#   #sum(is.na(tidy_articles$Year)) # no more NAs, so we move on...
#   mutate(Year = recode(Year, '(1987,1994]' = '88-94',
#                        '(1994,2001]' = '94-01',
#                        '(2001,2008]' = '01-08',
#                        '(2008,2015]' = '08-15',
#                        '(2015,2022]' = '15-20'))

## Text Modeling

# generate document term matrix
tidy_article_dtm <- tidy_articles %>%
  count(Title, word) %>%
  cast_dtm(Title, word, n)
tidy_article_dtm

# model words for r-squared calculation (based on Dr. Friedman's R code)
article_lda <- LDA(tidy_article_dtm, k = 10)
terms(article_lda, 5)

article_dfm <- tidy_articles %>%
  count(Title, word, sort = TRUE) %>%
  cast_dfm(Title, word, n)
topic_mat <- stm(article_dfm, K = 10, init.type = "Spectral")

## Analysis

# summary statistics for single word frequencies (keep unique instances)
token_stats <- tidy_articles %>%
  group_by(Type, Year) %>%
  count(word, sort = TRUE) %>%
  summarize(word_count = n(),
            word_avg = mean(n),
            word_sd = sd(n)
            # coefficient of determination (r-squared)
            #word_rsquared = cor(topic_mat)^2 # must be in matrix form
  )
token_stats <- token_stats %>%
  pivot_longer(cols = word_count:word_sd) %>%
  pivot_wider(names_from = Year, values_from = value) %>%
  select(-name)
token_stats

# save stats in workspace
write_xlsx(token_stats, 'Tables/token_summary.xlsx', col_names = TRUE)

# determine most prevalent POS to plot
tidy_articles %>% 
  group_by(pos) %>%
  count(pos, sort = TRUE)



toke_sum <- read_xlsx("token_summary2.xlsx")

ggplot(toke_sum, aes(x = SD, y = Mean, color = Type, size = Count)) + geom_point() +
  labs(title = "Tokens Summarized: Mean vs SD by Document Type and Count",
       x = 'Standard Deviation',
       color = "Document Type")



toke_sum %>% select(Count, Mean, SD) %>%  ggpairs(toke_sum) + labs(title  = "Token Summary")










# visualize word count by Year 
tidy_articles %>%
  filter(pos %in% c('NOUN','ADJ','VERB','ADV')) %>%
  group_by(pos) %>%
  count(word, sort = TRUE) %>%
  slice_max(word, n = 10) %>%
  ungroup %>%
  mutate(word = reorder_within(word, n, pos)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~pos, scales = 'free_y') +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0,25), oob = rescale_none) +
  labs(x = 'Unique Terms',
       y = 'Frequency',
       title = 'Most Frequent Terms by Parts-of-Speech',
       caption = '*Unique Term, visual, extends to past 300*')


# Time of POS frequency
tidy_articles %>% filter(pos %in% c('NOUN','ADJ','VERB','ADV')) %>%
  group_by(pos, Year) %>%
  count(word, sort = TRUE) %>%
  ggplot(aes(x = Year, y = n, fill = pos)) +
  geom_col() + labs(x = "Year",
                    y = "Frequency",
                    title = "Frequency of the Parts-of-Speech of Terms by Year",
                    fill = "Parts-of-Speech")

  ggplot(POS, aes(x = Type, y = n, fill = pos)) +
    coord_flip() +
  geom_col() + labs(x = "Type of Document",
                    y = "Frequency",
                    title = "Frequency of the Parts-of-Speech of Terms by Document Type",
                    fill = "Parts-of-Speech")



# Created df for tokenized parts of speech count for easier coding
POS <- tidy_articles %>%
  filter(pos %in% c('NOUN','ADJ','VERB','ADV')) %>%
  group_by(pos, Year, Type) %>%
  count(word, sort = TRUE) %>%
  slice_max(word, n = 5)
POS


POS0 <- tidy_articles %>%
  filter(pos %in% c('NOUN','ADJ','VERB','ADV')) %>%
  group_by(pos) %>%
  count(word, sort = TRUE) %>%
  slice_max(word, n = 5)

POS2 <- POS %>%
  mutate(Year = cut(Year, seq(1987, 2024, 7))) %>%
  #levels(tidy_articles$Year)
  # check for missing dates
  #sum(is.na(tidy_articles$Year)) # no more NAs, so we move on...
  mutate(Year = recode(Year, '(1987,1994]' = "'88 - '94",
                       '(1994,2001]' = "'94 - '01",
                       '(2001,2008]' = "'01 - '08",
                       '(2008,2015]' = "'08 - '15",
                       '(2015,2022]' = "'15 - '20"))


# Desinty Plots
ggplot(POS, aes(n)) + geom_density(aes(fill = factor(pos)), alpha=0.7)  + 
  scale_x_continuous(limits = c(0,6)) + labs(title = "Density of Terms by Parts-of Speech",
                                             x = "Frequency of Terms",
                                             fill = "Parts-of-Speech",
                                             y = "Density of Terms")

ggplot(POS, aes(n)) + geom_density(aes(fill = factor(Type)), alpha=0.7)  + 
  scale_x_continuous(limits = c(0,6)) + labs(title = "Density of Terms by Document Type",
                                             x = "Frequency of Terms",
                                             fill = "Document Type",
                                             y = "Density of Terms")

ggplot(POS2, aes(n)) + geom_density(aes(fill = factor(Year)), alpha=0.7)  + 
  scale_x_continuous(limits = c(0,6)) + labs(title = "Density of POS Word Count by Grouped Year",
                                             x = "Frequency of Terms",
                                             fill = "Year (Grouped)",
                                             y = "Density of Terms")


# Histogram
ggplot(POS, aes(x = pos)) + geom_bar(aes(fill = Type)) +
  labs(title = "Frequency of Parts-of-Speech by Document Type",
       x = "Document Type",
       y = "Frequency",
       fill = "Parts-of-Speech")

ggplot(POS2, aes(x = pos)) + geom_bar(aes(fill = Year)) + 
  labs(title = "Frequency of Parts-of-Speech by Grouped Year",
       x = "Parts-of-Speech",
       y = "Frequency",
       fill = "Year (Grouped)")



# Waffle Plot
var <- POS$pos  # the categorical data 
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var)))) #### Question this
categ_table

df$pos <- factor(rep(names(categ_table), categ_table)) ## this too

ggplot(df, aes(x = x, y = y, fill = pos)) + geom_tile(color = "black", size = .5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = "reverse") + 
  labs(title="Waffle Chart: Distribution of Used Parts-of-Speech", fill="Parts-of-Speech") + theme(axis.text = element_blank(),
                                                             axis.title = element_blank(),
                                                             axis.ticks = element_blank(),
                                                             legend.position = "right")

var2 <- POS$Type  # the categorical data 
nrows <- 9
df2 <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table2 <- round(table(var2) * ((nrows*nrows)/(length(var2)))) #### Question this
categ_table2

df2$Type <- factor(rep(names(categ_table2), categ_table2)) ## this too

ggplot(df2, aes(x = x, y = y, fill = Type)) + geom_tile(color = "black", size = .5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = "reverse") + 
  labs(title="Waffle Chart: Distribution of Document Type", fill="Document Type") + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")


# var3 <- POS$word  # the categorical data 
# nrows <- 50
# df3 <- expand.grid(y = 1:nrows, x = 1:nrows)
# categ_table3 <- round(table(var3) * ((nrows*nrows)/(length(var3)))) #### Question this
# categ_table3
# 
# df3$word <- factor(rep(names(categ_table3), categ_table3)) ## this too
# 
# ggplot(df3, aes(x = x, y = y, fill = Type)) + geom_tile(color = "black", size = .5) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0), trans = "reverse") + 
#   labs(title="Waffle Chart: Distribution of Document Type", fill="Document Type") + 
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "right")








# Boxplots                                     
ggplot(POS, aes(x = factor(Type), y = n)) + 
  geom_boxplot(aes(fill = pos, color = pos), show.legend = F) + coord_flip() + facet_wrap(~pos, scales = 'free_y') +
  guides(color = F) +
  labs(title = "Distribution of POS Term's\nFrequency by Document Type",
       x = "Document Type",
       y = "Frequency of Terms",
       fill = "Parts-of-Speech")

ggplot(POS2, aes(x = factor(Year), y = n)) + 
  geom_boxplot(aes(fill = pos, color = pos), show.legend = F) + coord_flip() + facet_wrap(~pos, scales = 'free_y') +
  guides(color = F) +
  labs(title = "Distribution of POS Term's\nFrequency by Grouped Years",
       x = "Years (Grouped)",
       y = "Frequency of Terms",
       fill = "Parts-of-Speech")
 


# by Type
tidy_articles %>%
  group_by(Type) %>%
  count(word, sort = TRUE) %>%
  slice_max(word, n = 10) %>%
  ungroup %>%
  mutate(word = reorder_within(word, n, Type)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Type, scales = 'free_y') +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0,15), oob = rescale_none) +
  labs(x = 'Unique Terms',
       y = 'Frequency',
       title = 'Most Frequent Terms by Document Type')










## BIGRAMS

# store tokenized bigram df
tidy_bigrams <- tidy_articles %>%
  unnest_tokens(bigrams, Title, token = 'ngrams', n = 2)
tidy_bigrams

tidy_bigrams_split <- tidy_bigrams %>%
  separate(bigrams, c('word1', 'word2'), sep = ' ')
tidy_bigrams_split

tidy_bigrams_split <- tidy_bigrams_split %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
tidy_bigrams_split

# check for NAs
which(is.na(tidy_bigrams_split$word1))
tidy_bigrams_split[1350,]
# both are missing, remove entire row
tidy_bigrams_split <- tidy_bigrams_split %>%
  drop_na()

sum(is.na(tidy_bigrams_split$word1))
sum(is.na(tidy_bigrams_split$word2))

# return bigrams to original format
tidy_bigrams <- tidy_bigrams_split %>%
  unite(bigram, word1, word2, sep = ' ')
tidy_bigrams

# create bigram dtm
bigram_dtm <- tidy_bigrams %>%
  count(line, bigram) %>%
  cast_dtm(bigram, line, n)
bigram_dtm

# summary statistics for bigram frequencies
bigram_stats <- tidy_bigrams %>%
  group_by(Type, Year) %>%
  count(bigram, sort = TRUE) %>%
  summarize(bigram_count = n(),
            bigram_avg = mean(n),
            bigram_sd = sd(n)
            # coefficient of determination (r-squared)
            #word_rsquared = cor(topic_mat)^2 # must be in matrix form
  )
bigram_stats

bigram_stats <- bigram_stats %>%
  pivot_longer(cols = bigram_count:bigram_sd) %>%
  pivot_wider(names_from = Year, values_from = value) %>%
  select(-name)

#write_xlsx(bigram_stats, 'Tables/bigram_summary.xlsx', col_names = TRUE)

bi_sum <- read_xlsx("bigram_summary2.xlsx")

ggplot(bi_sum, aes(x = SD, y = Mean, color = Type, size = Count)) + geom_point() +
  labs(title = "Bigrams Summarized: Mean vs SD by Document Type and Count",
       x = 'Standard Deviation',
       color = "Document Type")



bi_sum %>% select(Count, Mean, SD) %>%  ggpairs(bi_sum) + labs(title = "Bigram Summary")




# visualize bigram counts by POS
tidy_bigrams %>%
  filter(pos %in% c('NOUN','ADJ','VERB', 'ADV')) %>%
  group_by(pos) %>%
  count(bigram, sort = TRUE) %>%
  slice_max(bigram, n = 10) %>%
  ungroup %>%
  mutate(word = reorder_within(bigram, n, pos)) %>%
  ggplot(aes(x = word, y = n, fill = bigram)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~pos, scales = 'free_y') +
  coord_flip() +
  scale_x_reordered() +
  labs(x = 'Unique Term Pairs',
       y = 'Frequency',
       title = 'Most Frequent Word Pairs by Parts-of-Speech')



# by Type
tidy_bigrams %>%
  group_by(Type) %>%
  count(bigram, sort = TRUE) %>%
  slice_max(bigram, n = 10) %>%
  ungroup %>%
  mutate(word = reorder_within(bigram, n, Type)) %>%
  ggplot(aes(x = word, y = n, fill = bigram)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Type, scales = 'free_y') +
  coord_flip() +
  scale_x_reordered() +
  labs(x = 'Unique Term Pairs',
       y = 'Frequency',
       title = 'Most Frequent Word Pairs by Document Type')



bi <- tidy_bigrams %>%
  filter(pos %in% c('NOUN','ADJ','VERB', 'ADV')) %>%
  group_by(pos, Type, Year) %>%
  count(bigram, sort = TRUE) %>%
  slice_max(bigram, n = 5) 
  

bi2 <- bi %>%
  mutate(Year = cut(Year, seq(1987, 2024, 7))) %>%
  #levels(tidy_articles$Year)
  # check for missing dates
  #sum(is.na(tidy_articles$Year)) # no more NAs, so we move on...
  mutate(Year = recode(Year, '(1987,1994]' = "'88 - '94",
                       '(1994,2001]' = "'94 - '01",
                       '(2001,2008]' = "'01 - '08",
                       '(2008,2015]' = "'08 - '15",
                       '(2015,2022]' = "'15 - '20"))





# Bigram Density
ggplot(bi, aes(n)) + geom_density(aes(fill = factor(Type)), alpha=0.7)  + 
  scale_x_continuous(limits = c(0,8)) + labs(title = "Density of Bigrams by Document Type",
                                             x = "Frequency of Bigrams",
                                             fill = "Document Type",
                                             y = "Density of Bigrams")

ggplot(bi, aes(n)) + geom_density(aes(fill = factor(pos)), alpha=0.7)  + 
  scale_x_continuous(limits = c(0,8)) + labs(title = "Density of Bigrams by Parts-of-Speech",
                                             x = "Frequency of Bigrams",
                                             fill = "Parts-of-Speech",
                                             y = "Density of Bigrams")

ggplot(bi2, aes(n)) + geom_density(aes(fill = factor(Year)), alpha=0.7)  + 
  scale_x_continuous(limits = c(0,8)) + labs(title = "Density of Bigrams by Grouped Year",
                                             x = "Frequency of Bigrams",
                                             fill = "Year (Grouped)",
                                             y = "Density of Bigrams")


# Bigram Boxplots
ggplot(bi, aes(x = factor(Type), y = n)) + 
  geom_boxplot(aes(fill = pos, color = pos), show.legend = F) + coord_flip() + 
  scale_y_continuous(limits = c(0, 13)) + facet_wrap(~pos, scales = 'free_y') +
  guides(color = F) +
  labs(title = "Distribution of POS Word Pair's\nFrequency by Document Type",
       x = "Document Type",
       y = "Frequency of Word Pairs",
       fill = "Parts-of-Speech")

ggplot(bi2, aes(x = factor(Year), y = n)) + 
  geom_boxplot(aes(fill = pos, color = pos), show.legend = F) + coord_flip() + 
  scale_y_continuous(limits = c(0, 13)) + facet_wrap(~pos, scales = 'free_y') +
  guides(color = F) +
  labs(title = "Distribution of POS Word Pair's\nFrequency by Grouped Year",
       x = "Year (Grouped)",
       y = "Frequency of Word Pairs",
       fill = "Parts-of-Speech")







# Bigrams and Tokenized
POS3 <- POS
POS3$Id <- "Token"

bi3 <- bi
bi3$Id <- "Bigram"
bi3$word <- bi3$bigram
bi3 <- bi3[,-4]

both <- rbind(bi3, POS3)


ggplot(both, aes(x = Year, y = n, fill = Id)) + geom_col() +
  labs(title = "Bigram vs Tokenized Word Count by Year",
       y = "Frequency",
       fill = "Word Type")
  

ggplot(both, aes(x = pos, y = n, fill = Id)) + geom_col() +
  labs(title = "Bigram vs Tokenized Word Count by Parts-of-Speech",
       y = "Frequency",
       fill = "Word Type")









# add citations to a data frame
citation <- tidy_articles %>%
  filter(pos %in% 'NOUN') %>%
  group_by(pos, Year, Type, Number.of.citations) %>%
  count(word, sort = TRUE) %>%
  slice_max(word, n = 5)

citation2 <- tidy_articles %>%
  filter(pos %in% c('NOUN','ADJ','VERB','ADV')) %>%
  group_by(pos, Year, Type, Number.of.citations) %>%
  count(word, sort = TRUE) %>%
  slice_max(word, n = 5)



# Source out most frequent term from each POS grouping
topN <- citation2 %>% filter(pos == "NOUN") %>% slice_max(word, n = 10) %>% arrange(desc(n))
topN <- topN[1:10,]

topADJ <- citation2 %>% filter(pos == "ADJ") %>% slice_max(word, n = 10) %>% arrange(desc(n))
topADJ <- topADJ[1:10,] 

topV <- citation2 %>% filter(pos == "VERB") %>% slice_max(word, n = 10) %>% arrange(desc(n))
topV <- topV[1:10,]

topADV <- citation2 %>% filter(pos == "ADV") %>% slice_max(word, n = 10) %>% arrange(desc(n))
topADV <- topADV[1:10,]



# Create new data frame
topTerms <- data.frame(rbind(topN, topADJ, topADV, topV))

topN2 <- POS %>% filter(pos == "NOUN") %>% slice_max(word, n = 10) %>% arrange(desc(n))
topN2 <- topN2[1:10,]

topADJ2 <- POS %>% filter(pos == "ADJ") %>% slice_max(word, n = 10) %>% arrange(desc(n))
topADJ2 <- topADJ2[1:10,] 

topV2 <- POS %>% filter(pos == "VERB") %>% slice_max(word, n = 10) %>% arrange(desc(n))
topV2 <- topV2[1:10,]

topADV2 <- POS %>% filter(pos == "ADV") %>% slice_max(word, n = 10) %>% arrange(desc(n))
topADV2 <- topADV2[1:10,]

# Create new data frame
topTerms2 <- data.frame(rbind(topN2, topADJ2, topADV2, topV2))


# scatter plot 
ggplot(topTerms, aes(y = Number.of.citations, x = Year, color = pos, size = n)) + geom_point() +
  scale_y_continuous(limits = c(0,600)) + 
  labs(title = "Number of Citations vs Year Grouped by\nParts-of-Speech and Frequency of Terms",
       x = "Year",
       y = "Number of Citations",
       size = "Frequency",
       color = "Parts-of-Speech",
       caption = "*One Data Point Over 1000 Citations Removed*")

ggplot(topTerms, aes(y = Number.of.citations, x = Year, color = pos, size = n)) + geom_point() + 
  labs(title = "Number of Citations vs Year Grouped by\nParts-of-Speech and Frequency of Terms",
       x = "Year",
       y = "Number of Citations",
       size = "Frequency",
       color = "Parts-of-Speech")



ggplot(topTerms, aes(y = Number.of.citations, x = pos, fill = pos)) + geom_col(show.legend = F) + 
  labs(title = "Number of Citations per Parts-of-Speech",
       x = "Parts-of-Speech",
       y = "Number of Citations")




# Nouns only
ggplot(citation, aes(x = Year, y = Number.of.citations, fill = Type)) + geom_col() +
  labs(title = "Part-of-Speech Nouns: Number of Citations\nPer Year by Document Type",
       y = "Number of Citations",
       fill = "Document Type")



ggplot(topTerms, aes(x = Year, y = Number.of.citations, color = pos)) + geom_line(size = 1)


Topterms3 <- topTerms %>%
  mutate(Year = cut(Year, seq(1987, 2024, 7))) %>%
  #levels(tidy_articles$Year)
  # check for missing dates
  #sum(is.na(tidy_articles$Year)) # no more NAs, so we move on...
  mutate(Year = recode(Year, '(1987,1994]' = "'88 - '94",
                       '(1994,2001]' = "'94 - '01",
                       '(2001,2008]' = "'01 - '08",
                       '(2008,2015]' = "'08 - '15",
                       '(2015,2022]' = "'15 - '20"))


ggplot(Topterms3, aes(x = pos, y = Number.of.citations, fill = pos)) + geom_col(show.legend = F) +
  facet_wrap(~Year, scales = "free_y") + labs(title = "Number of Citations vs Parts-of-Speech by Grouped Year",
                                              y = "Number of Citations",
                                              x = "Parts-of-Speech")

ggplot(Topterms3, aes(y = Number.of.citations, x = Year, fill = pos)) + geom_col() +
  labs(title = "Number of Citations vs Year by Parts-of-Speech of Most Frequent Terms",
       y = "Number of Citations",
       x = "Year (Grouped)",
       fill = "Parts-of-Speech")

## CIRCLEPACKER

# create df for viz (tokens)
#data <- data.frame(
#  root = tidy_articles$word,
#  group = tidy_articles$Type,
#  subgroup = tidy_articles$pos,
#  subsubgroup = tidy_articles$lemma,
#  value = token_count$n # differing rows error
#)

# prepare data for circle packing viz (tokens)
#data$pathString <- paste(root, #root word
#                         group,
#                         subgroup,
#                         subsubgroup,
#                         #value,
#                         sep = '/')

#topics_byType <- as.Node(data)

# Plot circle packed viz
#circlepackeR(topics_byType, size = 'value')
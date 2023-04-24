library("tidyverse")
library("wordcloud2")
library("tm")
library("readr")
library("dplyr")
library("viridis")
library("extrafont")

# Remove at the end

publications_df <- read.csv("publications.csv")

# create a corpus object 

zenodo_corpus <- Corpus(VectorSource(publications_df$description))

# built a function to remove HTML tags from a text 

removeHTML <- function(text){
  text = gsub(pattern = '<. +\\">', '', text)
  text = gsub(pattern = '</ .+>', '', text)
  return(text)
}

# use tm package:
# to to remove any HTML tags
# to remove any numbers from the text
# to remove any punctuation marks from the text
# to remove any leading or trailing whitespaces from the text
# to convert all text to lowercase
# to remove common English and German stopwords
# to remove additional stopwords

zenodo_corpus <- zenodo_corpus %>% 
  tm_map(content_transformer(removeHTML)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% #
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("german")) %>%
  tm_map(removeWords, stopwords("SMART"))


# create a TDM object with word frequencies

tdm <- TermDocumentMatrix(zenodo_corpus) %>%
  as.matrix()

# create a vector of the total frequency of each word

words <- sort(rowSums(tdm), decreasing = TRUE)

# create a dataframe with two columns (unique words from guardian_amazon_title corpus 
# and corresponding frequency of each word)

df <- data.frame(word = names(words), freq = words)
df$word_freq <- paste(df$word, df$freq, sep = ": \n")

# remove rows with strings lower as two characters

df <- df %>% 
  filter(nchar(as.character(word)) >4,
         word !="don'")


# select colors from guardian and amazon logos

zenodo.colors <- c("#8c94bc", "#7ac9f4", "#ebf3f5")
zenodo.background <- "#0a62bc" 
  
# design the word cloud

wordcloud2(df,
           color = rep_len(zenodo.colors, nrow(df)),
           backgroundColor = zenodo.background,
           fontFamily = "DM Sans",
           size = 0.95,
           minSize = 5,
           rotateRatio = 0)

# create data frame with top 30 words

df_top30 <- head(df, 30)

# create a radar plot with top 30 words

ggplot(data=df_top30, aes(x=word_freq, y=freq, fill=word_freq))+
  geom_bar(width = 0.75, stat = "identity", colour = "black", size = 0.3)+
  scale_fill_viridis(discrete = TRUE,
                     option = "D")+
  coord_polar(theta = "x")+
  coord_polar(start = 0)+
  ylim(-2000, 7200)+
  ggtitle("Top 30 Words of Zenodo Article Descriptions by Frequency")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = NULL, y = NULL)



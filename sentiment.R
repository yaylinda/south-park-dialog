setwd("~/Developer/south-park-dialog")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("SentimentAnalysis")

###############################################################################
# READ
###############################################################################

#======================================
# READ DATA
#======================================

data = read.csv("data.csv", header = T)
data$LineCleaned = gsub("\n", "", data$Line)
data$LineCleaned = gsub("\\(", "", data$LineCleaned)
data$LineCleaned = gsub(")", "", data$LineCleaned)

unique(data$Character)

#======================================
# FUNCTIONS
#======================================

get_emotion_sums_for_character = function(character) {
  character_lines = data[which(data$Character == character), ]$LineCleaned
  emotions = get_nrc_sentiment(character_lines)
  emotions_trans = data.frame(t(emotions))
  
  emotions_trans_sum = data.frame(rowSums(emotions_trans[1:ncol(emotions_trans)]))
  
  names(emotions_trans_sum)[1] = "count"
  emotions_trans_sum = cbind("sentiment" = rownames(emotions_trans_sum), emotions_trans_sum)
  emotions_trans_sum = emotions_trans_sum[1:8,]
  rownames(emotions_trans_sum) <- NULL
  
  emotions_trans_sum$name = character
  
  total = sum(emotions_trans_sum$count)
  emotions_trans_sum$proportion = (emotions_trans_sum$count / total) * 100
  
  emotions_trans_sum
}

#======================================
# EMOTION SUMS FOR CHARACTERS
#======================================

Stan = get_emotion_sums_for_character("Stan")
Kyle = get_emotion_sums_for_character("Kyle")
Cartman = get_emotion_sums_for_character("Cartman")
Kenny = get_emotion_sums_for_character("Kenny")
Butters = get_emotion_sums_for_character("Butters")
Randy = get_emotion_sums_for_character("Randy")

combined = do.call(
  "rbind", list(
    Stan, 
    Kyle, 
    Cartman, 
    Kenny, 
    Butters, 
    Randy
))

#======================================
# STACKED BAR PLOT
#======================================

ggplot(
  combined, 
  aes(
    x = sentiment, 
    y = proportion, 
    fill = name
  )
) + 
geom_bar(
  position= "stack", 
  stat = "identity"
)

#======================================
# TILE PLOT
#======================================

ggplot(
  combined, 
  aes(
    x = sentiment, 
    y = name, 
    fill = proportion
  )
) + 
geom_tile(color = "white", alpha = 0.8) + 
geom_text(
  aes(label = proportion), 
  size = 2
)
###############################################################################
# EXPLORE / PLAY
###############################################################################

#======================================
# PROCESS DATA
#======================================

#--------------------------------------
# Get character's lines
#--------------------------------------
character_lines = data[which(data$Character == "Stan"), ]$LineCleaned

#--------------------------------------
# Corpus
#--------------------------------------
corpus = Corpus(VectorSource(character_lines))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, stemDocument)

#--------------------------------------
# Term Document Matrix & Data Frame
#--------------------------------------
document_matrix_obj = TermDocumentMatrix(corpus)
document_matrix = as.matrix(document_matrix_obj)
document_matrix_df = sort(rowSums(document_matrix), decreasing = TRUE)
document_matrix_df = data.frame(
  word = names(document_matrix_df), 
  freq = document_matrix_df
)

#--------------------------------------
# Word Associations
#--------------------------------------
findAssocs(
  document_matrix_obj, 
  terms = document_matrix_df$word[1:5], 
  corlimit = 0.20
)			
findAssocs(
  document_matrix_obj, 
  terms = findFreqTerms(document_matrix_obj, lowfreq = 300), 
  corlimit = 0.25
)

#--------------------------------------
# Sentiment Scores
#--------------------------------------

# syuzhet
syuzhet_vector = get_sentiment(character_lines, method = "syuzhet")
summary(syuzhet_vector)

# bing
bing_vector = get_sentiment(character_lines, method = "bing")
summary(bing_vector)

# affin
afinn_vector = get_sentiment(character_lines, method = "afinn")
summary(afinn_vector)

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

#--------------------------------------
# Emotion Classification
#--------------------------------------

emotions = get_nrc_sentiment(character_lines)
emotions_trans = data.frame(t(emotions))

emotions_trans_sum = data.frame(rowSums(emotions_trans[1:ncol(emotions_trans)]))

names(emotions_trans_sum)[1] = "count"
emotions_trans_sum = cbind("sentiment" = rownames(emotions_trans_sum), emotions_trans_sum)
emotions_trans_sum = emotions_trans_sum[1:8,]
rownames(emotions_trans_sum) <- NULL

#--------------------------------------
# SentimentAnalysis Package
#--------------------------------------

cartman_lines = data[which(data$Character == "Cartman"), ]$LineCleaned
butters_lines = data[which(data$Character == "Butters"), ]$LineCleaned

sentiment_cartman = analyzeSentiment(cartman_lines)
sentiment_butters = analyzeSentiment(butters_lines)

plotSentimentResponse(sentiment_butters$SentimentQDAP, rep(0, nrow(sentiment_butters)))

#======================================
# PLOT DATA
#======================================

# Word Cloud Plot
set.seed(1234)
wordcloud(
  words = document_matrix_df$word, 
  freq = document_matrix_df$freq, 
  min.freq = 5,
  max.words = 100, 
  random.order = FALSE, 
  rot.per = 0.40, 
  colors = brewer.pal(8, "Dark2")
)

# Emotion Classification Count Plot
ggplot(emotions_trans_sum, aes(x = sentiment, y = count)) + 
  geom_bar(stat = "identity")

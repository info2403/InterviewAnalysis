# ---------------------------------------------------------------------
# R Script for Text Data Preprocessing and Analysis
# Purpose: To preprocess and clean interview text data for further analysis
# Author: Sonal Mendonca
# Date Created: 2024-01-20
# Last Modified: 2024-04-15
# R Version: R version 4.3.3
# Required Packages: tm, SnowballC
# ---------------------------------------------------------------------

library(tm)
library(SnowballC)

# Set the path to the directory containing the text files
combined_interview_text_dir <- "C:/Users/SMendonca/Desktop/Interviews/InterviewFileBoth"
consultant_interview_text_dir <- "C:/Users/SMendonca/Desktop/Interviews/ConsultantinterviewFiles"
RadDos_interview_text_dir <- "C:/Users/SMendonca/Desktop/Interviews/InterviewRadDos"

# Create a corpus from the text files
consultant_corpus_raw <- Corpus(DirSource(consultant_interview_text_dir), readerControl = list(language = "en", reader = readPlain, encoding = "UTF-8"))
RadDos_corpus_raw <- Corpus(DirSource(RadDos_interview_text_dir), readerControl = list(language = "en", reader = readPlain, encoding = "UTF-8"))
combined_corpus_raw <- Corpus(DirSource(combined_interview_text_dir), readerControl = list(language = "en", reader = readPlain, encoding = "UTF-8"))


# Lowercase transformation
consultant_corpus_clean <- tm_map(consultant_corpus_raw, content_transformer(tolower))
RadDos_corpus_clean <- tm_map(RadDos_corpus_raw, content_transformer(tolower))
combined_corpus_clean <- tm_map(combined_corpus_raw, content_transformer(tolower))

# Remove punctuation
consultant_corpus_clean <- tm_map(consultant_corpus_clean, removePunctuation)
RadDos_corpus_clean <- tm_map(RadDos_corpus_clean, removePunctuation)
combined_corpus_clean <- tm_map(combined_corpus_clean, removePunctuation)

# Remove numbers
consultant_corpus_clean <- tm_map(consultant_corpus_clean, removeNumbers)
RadDos_corpus_clean <- tm_map(RadDos_corpus_clean,removeNumbers)
combined_corpus_clean <- tm_map(combined_corpus_clean,removeNumbers)


# Remove common stop words
consultant_corpus_clean <- tm_map(consultant_corpus_clean, removeWords, stopwords("en"))
RadDos_corpus_clean <- tm_map(RadDos_corpus_clean, removeWords, stopwords("en"))
combined_corpus_clean <- tm_map(combined_corpus_clean, removeWords, stopwords("en"))

# Additional cleaning steps to enhance the quality of my text data

# Remove custom stopwords or filler words
custom_stopwords <- c("uh", "um", "ah", "like")  # Add any other words identified as irrelevant
consultant_corpus_clean <- tm_map(consultant_corpus_clean, removeWords, custom_stopwords)
RadDos_corpus_clean <- tm_map(RadDos_corpus_clean, removeWords,custom_stopwords)
combined_corpus_clean <- tm_map(combined_corpus_clean, removeWords,custom_stopwords)

# Remove extra whitespace
consultant_corpus_clean <- tm_map(consultant_corpus_clean, stripWhitespace)
RadDos_corpus_clean <- tm_map(RadDos_corpus_clean, stripWhitespace)
combined_corpus_clean <- tm_map(combined_corpus_clean, stripWhitespace)

###### Stemming - reducing words to their root form
#consultant_corpus_clean <- tm_map(consultant_corpus_clean, stemDocument)#optional

# OPTIONAL: Removing specific terms that don't contribute to analysis
# Might want to remove specific terms or patterns that are irrelevant to my analysis.
# For example, specific names, locations, or other identifiable information if not relevant.

#only use for wordcloud KEYWORDAND THEME IDENTIFICATION
consultant_corpus_clean_key <- tm_map(consultant_corpus_clean, removeWords, c("yeah", "thing", "um", "uh", "you know", "I mean", "sort of", "kind of", "actually", "basically", "like", "essentially", "right", "OK", "please", "tell me again", "ask the question again", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "8:15", "10:00", "thank you", "so", "and", "but", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now"))
RadDos_corpus_clean_key <- tm_map(RadDos_corpus_clean, removeWords, c("yeah", "thing", "um", "uh", "you know", "I mean", "sort of", "kind of", "actually", "basically", "like", "essentially", "right", "OK", "please", "tell me again", "ask the question again", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "8:15", "10:00", "thank you", "so", "and", "but", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now"))

# Check the cleaning result on a sample document
writeLines(as.character(consultant_corpus_clean[[1]]))  # Display the cleaned text of the first document

# ---------------------------------------------------------------------
# R Script for Keyword and Theme Identification
# Purpose: 
# Author: Sonal Mendonca
# Date Created: 2024-01-20
# Last Modified: 2024-04-15
# R Version: R version 4.3.3
# Required Packages: topicmodels, wordcloud, RColorBrewer
# ---------------------------------------------------------------------

library(topicmodels)
library(wordcloud)
library(RColorBrewer)


# Ensure the corpus is ready for DTM creation
# Note: I have already cleaned my corpus in previous steps

# Create a Document-Term Matrix (DTM)
consultant_dtm <- DocumentTermMatrix(consultant_corpus_clean_key)
RadDos_dtm <- DocumentTermMatrix(RadDos_corpus_clean_key)

# Inspect the DTM to understand its structure
inspect(consultant_dtm[1:5, 1:10])  # Inspect the first 5 documents and first 10 terms
inspect(RadDos_dtm[1:4, 1:10])
# Perform LDA on the DTM

set.seed(123)  # For reproducibility
consultant_lda_model <- LDA(consultant_dtm, k = 5, control = list(seed = 123))
RadDos_lda_model <- LDA(RadDos_dtm, k = 5, control = list(seed = 123))


##inspect the topics to understand what terms are included:
consultant_topics_terms <- terms(consultant_lda_model, 10)
RadDos_topics_terms <- terms(RadDos_lda_model, 10)

print(consultant_topics_terms)
print(RadDos_topics_terms)


##To generate word clouds - need the term frequencies within each topic. 
##However, terms() only gives the top terms, not their frequencies. 
# The beta component of the LDA model, which represents 
#the distribution of words across topics can be used to generate word clouds
consultant_topics_beta <- consultant_lda_model@beta

for (topic in 1:5) {
  terms <- consultant_topics_terms[, topic]
  term_indices <- match(terms, colnames(consultant_dtm))
  probabilities <- colSums(as.matrix(consultant_dtm)[, term_indices, drop = FALSE])
  probabilities <- probabilities / sum(probabilities) * 100
  
  if (length(probabilities) > 0 && !any(is.na(probabilities))) {
    wordcloud(words = terms, freq = probabilities, min.freq = 1,
              max.words = 100, random.order = FALSE, rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  } else {
    cat(sprintf("Skipping topic %d due to data issue.\n", topic))
  }
}

RadDos_topics_beta <- RadDos_lda_model@beta

for (topic in 1:4) {
  terms <- RadDos_topics_terms[, topic]
  term_indices <- match(terms, colnames(RadDos_dtm))
  probabilities <- colSums(as.matrix(RadDos_dtm)[, term_indices, drop = FALSE])
  probabilities <- probabilities / sum(probabilities) * 100
  
  if (length(probabilities) > 0 && !any(is.na(probabilities))) {
    wordcloud(words = terms, freq = probabilities, min.freq = 1,
              max.words = 100, random.order = FALSE, rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  } else {
    cat(sprintf("Skipping topic %d due to data issue.\n", topic))
  }
}

# ---------------------------------------------------------------------
# R Script for Sentiment Analysis and Plots
# Purpose: 
# Author: Sonal Mendonca
# Date Created: 2024-01-20
# Last Modified: 2024-04-15
# R Version: R version 4.3.3
# Required Packages: syuzhet,ggplot2,tidyr, dplyr
# ---------------------------------------------------------------------
library(syuzhet)
library(ggplot2)
library(tidyr)
library(dplyr)

# corpus_clean is my cleaned corpus
# Convert the cleaned corpus back to a character vector
consultant_texts <- sapply(consultant_corpus_clean, as.character)
RadDos_texts <- sapply(RadDos_corpus_clean, as.character)

# Apply NRC sentiment analysis
consultant_sentiment_scores <- get_nrc_sentiment(consultant_texts)
RadDos_sentiment_scores <- get_nrc_sentiment(RadDos_texts)

# View a summary of sentiment scores
consultant_sentiment_summary <- data.frame(colSums(consultant_sentiment_scores))
consultant_sentiment_summary

RadDos_sentiment_summary <- data.frame(colSums(RadDos_sentiment_scores))
RadDos_sentiment_summary

consultant_sentiment_long <- consultant_sentiment_scores %>%
  mutate(doc_id = row_number()) %>%
  pivot_longer(
    cols = -doc_id, 
    names_to = "sentiment", 
    values_to = "score"
  )

RadDos_sentiment_long <- RadDos_sentiment_scores %>%
  mutate(doc_id = row_number()) %>%
  pivot_longer(
    cols = -doc_id, 
    names_to = "sentiment", 
    values_to = "score"
  )

# Convert sentiment scores to a long format for ggplot
#sentiment_long <- sentiment_scores %>%
#mutate(doc_id = row_number()) %>%
#gather(key = "sentiment", value = "score", -doc_id)

# Plot
ggplot(consultant_sentiment_long, aes(x = doc_id, y = score, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_minimal() +
  labs(x = "Document ID", y = "Sentiment Score", title = "Sentiment Scores Across Oncologist Interviews")

# Plot
ggplot(RadDos_sentiment_long, aes(x = doc_id, y = score, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_minimal() +
  labs(x = "Document ID", y = "Sentiment Score", title = "Sentiment Scores Across Dosimetrist and Radiogrpaher Interviews")


# Summarize overall sentiment scores
consultant_sentiment_summary <- colSums(consultant_sentiment_scores)
RadDos_sentiment_summary <- colSums(RadDos_sentiment_scores)

#Print the summary
print(consultant_sentiment_summary)
print(RadDos_sentiment_summary)

#Visualise - bar plots to show prevalence of each emotion
barplot(consultant_sentiment_summary, las=2, col=rainbow(length(consultant_sentiment_summary)),
        main="Sentiment Scores Across Consultant Interviews")

barplot(RadDos_sentiment_summary, las=2, col=rainbow(length(consultant_sentiment_summary)),
        main="Sentiment Scores Across Dosimetrist and Radiogrpaher Interviews")

# ---------------------------------------------------------------------
# R Script for N-grams
# Purpose: 
# Author: Sonal Mendonca
# Date Created: 2024-01-20
# Last Modified: 2024-04-15
# R Version: R version 4.3.3
# Required Packages: quanteda
# ---------------------------------------------------------------------
library(quanteda)
# Convert  tm corpus to character vectors because quanteda works with character data or its own corpus objects
consultant_text_2 <- sapply(consultant_corpus_clean, as.character)

# Now, create a quanteda corpus from the character vector
consultant_corpus_q <- corpus(consultant_text_2)

# Tokenize the corpus. This splits the text into individual words
consultant_tokens <- tokens(consultant_corpus_q)

# Generate n-grams. can adjust 'n' to create bigrams (n=2), trigrams (n=3), etc.
consultant_bigrams <- tokens_ngrams(consultant_tokens, n = 2)
consultant_trigrams <- tokens_ngrams(consultant_tokens, n = 3)

# can also remove stopwords directly with quanteda's tokens_remove(), which might be more efficient
consultant_bigrams <- tokens_remove(consultant_bigrams, stopwords("en"))
consultant_trigrams <- tokens_remove(consultant_trigrams, stopwords("en"))

# Create a document-feature matrix (DFM) from tokens to analyze frequency
consultant_dfm_bigrams <- dfm(consultant_bigrams)
consultant_dfm_trigrams <- dfm(consultant_trigrams)

# Now, find the most frequent n-grams
consultant_top_bigrams <- topfeatures(consultant_dfm_bigrams, n = 30)  # Adjust n to see more or fewer terms
consultant_top_trigrams <- topfeatures(consultant_dfm_trigrams, n = 30)

# Print the most common bigrams and trigrams
print(consultant_top_bigrams)
print(consultant_top_trigrams)

# For visualization, can create a wordcloud or bar plot
library(wordcloud)
set.seed(1234)  # For reproducibility

# Plotting the top bigrams
wordcloud(names(consultant_top_bigrams), consultant_top_bigrams, min.freq = 1, max.words = 100, colors = brewer.pal(8, "Dark2"))

# Plotting the top trigrams
wordcloud(names(consultant_top_trigrams), consultant_top_trigrams, min.freq = 1, max.words = 100, colors = brewer.pal(8, "Dark2"))

# ---------------------------------------------------------------------
# R Script for N-grams keywords
# Purpose: 
# Author: Sonal Mendonca
# Date Created: 2024-01-20
# Last Modified: 2024-04-15
# R Version: R version 4.3.3
# Required Packages: quanteda, quanteda.textplots, ggplot2
# ---------------------------------------------------------------------
library(quanteda)
library(quanteda.textplots)
library(ggplot2)

# Function to get top trigrams for a specific term
get_top_trigrams <- function(corpus, term, top_n = 10) {
  # Convert to tokens and get trigrams
  tokens_1 <- tokens(corpus)
  trigrams_1 <- tokens_ngrams(tokens_1, n = 3, concatenator = " ")
  
  # Create a pattern for trigrams ending with the term
  pattern <- paste0("\\b\\S+ \\S+ ", term, "\\b")
  tokens_filtered <- tokens_select(trigrams_1, pattern = pattern, valuetype = "regex", selection = "keep")
  
  # Convert to dfm and calculate frequencies
  dfm_trigrams_1 <- dfm(tokens_filtered)
  trigram_frequencies_1 <- colSums(dfm_trigrams_1)
  
  # Create dataframe and sort by frequency
  freq_df_1 <- data.frame(trigram = names(trigram_frequencies_1), frequency = trigram_frequencies_1)
  freq_df_1 <- freq_df_1[order(-freq_df_1$frequency), ]
  
  # Subset to top n trigrams
  if (nrow(freq_df_1) > top_n) {
    freq_df_1 <- head(freq_df_1, top_n)
  }
  
  # Plotting
  ggplot(freq_df_1, aes(x = reorder(trigram, frequency), y = frequency)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    coord_flip() +  # Flip coordinates for horizontal bars
    labs(x = "Trigram", y = "Frequency", title = paste("Top Trigram Frequencies for", term))
}

# Example usage for the term 'workload'
get_top_trigrams(consultant_workload_corpus, "workload", top_n = 10)

# Repeat the process for other terms
get_top_trigrams(consultant_workload_corpus, "Secretary", top_n = 10)
get_top_trigrams(consultant_workload_corpus, "Technology", top_n = 10)
get_top_trigrams(consultant_workload_corpus, "Staff", top_n = 10)
get_top_trigrams(consultant_workload_corpus, "Patient", top_n = 10)


# ---------------------------------------------------------------------
# R Script forsentiment analysis of keywords
# Purpose: 
# Author: Sonal Mendonca
# Date Created: 2024-01-20
# Last Modified: 2024-04-15
# R Version: R version 4.3.3
# Required Packages: quanteda, syuzhet, ggplot2,wordcloud,RColorBrewer
# ---------------------------------------------------------------------
library(quanteda)
library(syuzhet)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# Helper function to get concordance and sentiment scores
analyze_sentiment <- function(tokens, term) {
  kwic_tokens <- kwic(tokens, pattern = term)
  contexts <- paste(kwic_tokens$pre, kwic_tokens$keyword, kwic_tokens$post, sep=" ")
  sentiment_scores <- get_sentiment(contexts, method = "syuzhet")
  
  list(
    contexts = contexts,
    sentiment_scores = sentiment_scores
  )
}

# Helper function to plot sentiment scores
plot_sentiment <- function(sentiment_scores, term) {
  ggplot(data.frame(SentimentScore = sentiment_scores), aes(x = SentimentScore)) +
    geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = paste("Distribution of Sentiment Scores Around", term),
         x = "Sentiment Score",
         y = "Frequency")
}

# Helper function to create wordclouds
create_wordcloud <- function(dfm_contexts) {
  mat <- as.matrix(dfm_contexts)
  word_freqs <- colSums(mat)
  word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs, row.names = NULL)
  wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq, min.freq = 1,
            max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
}

# List of terms to analyze
terms <- c("workload", "Secretary", "Technology", "Staff", "Patient")

# Loop through terms to analyze sentiment and create plots
for (term in terms) {
  result <- analyze_sentiment(tokens, term)
  
  # Plot sentiment distribution
  plot_sentiment(result$sentiment_scores, term)
  
  # Print contexts (optional, can be commented out if not needed)
  print(result$contexts)
  
  # Create wordcloud for each context
  tokens_contexts <- tokens(result$contexts)
  dfm_contexts <- dfm(tokens_contexts)
  create_wordcloud(dfm_contexts)
}
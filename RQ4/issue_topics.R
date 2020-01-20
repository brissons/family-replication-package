library(readr)
library(tidytext)
library(dplyr)
library(ldatuning)
library(stm)
library(ggplot2)
library(quanteda)

#load data
data <-
  read_tsv(
    "issue_topic_comments.tsv",
    col_names = FALSE,
    quote = "",
    quoted_na = FALSE,
    col_types = list(col_double(), col_double(), col_character())
  )

colnames(data) = c("project_id", "issue_id", "text")

#remove stop words
tidy_data <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#transform into document-feature matrix for topic modelling
data_dfm <- tidy_data %>%
  count(issue_id, word, sort = TRUE) %>%
  cast_dfm(issue_id, word, n)

#transform into document-term matrix to determine optimal number of topics
data_dtm <- convert(data_dfm, to = "topicmodels")

#find optimal number of topics
topic_number <- FindTopicsNumber(
  data_dtm,
  topics = seq(from = 12, to = 32, by = 1),
  metrics = c("Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 7L,
  verbose = TRUE
)

FindTopicsNumber_plot(topic_number)

#topic modeling
topic_model <- stm(data_dfm,
                   K = 20,
                   verbose = FALSE,
                   init.type = "Spectral")

#retrieve top words per topic
td_beta <- tidy(topic_model)

top_words <- td_beta %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

#classify each issue by topic
td_gamma <- tidy(topic_model,
                 matrix = "gamma",
                 document_names = rownames(data_dfm))

#returns guess of topic for each issue
issue_classifications <- td_gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

write.table(issue_classifications,
            file = "issue_topic_classifications.csv",
            sep = ",")

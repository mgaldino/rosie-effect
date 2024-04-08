library(tidyverse)
library(tidytext)
library(SnowballC)
library(data.table)
library(janitor)
library(lubridate)
library(quanteda)
library(slider)
library(widyr)
library(irlba)
library(targets)
library(janitor)
library(txt4cs)
library(stopwords)
library(lubridate)

head(stopwords::stopwords("pt", source = "snowball"), 20)

my_stop_words <- stopwords::stopwords("pt", source = "snowball")
words <- words %>%
  anti_join(my_stop_words, by = "word")

## Import data set
tar_load(leg_tweet_allowance_joined)
glimpse(leg_tweet_allowance_joined)

tr_df <- leg_tweet_allowance_joined %>%
  dplyr::slice(1:1000)

glimpse(tr_df)

tr_df1 <- leg_tweet_allowance_joined %>%
  mutate(created_at1 = as.Date(created_at),
         day_tweet = day(created_at1),
         month_tweet = month(created_at1),
         year_tweet = year(created_at1),
         day_issued = day(dat_emissao),
         month_issued = month(dat_emissao),
         year_issued = year(dat_emissao),
         spending = as.numeric(vlr_documento)) %>%
  group_by(cpf ) %>%
  mutate(bol_spending_before_tweet = as.numeric(created_at1 > dat_emissao),
         bol_spending_after_tweet = as.numeric(created_at1 <= dat_emissao)) %>%
  group_by(cpf, bol_spending_before_tweet, bol_spending_after_tweet) %>%
  summarise(total_spending = sum(spending))

max(tr_df$dat_emissao)

tr_df1 <- leg_tweet_allowance_joined %>%
  mutate(
    created_at1 = as.Date(created_at),
    dat_emissao1 = as.Date(dat_emissao),
    spending = as.numeric(vlr_documento),
    start_window = created_at - days(30),
    end_window = created_at + days(30),
    bol_spending_before_tweet = ifelse(dat_emissao1 < created_at1, 1, 0)
  ) 

df <- df %>%
  mutate(
    antes_tuite = data_tuite - days(30),
    depois_tuite = data_tuite + days(30)
  )

# Filtrando os gastos que ocorrem dentro das janelas de 30 dias antes e depois de cada tuíte
# E então, agrupando e sumarizando os gastos
gastos_analise <- df %>%
  group_by(cpf, data_tuite) %>%
  summarise(
    gasto_antes = sum(valor_gasto[data_gasto >= antes_tuite & data_gasto < data_tuite], na.rm = TRUE),
    gasto_depois = sum(valor_gasto[data_gasto > data_tuite & data_gasto <= depois_tuite], na.rm = TRUE),
    .groups = 'drop'
  )


analise_gastos <- tr_df1 %>%
  rowwise() %>%
  do({
    current_tweet <- .
    
    analyzed_spending <- leg_tweet_allowance_joined %>%
      filter(cpf == current_tweet$cpf) %>%
      mutate(
        periodo = case_when(
          data_gasto < tuíte_atual$created_at ~ "Antes",
          data_gasto >= tuíte_atual$created_at & data_gasto <= tuíte_atual$fim_janela ~ "Depois",
          TRUE ~ NA_character_
        )
      ) %>%
      group_by(periodo) %>%
      summarise(
        total_gasto = sum(valor_gasto, na.rm = TRUE),
        dias = n_distinct(data_gasto),
        gasto_medio_diario = total_gasto / dias
      ) %>%
      filter(!is.na(periodo))
    
    bind_cols(tuíte_atual, gastos_analisados)
  })

# %>%
#   group_by(cpf) %>%
#   summarise(
#     num_days_before = min(dat_emissao)
#     total_spending_before_tweet = sum(spending[bol_spending_before_tweet == 0]),
#     total_spending_after_tweet = sum(spending[bol_spending_before_tweet == 1]),
#     .groups = 'drop' # Remove o agrupamento após a summarise
#   )
  
glimpse(tr_df1)

# group_by(cpf, created_at1, dat_emissao) %>%
  # summarise(retweet_count = sum(retweet_count),
  #           favorite_count = sum(favorite_count),
  #           spending = as.numeric(vlr_documento)) %>%
  # mutate(favorite_count = ifelse(is.na(created_at1), 0*favorite_count, favorite_count),
  #        retweet_count  = ifelse(is.na(created_at1), 0*retweet_count , retweet_count ),
  #        tweeted = ifelse(is.na(created_at1), 0 , 1 ))

# filter dataset e crate text colum

df_19 <- leg_tweet_allowance_joined %>%
  filter(nu_legislatura == "2019")

# 
#   mutate(text_embeddings = tolower(paste(txt_descricao, txt_descricao_especificacao, txt_fornecedor)))

# Remove stopwords
# Define a função para remover stopwords de uma string
remove_stopwords <- function(string) {
  words <- unlist(strsplit(string, " ")) # Separa a string em palavras
  words_filtered <- words[!words %in% my_stop_words] # Remove as stopwords
  return(paste(words_filtered, collapse = " ")) # Junta as palavras restantes de volta em uma string
}

# Aplica a função à coluna text_embeddings
df_word_em$text_embeddings <- sapply(df_word_em$text_embeddings, remove_stopwords)

# Verifica o resultado
glimpse(df_word_em)


# my_stop_words <- stopwords::stopwords("pt", source = "snowball")
# df_word_em_filtered <- df_word_em %>%
#   mutate(text_embeddings = strsplit(as.character(text_embeddings), " ")) %>%
#   unnest(text_embeddings) %>%
#   anti_join(tibble(word = my_stop_words), by = c("text_embeddings" = "word")) %>%
#   group_by(nu_deputado_id, ide_documento) %>%
#   summarise(text_embeddings = paste(text_embeddings, collapse = " "), .groups = 'drop')

# glimpse(df_word_em_filtered)
   

#create context window with length 4

tidy_skipgrams <- df_word_em %>%
  filter(!is.na(text_embeddings)) %>%
  unnest_tokens(ngram, text_embeddings, token = "ngrams", n = 4) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, ide_documento, ngramID) %>%
  unnest_tokens(word, ngram)

glimpse(tidy_skipgrams)

#calculate unigram probabilities (used to normalize skipgram probabilities later)
unigram_probs <- df_word_em %>%
  unnest_tokens(word, txt_descricao) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

#normalize probabilities
normalized_prob <- skipgram_probs %>%
  # filter(n > 5) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

dim(normalized_prob)
normalized_prob[200:210,]

normalized_prob %>% 
  filter(word1 == "segurança") %>%
  arrange(-p_together)

normalized_prob %>% 
  filter(word1 == "telefonia") %>%
  arrange(-p_together)
# SVD

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

#remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0
#run SVD
pmi_svd <- irlba(pmi_matrix, 256, maxit = 500)
#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)


#Here’s a handy function written by Julia Silge to identify synonyms using the word vectors we created above:

library(broom)

search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector
  similarity_colum <- row.names(similarities) 
  similarities <- similarities %>%
  tidy %>%
    as_tibble() %>%
    mutate(token = similarity_colum)
  
  colnames(similarities)[1] <- "similarity"

  similarities %>%
    arrange(-similarity)    
}

selected_vector <- word_vectors["segurança",]

seg_synonym <- search_synonyms(word_vectors, word_vectors["segurança",])
seg_synonym

# plot
mi_svd <- irlba(pmi_matrix, 2, maxit = 500)

#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

#grab 100 words
forplot <- as.data.frame(word_vectors[200:300,])
forplot$word <- rownames(forplot)

#now plot
library(ggplot2)
ggplot(forplot, aes(x=V1, y=V2, label=word))+
  geom_text(aes(label=word),hjust=0, vjust=0, color="blue")+
  theme_minimal()+
  xlab("First Dimension Created by SVD")+
  ylab("Second Dimension Created by SVD")

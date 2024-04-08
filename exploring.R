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
## Alternativa
tar_load(cota_parlamentar_data1)

#create context window with length 8

df_word_em <- cota_parlamentar_data1 %>%
  filter(nu_legislatura == "2019") %>%
  mutate(text_embeddings = paste(txt_descricao, txt_descricao_especificacao,txt_fornecedor, txt_cnpjcpf,
                                 paste0("R$ ", vlr_documento)))
   

glimpse(df_word_em)

tidy_skipgrams <- df_word_em %>%
  select(nu_deputado_id, ide_documento, text_embeddings) %>%
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
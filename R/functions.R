# R/functions.R
get_raw_data <- function(file) {
  if(file_ext(file) == "csv") {
    data <- fread(file)
  } else {
    data <- read_excel(file,
                  skip = 1, col_types  = c("numeric", "date", "numeric", 
                                           "numeric", "text", "text", "text"))
                  
  }
  data <- data %>%
    clean_names()
  return(data)
}

get_legislators_name <- function(file2){
  
  file2 <- file2 %>%
    dplyr::select(cpf, tx_nome_parlamentar, nu_deputado_id, nu_legislatura) %>%
    distinct(new_id = paste(cpf,nu_deputado_id,nu_legislatura, sep="_"), .keep_all = TRUE)
  
  return(file2)
  
}

clean_names_legislators <- function(data, name_var) {
  data <- data %>%
    mutate({{name_var}} := iconv(tolower(!!sym(name_var)), "UTF-8", "ASCII//TRANSLIT"))
  
  # legislators_name_data <- legislators_name_data %>%
  #   mutate(txNomeParlamentar = iconv(tolower(txNomeParlamentar), "UTF-8", "ASCII//TRANSLIT"))
  # 
  # tweet_data2 <- tweet_data2 %>%
  #   mutate(nome_deputado = iconv(tolower(nome_deputado), "UTF-8", "ASCII//TRANSLIT"))
  # 
}

add_legislature <- function(tweet_data) {
  tweet_data <- tweet_data %>%
    mutate(nu_legislatura = ifelse(created_at < as.Date("2007-01-01"), 2003, 
                                   ifelse(created_at < as.Date("2011-01-01"), 2007,
                                          ifelse(created_at < as.Date("2015-01-01"), 2011,
                                                 ifelse(created_at < as.Date("2019-01-01"), 2015, 2019)))),
           nu_legislatura = as.character(nu_legislatura))
}


names_first_match <- function(legislators_name_data, tweet_data){
  
  tweet_data <- add_legislature(tweet_data)
  # join
  tweet_data2 <- tweet_data %>%
    left_join(legislators_name_data, by = join_by(nome_deputado == tx_nome_parlamentar, nu_legislatura))
  
}

clean_parlamentary_data <- function(data) {
  data <- data %>%
    janitor::clean_names() %>%
    filter(!is.na(dat_emissao) & !grepl("TELEFONIA", txt_descricao)) %>% # !!sym(var_name)
    mutate(dat_emissao = as_date(dat_emissao)) %>%
    filter(!is.na(dat_emissao) & cpf != "cpf") %>%
    mutate(dat_pagamento_restituicao = as_date(dat_pagamento_restituicao),
           year_month_payment = paste(year(dat_emissao),month(dat_emissao), sep="-"),
           year_month_refund = paste(year(dat_pagamento_restituicao),month(dat_pagamento_restituicao),sep="-")) 
  
}

print_summary <- function(legislators_1st_match_data) {
  print(summary(legislators_1st_match_data))
}
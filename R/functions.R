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

join_by_cpf <- function(datax, datay){
  df <- dplyr::inner_join(datax, datay, by = join_by(cpf, nu_legislatura))
}

tweets_by_month <- function(data) {
  df_agregado <- data %>%
    mutate(created_at = as.Date(created_at),
           mes_ano = floor_date(created_at, "month")) %>%
    group_by(cpf, mes_ano) %>%
    summarise(num_tuites = n(), .groups = 'drop') %>%
    filter(cpf != "") %>%
    mutate(treatment = ifelse(num_tuites > 0, 1, 0))
  
  cpf_range <- unique(df_agregado$cpf)
  date_range <- seq(from = as.Date("2015-02-01", "%Y-%m-%d"), to = max(df_agregado$mes_ano), by = "month")
  
  complete_data <- expand.grid(cpf = cpf_range, mes_ano = date_range) %>%
    arrange(cpf, mes_ano)
  
  # Unindo aos dados atuais e preenchendo valores ausentes
  dados_completos <- left_join(complete_data, df_agregado, by = c("cpf", "mes_ano")) %>%
    replace_na(list(num_tuites = 0, treatment = 0))
  
  return(dados_completos)
}

allowance_by_month <- function(data) {
  df <- data %>%
    filter(nu_legislatura %in% c("2015", "2019")) %>%
    mutate(month_year = floor_date(dat_emissao, "month"),
           spending = as.numeric(vlr_documento)) %>%
    filter(cpf  != "") %>%
    group_by(cpf, month_year) %>%
    summarise(spending = sum(spending))
}

plot_treatment_status <- function(data) {
  panelview(num_tuites ~ treatment, data = data, index = c("cpf", "mes_ano"),
            xlab = "month-year", ylab = "MP", axis.lab.gap = c(5,0))
}

unique_cpf <- function(data1) {
  
  base_cpf1 <- data1 %>%
    ungroup() %>%
    distinct(cpf)
}

create_aux_vec <- function(data) {
  vec <- seq(1, nrow(data), by = floor(nrow(data)/3))
}
  
sample1 <- function(data1, vec_cpf) {
  
  sample_cpf1 <-  data1 %>%
    ungroup() %>%
    distinct(cpf) %>%
    slice(vec_cpf[1]:vec_cpf[2])
}

sample2 <- function(data1, vec_cpf) {
  
  sample_cpf2 <- data1 %>%
    ungroup() %>%
    distinct(cpf) %>%
    slice(vec_cpf[2]:vec_cpf[3])
}

sample3 <- function(data1, vec_cpf, df_unique_cpf) {
  
  sample_cpf3 <- data1 %>%
    ungroup() %>%
    distinct(cpf) %>%
    slice(vec_cpf[3]:nrow(df_unique_cpf))
}

plot_outcome_treatment1 <- function(data1, data2, s_df) {
  data2 <- data2 %>%
    rename(month_year = mes_ano)
  
  df1 <- data1 %>%
    left_join(data2, by = join_by(cpf, month_year)) %>%
    mutate(num_tuites = ifelse(is.na(num_tuites), 0, 1),
           treatment = ifelse(is.na(treatment), 0, 1)) %>%
    inner_join(s_df, by= join_by(cpf))
  
  panelview(spending ~ treatment, data = df1, index = c("cpf","month_year"),
            axis.lab = "time", xlab = "Time", ylab = "Spending", show.id = c(1:200),
            theme.bw = TRUE, type = "outcome", main = "Total spending",
            axis.lab.gap = c(5,0), pre.post=TRUE, shade.post=TRUE)
}  

plot_outcome_treatment2 <- function(data1, data2, s_df) {
  data2 <- data2 %>%
    rename(month_year = mes_ano)
  
  df2 <- data1 %>%
    left_join(data2, by = join_by(cpf, month_year)) %>%
    mutate(num_tuites = ifelse(is.na(num_tuites), 0, 1),
           treatment = ifelse(is.na(treatment), 0, 1)) %>%
    inner_join(s_df, by= join_by(cpf))
  
  panelview(spending ~ treatment, data = df2, index = c("cpf","month_year"),
            axis.lab = "time", xlab = "Time", ylab = "Spending", show.id = c(1:200),
            theme.bw = TRUE, type = "outcome", main = "Total spending",
            axis.lab.gap = c(5,0), pre.post=TRUE, shade.post=TRUE)
}  

plot_outcome_treatment3 <- function(data1, data2, s_df) {
  data2 <- data2 %>%
    rename(month_year = mes_ano)
  
  df3 <- data1 %>%
    left_join(data2, by = join_by(cpf, month_year)) %>%
    mutate(num_tuites = ifelse(is.na(num_tuites), 0, 1),
           treatment = ifelse(is.na(treatment), 0, 1)) %>%
    inner_join(s_df, by= join_by(cpf))
  
  panelview(spending ~ treatment, data = df3, index = c("cpf","month_year"),
            axis.lab = "time", xlab = "Time", ylab = "Spending", show.id = c(1:200),
            theme.bw = TRUE, type = "outcome", main = "Total spending",
            axis.lab.gap = c(5,0), pre.post=TRUE, shade.post=TRUE)
}  

plot_spending_desc <- function(data) {
  
  allowance_month <- data %>%
    mutate(month_year = floor_date(dat_emissao, "month"),
           spending = as.numeric(vlr_documento)) %>%
    filter(cpf  != "") %>%
    group_by(cpf, nu_legislatura, month_year) %>%
    summarise(spending = sum(spending))
  
  allowance_month %>%
    ungroup() %>%
    group_by(month_year) %>%
    summarise(num_mp = n_distinct(cpf),
              spending = sum(spending),
              mean_spending_per_mp = sum(spending)/num_mp) %>%
    ggplot(aes(x=month_year, y = spending)) + geom_line()  +
    theme_bw() + xlab("date") + ylab("Total spending")
  
  allowance_month %>%
    ungroup() %>%
    group_by(month_year) %>%
    summarise(num_mp = n_distinct(cpf),
              spending = sum(spending),
              mean_spending_per_mp = sum(spending)/num_mp) %>%
    ggplot(aes(x=month_year, y = mean_spending_per_mp)) + geom_line() +
    theme_bw() + xlab("date") + ylab("Mean Spending per MP")
  
  allowance_month %>%
    ungroup() %>%
    group_by(month_year) %>%
    summarise(num_mp = n_distinct(cpf),
              spending = sum(spending),
              mean_spending_per_mp = sum(spending)/num_mp) %>%
    ggplot(aes(x=month_year, y = num_mp)) + geom_line() +
    theme_bw() + xlab("date") + ylab("Number of MPs")
  
}

print_summary <- function(data) {
  print(summary(data))
}
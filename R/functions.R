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
           spending = as.numeric(vlr_documento),
           net_spending = as.numeric(vlr_liquido),
           vlr_restituicao = as.numeric(vlr_restituicao)) %>%
    filter(cpf  != "") %>%
    group_by(cpf, month_year) %>%
    summarise(spending = sum(spending),
              net_spending = sum(net_spending, na.rm=T),
              mp_cashback = sum(vlr_restituicao, na.rm=T),
              sg_partido = max(sg_partido),
              sg_uf = max(sg_uf))
}

prep_data_panelmatch <- function(data1, data2) {
  
  data2 <- data2 %>%
    rename(month_year = mes_ano)
  
  df1 <- data1 %>%
    left_join(data2, by = join_by(cpf, month_year)) %>%
    ungroup() %>%
    filter(month_year > "2014-01-01") %>%
    mutate(num_tuites = ifelse(is.na(num_tuites), 0, num_tuites),
           treatment = ifelse(is.na(treatment), 0, treatment),
           cpf_id = as.integer(cpf),
           time = 1+ interval(start = min(month_year), end = month_year) / months(1),
           time = as.integer(time)) %>%
    filter(!is.na(cpf_id))
  
  df1 <- as.data.frame(df1)
  df1$time <- as.integer(df1$time)
  df1$cpf_id <- as.integer(df1$cpf_id)
  df1$uf <- as.factor(df1$sg_uf)
  df1$party <- as.factor(df1$sg_partido)
  
  return(df1)
}


plot_status <- function(data) {
  DisplayTreatment(unit.id = "cpf_id",
                   time.id = "time", legend.position = "none",
                   xlab = "date", ylab = "cpf",
                   treatment = "treatment", data = data,
                   hide.x.tick.label = TRUE, hide.y.tick.label = TRUE,
                   dense.plot = TRUE)
}

pm_maha_spending <- function(data) {
  data <- as.data.frame(data)
  
  PM.results.maha <- PanelMatch(lag = 4, time.id = "time", unit.id = "cpf_id",
                                treatment = "treatment", refinement.method = "mahalanobis",
                                # use Mahalanobis distance
                                data = data, match.missing = TRUE,
                                covs.formula = ~ uf + party,
                                size.match = 5, qoi = "att" , outcome.var = "spending",
                                lead = 0:5, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
}

pm_maha_net_spending <- function(data) {
  data <- as.data.frame(data)
  
  PM.results.maha <- PanelMatch(lag = 4, time.id = "time", unit.id = "cpf_id",
                                treatment = "treatment", refinement.method = "mahalanobis",
                                # use Mahalanobis distance
                                data = data, match.missing = TRUE,
                                covs.formula = ~ uf + party,
                                size.match = 5, qoi = "att" , outcome.var = "net_spending",
                                lead = 0:5, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
}

pm_maha_cash <- function(data) {
  data <- as.data.frame(data)
  
  PM.results.maha <- PanelMatch(lag = 4, time.id = "time", unit.id = "cpf_id",
                                treatment = "treatment", refinement.method = "mahalanobis",
                                # use Mahalanobis distance
                                data = data, match.missing = TRUE,
                                covs.formula = ~ uf + party,
                                size.match = 5, qoi = "att" , outcome.var = "mp_cashback",
                                lead = 0:5, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
}

teste_pm <- function(pe_results) {
  msets.maha <- pe_results$att
  plot(msets.maha)
}

reg_twfe <- function(pe_results, data) {
  PE.results.maha <- PanelEstimate(sets = pe_results, data = data,
                                   se.method = "bootstrap",
                                   number.iterations = 1000,
                                   confidence.level = .95)
  
}

plot_spending_desc <- function(data) {

  data %>%
    ungroup() %>%
    group_by(month_year) %>%
    summarise(num_mp = n_distinct(cpf),
              spending = sum(spending),
              mean_spending_per_mp = sum(spending)/num_mp) %>%
    ggplot(aes(x=month_year, y = spending)) + geom_line()  +
    theme_bw() + xlab("date") + ylab("Total spending") + geom_smooth(method=lm)
}


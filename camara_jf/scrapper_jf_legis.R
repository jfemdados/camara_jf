library(tidyverse)
library(rvest)
library(lubridate)

leis_jf_2020_2 <- httr::POST("https://jflegis.pjf.mg.gov.br/indexConsulta.php?pesq=1#tbr")
leis_jf_2020_nodes2<- leis_jf_2020_2%>%
  html_node('table')%>%
  html_table()

leis_jf_2020 <- read_html("leis_jf_2020.txt")

leis_jf_2020_nodes<- leis_jf_2020%>%
  html_node('table')%>%
  html_table()


leis_jf_2020_df <- leis_jf_2020_nodes[3:135,]%>%
  mutate(X2= str_squish(X2))%>%
  rename(resultado = X2)%>%
  mutate(lei = str_extract(resultado, "Lei \\d+"))%>%
  mutate(dispositivo = str_extract(resultado, "(?=)(\\d+/\\d+/\\d+).*"))%>%
  separate(dispositivo, into = c("data_promulgacao", "dispositivo"), sep= "[ ]",extra = "merge")%>%
  mutate(data_promulgacao= dmy(data_promulgacao))%>%
  mutate(dispositivo= tjsp::remover_acentos(dispositivo))%>%
  mutate(dispositivo= str_to_lower(dispositivo))%>%
  mutate(tipo_de_lei = case_when(
    str_detect(dispositivo, "titulo") ~ "cidadao honorario ou benemerito",
    str_detect(dispositivo, "denomina") ~ "nome de rua",
    str_detect(dispositivo, "dia|calendario|semana|mes") ~ "calendario",
    str_detect(dispositivo, "real|imove(l)(is)|be(m)(ns)") ~ "imoveis",
    TRUE ~ "outros"))


leis_complementares_2020 <- read_html("lei_complementar_2020.txt")

leis_complementares_2020_nodes<- leis_complementares_2020%>%
  html_node('table')%>%
  html_table()

leis_complementares_2020_df <- leis_complementares_2020_nodes[3:20,]%>%
  select(X2)%>%
  mutate(X2= str_squish(X2))%>%
  rename(resultado = X2)%>%
  mutate(lei = str_extract(resultado, "Lei Complementar \\d+"))%>%
  mutate(dispositivo = str_extract(resultado, "(?=)(\\d+/\\d+/\\d+).*"))%>%
  separate(dispositivo, into = c("data_promulgacao", "dispositivo"), sep= "[ ]",extra = "merge")%>%
  mutate(data_promulgacao= dmy(data_promulgacao))%>%
  mutate(dispositivo= tjsp::remover_acentos(dispositivo))%>%
  mutate(dispositivo= str_to_lower(dispositivo))

ggplot(leis_jf_2020_df, aes(y= tipo_de_lei)) +geom_bar()

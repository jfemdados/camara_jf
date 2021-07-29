# Camara JF- Requerimentos
# Autor - Marcello Filgueiras

library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)


# Agora não precisa de Scrapper =D. Câmara fez um "Exportar Excel"



# Importing ---------------------------------------------------------------

requerimentos_raw <- read_delim("camara_jf/requerimentos/data_raw/requerimentos.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)


# Tidying -----------------------------------------------------------------


tidyer_camara_jf_requerimento <- function(x) {
  
  situacao_regex<- paste("Avulso",
                         "Rejeitado",
                         "Retirada Temporária",
                         "Retirado Definitivamente",
                         sep = "|")
  
  x %>%
    select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
    filter(Projeto != is.na(Projeto)) %>%
    janitor::clean_names() %>%
    mutate(ementa_alterada= str_to_lower(str_squish(ementa)),
           situacao = str_replace(situacao, situacao_regex, "Arquivada" ),
           situacao = str_replace(situacao, "Aprovado", "Aprovada"),
           situacao = str_remove (situacao, " com Encaminhamento para Executivo"))
}


requerimentos_tidy <- requerimentos_raw %>%
  tidyer_camara_jf_requerimento()



table(requerimentos_tidy $ ano)

table(requerimentos_tidy $ situacao)




# Classificação -----------------------------------------------------------------

#por tema

requerimentos_classificado <- requerimentos_tidy %>%
  mutate(tema= case_when(
    str_detect(ementa_alterada, "capina|\\bro[cç]a|\\bpod[ae](s)*\\b|[áa]rvore") ~ "Capina e Podas de Árvores",
    str_detect(ementa_alterada, "buraco|esc[oó]ria|asf[áa]lt|tampa|cascalh|patrol") ~ "Tapar Buracos e Asfaltamento",
    str_detect(ementa_alterada, "m[ée]dico|\\bubs\\b|sa[úu]de|vacina|covid") ~  "Saúde e Vacinação",
    str_detect(ementa_alterada, "ilumina[cç][aã]o|poste|l[âa]mpada|\\bled\\b") ~ "Troca de Lâmpadas em Postes",
    str_detect(ementa_alterada, "\\blinha\\b|[ôo]nibus") ~ "Ônibus e Transporte Público",
    str_detect(ementa_alterada, "transito|trânsito|mão dupla|mão única|radar|quebra[- ]molas|pedestre|redutor(es)* de velocidade|sinal|placa|rotat[óo]ria") ~ "Trânsito",
    str_detect(ementa_alterada, "esgoto|\\b[aá]gua|canal|manilha") ~ "Saneamento Básico",
    str_detect(ementa_alterada, "limpeza|entulho|boca de lobo") ~ "Limpeza de Vias Públicas",
    str_detect(ementa_alterada, "pavimenta[cç][aã]o") ~ "Tapar Buracos e Asfaltamento",
    TRUE ~  "Outros"))



requerimentos_classificado %>%
  filter(ano==2021) %>%
  count(tema) %>%
  arrange(desc(n))


requerimentos_classificado %>%
  filter(ano==2021) %>%
  select(!ementa) #%>%
  #view()
  #DT::datatable(extensions= "Responsive")

# Por Autor

requerimentos_autor <- requerimentos_classificado %>%
  mutate(autor = str_split(autor, ",")) %>%
  unnest(autor) %>%
  mutate(autor=str_squish(autor))


# Filtro de 2021


requerimentos_2021 <- requerimentos_classificado %>%
  filter(ano == 2021)


requerimentos_autor_2021 <- requerimentos_autor %>%
  filter(ano == 2021)

req_autor_2021_count <-requerimentos_autor_2021  %>%
  count(autor, name = "n_requerimentos") %>%
  arrange(desc(n_requerimentos)) %>%
  mutate(
   autor = forcats::fct_reorder(autor,
                                n_requerimentos))



# Requerimento Discutidos e Aprovados não fazem sentido
# por que todos Requerimentos são aprovados


# Visualização ------------------------------------------------------------

library(ggplot2)

# Gráfico 4A - Requerimentos por Autor

vermelho<- "#a50c0c"

req_autor_2021_count %>%
  ggplot(aes(y=autor, x= n_requerimentos)) +
  geom_col( fill= vermelho, color = "gray20") +
  theme_light() +
  labs(title= "Numero de Requerimentos por Vereador",
       subtitle= "Na função de fiscalização da Prefeitura, requerimentos são pedidos dos Vereadores à Prefeitura\npara tomar providências",
       caption = " Fonte: Câmara JF - Elaboração: JF em Dados")


# 4b -Requerimentos por autor agrupado por tema

requerimentos_classificado %>%
  #Filtrando
  filter(ano==2021) %>%
  # Unnesting
  mutate(autor = str_split(autor, ",")) %>%
  unnest(autor) %>%
  mutate(autor=str_squish(autor))%>%
  # Contando
  group_by(tema) %>%
  count(autor) %>%
  mutate(autor = forcats::fct_reorder(autor,n)) %>%
  #ggploting
  ggplot(aes(y=autor, x= n, fill= tema)) +
  geom_col( #position = "fill",
            color = "gray20") +
  theme_light() +
  labs(title= "Numero de Requerimentos por Vereador",
       subtitle= "Na função de fiscalização da Prefeitura, requerimentos são pedidos dos Vereadores à Prefeitura\npara tomar providências",
       caption = " Fonte: Câmara JF - Elaboração: JF em Dados")



# 4c -Requerimentos por autor no percentual do tema

requerimentos_classificado %>%
  #Filtrando
  filter(ano==2021) %>%
  # Unnesting
      mutate(autor = str_split(autor, ",")) %>%
      unnest(autor) %>%
      mutate(autor=str_squish(autor))%>%
  # Contando
  group_by(tema) %>%
      count(autor) %>%
      mutate(autor = forcats::fct_reorder(autor,n)) %>%
  #ggploting
  ggplot(aes(y=autor, x= n, fill= tema)) +
  geom_col( position = "fill", color = "gray20") +
  theme_light() +
  labs(title= "Numero de Requerimentos por Vereador",
       subtitle= "Na função de fiscalização da Prefeitura, requerimentos são pedidos dos Vereadores à Prefeitura\npara tomar providências",
       caption = " Fonte: Câmara JF - Elaboração: JF em Dados")



# Requerimentos Total Tema

requerimentos_classificado %>%
  #Filtrando
  filter(ano==2021) %>%
# Contando
  count(tema) %>%
  mutate(tema = forcats::fct_reorder(tema,n)) %>%
  #ggploting
  ggplot(aes( x=n, y= tema)) +
  geom_col( #position = "fill",
            color = "black",  fill= vermelho) +
  theme_light() +
  labs(title= "Numero de Requerimentos por Vereador",
       subtitle= "Na função de fiscalização da Prefeitura, requerimentos são pedidos dos Vereadores à Prefeitura\npara tomar providências",
       caption = " Fonte: Câmara JF - Elaboração: JF em Dados")



# Exporting para Flourish -------------------------------------------------


#Número de Requerimentos por Vereador

rio::export( req_autor_2021_count #%>%  pivot_wider(names_from = autor, values_from= n_requerimentos)
            ,file= "camara_jf/requerimentos/exports/requerimentos_por_autor.csv" )


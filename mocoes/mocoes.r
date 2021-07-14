# Analisando TODOS PLs da Câmara de Juiz de Fora
# Autor: Marcello Filguieras

library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)
library(tidyr)




# Agora não precisa de Scrapper =D. Câmara fez um "Exportar Excel"



# Importing ---------------------------------------------------------------

mocoes_raw <- read_delim("camara_jf/mocoes/data_raw/mocoes.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)


# Tidying -----------------------------------------------------------------


tidyer_camara_jf_mocoes <- function(x) {
  
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
           situacao = str_replace(situacao,"Aprovado","Aprovada"),
           situacao = str_replace(situacao, situacao_regex, "Arquivada" ))
}


mocoes_tidy <- mocoes_raw %>%
  tidyer_camara_jf_mocoes()



table(mocoes_tidy$ano)



# Classificando -----------------------------------------------------------



mocoes_classificado <- mocoes_tidy %>%
  mutate(
    grande_tema= case_when(
      str_detect(ementa_alterada, "aplauso") ~ "Moção de Aplauso",
      str_detect(ementa_alterada, "rep[úu]dio") ~ "Moção de Repúdio",
      str_detect(ementa_alterada, "pesar") ~ "Moção de Pesar",
      str_detect(ementa_alterada, "mo[çc][ãa]o de apoio") ~ "Moção de Apoio",
      TRUE ~ "Não Especificado"),
    tema_especifico = case_when(
      str_detect(ementa_alterada, "falecimento|morte do|post mortem") ~ "Falecimento",
      str_detect(ementa_alterada, "atlet|vôlei|basquete|futebol|bom pastor|malha clube|natação|jiu") ~ "Moções a Esportistas",
      str_detect(ementa_alterada, "lanche|restaurante|pizza|cachorro|hamb|cervej|caf[ée]") ~ "Moções a Restaurantes",
      str_detect(ementa_alterada, "escola[s](?! de samba)|col[eé]gio|professor|alun[oa]|faculdade|universidade") ~ "Moções a Escolas, Prof. e Alunos",
      str_detect(ementa_alterada, "pol[ií]cia|sgt|sargento|\\bcabo\\b|\\bcb\\b|\\bpm") ~ "Moções a Policiais",
      str_detect(ementa_alterada, "padre|bispo|pastor|igreja") ~ "Moções a Autoridades Religiosas",
      str_detect(ementa_alterada, "govern|projeto de lei|\\bpl\\b|\\blula\\b|\\bdilma\\b") ~ "Moções Políticas"
      #TRUE ~ "Outros"
      ))


table(mocoes_classificado$tema_especifico)

table(mocoes_classificado $ situacao)
table(mocoes_tidy $ situacao)
table(mocoes_raw $ Situação)


table(mocoes_classificado$tema_especifico)


#Moções dividas por cada vereador

mocoes_unnest <- mocoes_classificado %>%
  mutate(autor = str_split(autor, ",")) %>%
  unnest(autor)%>%
  mutate(autor=str_squish(autor))

mocoes_unnest %>%
  count(autor) %>%
  arrange(desc(n))


# Filtrando Legislatura de 2021 ------


mocoes_2021 <- mocoes_classificado %>%
  filter(ano == 2021)

mocoes_2021_por_autor <- mocoes_unnest %>%
  filter(ano == 2021)

mocoes_2021 %>%
  count(tema_especifico) %>%
  arrange(desc(n)) %>%
  view()

#%>%
 # mutate(
  #  autor = forcats::fct_reorder(autor,
   #                              impacto_legislativo == "Legislação Simbólica ou Irrelevante",
    #                             .fun = "mean")
  #)



# Indicadores - Resultados ------------------------------------------------


#Contando por tema  dos apresentados 

mocoes_2021%>%
  count(tema_especifico) %>%
  arrange(desc(n)) #%>% view()


#Contando por tema por aprovado ou reprovado 
mocoes_2021%>%
  count(situacao)#%>% view()




# Visualização ----------------------------------------------------------------


# Por Vereador ------------------------------------------------------------

mocoes_2021_por_autor%>%
ggplot(aes(x=autor, fill= tema_especifico )) +
  geom_bar( width = 0.5, color="black") +
  ggplot2::theme_minimal() + 
  labs(title= "Qual o quantidade de Moções apesentadas por Vereador na Câmara JF?",
       subtitle = "Relatório 6 Meses da Câmara -Legislatura 2021-2024", caption = "Fonte: Site Oficial Câmara Municipal - Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + ylab(label = "Número de Moções")
  #scale_y_continuous(name = "Porcentagem entre nº Projetos",
   #                  labels = c("0%","25%", "50%","75%" , "100%")) +
  #scale_fill_manual(values=c( verde, vermelho),
   #                 name= "Tipo de Projeto", 
    #                labels= c("Outros PLs em geral",
     #                         "Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário")
  #geom_hline(yintercept = 0.5, color = "black", linetype = 2) 


+ theme(legend.position = "top") 




mocoes %>%
  ggplot( aes(x=tema_especifico, fill= situacao)) + geom_bar(width = 0.7, color= "black")+
  labs(title= "Relatório 6 Meses da Câmara - Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura de 2021 - Autoria Executivo e Legislativo", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 50, hjust = 1))


# Export ------------------------------------------------------------------

#CSV
rio::export(mocoes_classificado, file= "camara_jf/mocoes/exports/mocoes_classificado.csv")
rio::export(mocoes_2021, file= "camara_jf/mocoes/exports/mocoes_classificado_2021.csv")

#XLSX

writexl::write_xlsx(mocoes_classificado, path= "camara_jf/mocoes/exports/mocoes_classificado.xlsx")
writexl::write_xlsx(mocoes_2021, path = "camara_jf/mocoes/exports/mocoes_classificado_2021.xlsx")


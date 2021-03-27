#análise PLs Covid

library(tidyverse)

library(readxl)

pls_juiz_de_fora2021 <- read_excel("camara_jf/pls juiz de fora2021.xlsx")%>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
  filter(Projeto != is.na(Projeto))%>%
  mutate(Ementa_Alterada= str_to_lower(str_squish(Ementa)))%>%
  #Fazendo Filtro
  mutate(Tema = case_when(
    str_detect(Ementa_Alterada, "próprio|próprios") ~ "Nome de Prédios",
    str_detect(Ementa_Alterada, "logradou") ~ "Nome de Rua",
    str_detect(Ementa_Alterada, "dia |calendário|semana|mês") ~ "Criação de Dias Comemorativos",
    #str_detect(Ementa_Alterada, "\\breal\\b|\\bimóv(el)(éis)\\b|\\bbe(m)(ns)\\b") ~ "imoveis",
    str_detect(Ementa_Alterada, "benemérit|honorári|honorífico") ~ "Cidadão Benemérito ou Honorário",
    TRUE ~ "Outros"))%>%
  #####contando o Impacto Legislativo
  mutate(ImpactoLegislativo = case_when(
    Tema%in% pl_inuteis_classe ~ "Legislação Simbólica ou Irrelevante",
    Tema == "Outros" ~ "Impacto Variado",  
  ))%>%
  mutate(Situação= case_when(
    Situação == "Transformado em Norma Jurídica" ~ "Aprovada",
    Situação == "Em Tramitação - Vetado Parcialmente" ~ "Em Tramitação",
    TRUE ~ as.character(Situação)
  ))

  




pls_2017_2021<-full_join(pls_juiz_de_fora2021, todos_pls_2017_2020)%>%
  ### TEMAS COVID
  mutate(Tema= case_when(
  str_detect(Ementa_Alterada, "essenc") ~ "Essencial para COVID",
  str_detect(Ementa_Alterada, "covid") ~ "Menciona COVID",
  TRUE ~ as.character(Tema)),
  Grandes_Temas= case_when(
    Tema %in% c("Essencial para COVID", "Menciona COVID") ~ "COVID",
    Tema %in% pl_inuteis_classe ~ "Irrelevante ou Simbólico",
    Tema == "Outros" ~ "Outros"
  ))
  


#data frame interativo
DT::datatable(pls_2017_2021, extensions = "Responsive")


pls_2017_2021_split<- pls_2017_2021%>%
  mutate(Autor = str_split(Autor, ","))%>%
  unnest(Autor)%>%
  mutate(Autor=str_squish(Autor))%>%
  mutate( Autor = forcats::fct_reorder(Autor, Grandes_Temas== "COVID", .fun = "mean")
  )%>%
  filter(!Autor %in% c("Jucelio"))


pls_2017_2021_split%>%
  filter(Ano>=2020)%>%
ggplot(aes(y= Autor, fill= Grandes_Temas)) +geom_bar(position = "fill")+
  labs(title = "Grandes Temas dos PLs discutidos na Câmara", subtitle = "Desde 2020")

  



#grafico

count_vereadores_2<- pls_2017_2021_split%>%
  group_by(Autor)%>%
  count(Tema)

pls_2017_2021_split%>%
  filter(Ano >= 2020)%>%
ggplot(aes(y= Autor, fill= ImpactoLegislativo )) + geom_bar(position = "fill")

####### ANALISANDO TEMAS######


pls_sargento_mello<- pls_2017_2021_split%>%
  filter(Autor== "Sargento Mello Casal")%>%
  




%>%
  ggplot(aes(y= Autor, fill= ImpactoLegislativo )) + geom_bar(position = "fill")




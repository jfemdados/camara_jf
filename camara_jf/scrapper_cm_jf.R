library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)
library(tidyr)

################## TEMAS DA CÂMARA####################################

### PLs de Autoria dos Vereadores ######

pls_juiz_de_fora_2017_2020 <- read_excel("camara_jf/pls juiz de fora2.xlsx", skip= 2)

#contando numero de processos inuteis
pl_inuteis_classe <- c("Cidadão Benemérito ou Honorário", "Criação de Dias Comemorativos", "Nome de Rua", "Nome de Prédios", "imoveis")


#faxinando os dados para tirar os NAs e colunas vazias
pls_juiz_de_fora_2017_2020_2 <- pls_juiz_de_fora_2017_2020%>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
  filter(Projeto != is.na(Projeto))%>%
  mutate(Ementa_Alterada= str_to_lower(str_squish(Ementa)))%>%
  mutate(Ementa_Alterada= str_to_lower(Ementa_Alterada))%>%
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

#conta o tipo de leis
table(pls_juiz_de_fora_2017_2020_2$Tema)

#data frame interativo
DT::datatable(pls_juiz_de_fora_2017_2020_2, extensions = "Responsive")


#Qual diferença desse df?

#projetos_inuteis_df <- pls_juiz_de_fora_2017_2020_2%>%
 # mutate(ImpactoLegislativo = case_when(
  #  Tema%in% pl_inuteis_classe ~ "Legislação Simbólica ou Irrelevante",
   # Tema == "Outros" ~ "Impacto Variado",  
    #  ))

#DT::datatable(projetos_inuteis_df, extensions = "Responsive")

####  PLs Iniciativa Executivo #########

pls_iniciativa_executivo_2017_2020 <- read_excel("camara_jf/pl_iniciativa_executivo_2017_2020.xlsx")

pls_iniciativa_executivo_2017_2020 <- pls_iniciativa_executivo_2017_2020%>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
  filter(Projeto != is.na(Projeto))%>%
  mutate(Autor = replace_na(Autor, "Executivo"), Tipo = "Projeto de Lei")%>%
  mutate(Ementa_Alterada= str_to_lower(str_squish(Ementa)))%>%
  #fazendo os filtros
  mutate(Tema = case_when(
   # str_detect(Ementa_Alterada, "próprio|próprios") ~ "Nome de Prédios",
    str_detect(Ementa_Alterada, "logradou") ~ "Nome de Rua",
   # str_detect(Ementa_Alterada, "dia |calendário|semana|mês") ~ "Criação de Dias Comemorativos",
    #str_detect(Ementa_Alterada, "\\breal\\b|\\bimóv(el)(éis)\\b|\\bbe(m)(ns)\\b") ~ "imoveis",
    #str_detect(Ementa_Alterada, "benemérit|honorári|honorífico") ~ "Cidadão Benemérito ou Honorário",
    TRUE ~ "Outros"))%>%
  #####contando o ImpactoLegislativo
  mutate(ImpactoLegislativo = case_when(
    Tema%in% pl_inuteis_classe ~ "Legislação Simbólica ou Irrelevante",
    Tema == "Outros" ~ "Impacto Variado",  
  ))%>%
  mutate(Situação= case_when(
    Situação == "Transformado em Norma Jurídica" ~ "Aprovada",
    Situação == "Em Tramitação - Vetado Parcialmente" ~ "Em Tramitação",
    TRUE ~ as.character(Situação)
  ))

#### PLs Leis Complementares - Câmara ####

pls_lei_complementar <- read_excel("camara_jf/lei_complementar.xlsx")

pls_lei_complementar <-  pls_lei_complementar%>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
  filter(Projeto != is.na(Projeto))%>%
  #fazendo os filtros
  mutate(Ementa_Alterada= str_to_lower(str_squish(Ementa)), Tipo= "Projeto de Lei Complementar")%>%
  mutate(Tema = case_when(TRUE ~ "Outros"))%>%
  #####contando o ImpactoLegislativo
  mutate(ImpactoLegislativo = case_when(
    Tema%in% pl_inuteis_classe ~ "Legislação Simbólica ou Irrelevante",
    Tema == "Outros" ~ "Impacto Variado",  
  ))%>%
  mutate(Situação= case_when(
    Situação == "Transformado em Norma Jurídica" ~ "Aprovada",
    Situação == "Em Tramitação - Vetado Parcialmente" ~ "Em Tramitação",
    TRUE ~ as.character(Situação)
  ))

pls_lei_complementar_2017_2020 <- pls_lei_complementar%>%
  filter(Ano >= 2017)


#### TODOS os PLs discutidos na Câmara ####

todos_pls_2017_2020 <- full_join(pls_juiz_de_fora_2017_2020_2, pls_iniciativa_executivo_2017_2020)%>%
full_join(pls_lei_complementar_2017_2020)#%>%
#mutate(Tema = case_when(
 # str_detect(Ementa_Alterada, "próprio|próprios") ~ "Nome de Prédios",
  #str_detect(Ementa_Alterada, "logradou") ~ "Nome de Rua",
#  str_detect(Ementa_Alterada, "dia |calendário|semana|mês") ~ "Criação de Dias Comemorativos",
  #str_detect(Ementa_Alterada, "\\breal\\b|\\bimóv(el)(éis)\\b|\\bbe(m)(ns)\\b") ~ "imoveis",
 # str_detect(Ementa_Alterada, "benemérit|honorári|honorífico") ~ "Cidadão Benemérito ou Honorário",
#  TRUE ~ "Outros"))%>%
  #####contando o ImpactoLegislativo
 # mutate(ImpactoLegislativo = case_when(
  #  Tema%in% pl_inuteis_classe ~ "Legislação Simbólica ou Irrelevante",
   # Tema == "Outros" ~ "Impacto Variado",  
#  ))%>%
 # mutate(Situação= case_when(
  #  Situação == "Transformado em Norma Jurídica" ~ "Aprovada",
   # Situação == "Em Tramitação - Vetado Parcialmente" ~ "Em Tramitação",
  #  TRUE ~ as.character(Situação)
#  ))

#conta o tipo de leis
table(todos_pls_2017_2020$ImpactoLegislativo)


################# - * PROJETOS Por VEREADOR * ######################33


#Filtrando a base por Vereador
#numero de PLs maior do que o numero real de projetos, 
#porque Projetos com Mais de um autor foram divididos em linhas diferentes


pls_por_vereador_split <- full_join(pls_juiz_de_fora_2017_2020_2, pls_lei_complementar_2017_2020)%>%
  mutate(Autor = str_split(Autor, ","))%>%
  unnest(Autor)%>%
  mutate(Autor=str_squish(Autor))%>%
  mutate(ImpactoLegislativo = case_when(
    Tema%in% pl_inuteis_classe ~ "Legislação Simbólica ou Irrelevante",
    Tema == "Outros" ~ "Impacto Variado",  
  ))%>%
  mutate( Autor = forcats::fct_reorder(Autor, ImpactoLegislativo == "Legislação Simbólica ou Irrelevante", .fun = "mean")
  )%>%
  filter(!Autor %in% c("Jucelio"))



###### - PLs Aprovados ####

leis_2017_2020 <- todos_pls_2017_2020%>%
  filter(Situação == "Aprovada")

table(leis_2017_2020$ImpactoLegislativo)

#### Data Frames para o FLOURISH ####

### Geral ####

# Impacto Legislativo em Geral

impacto_geral <- todos_pls_2017_2020%>%
  count(ImpactoLegislativo, Tema)%>%
  mutate(porcentagem = n/sum(n))%>%
   pivot_wider(names_from = Tema, values_from= c(n, porcentagem))
  
impacto_geral_aprovados <- todos_pls_2017_2020%>%
  filter(Situação == "Aprovada")%>%
  count(ImpactoLegislativo, Tema)%>%
  mutate(porcentagem = n/sum(n))%>%
  pivot_wider(names_from = Tema, values_from= c(n, porcentagem))


#### Por Vereador ####

##  Impacto Legislativo por Vereador
impacto_vereador <- pls_por_vereador_split%>%
  group_by(Autor)%>%
  count(ImpactoLegislativo)%>%
  mutate(porcentagem = n/sum(n))%>%
  pivot_wider(names_from = ImpactoLegislativo, values_from= c(n, porcentagem) )

## Taxa de Aprovação por Vereador 

aprovação_vereador <- pls_por_vereador_split%>%
  group_by(Autor)%>%
  count(Situação)%>%
  mutate(porcentagem = n/sum(n))%>%
  pivot_wider(names_from = Situação, values_from= c(n, porcentagem))


count_vereador <- inner_join(impacto_vereador, aprovação_vereador, by="Autor")
  
aprovação_vereador_por_impacto <-  pls_por_vereador_split%>%
  group_by(Autor, ImpactoLegislativo)%>%
  count(Situação)%>%
  mutate(porcentagem = n/sum(n))%>%
  pivot_wider(names_from = c(Situação, ImpactoLegislativo), values_from= c(n, porcentagem))


aprovação_vereador_por_impacto_facet <- pls_por_vereador_split%>%
  group_by(Autor, ImpactoLegislativo)%>%
  count(Situação)%>%
  mutate(porcentagem = n/sum(n))%>%
  pivot_wider(names_from = c(Situação), values_from= c(n, porcentagem))



####Taxa de Aprovação por Tema Por Vereador#### #Se pa muita informação

aprovação_vereador_por_tema <- pls_por_vereador_split%>%
  group_by(Autor, Tema)%>%
  count(Situação)%>%
  mutate(porcentagem = n/sum(n))%>%
  pivot_wider(names_from = Tema, values_from= c(n, porcentagem))




### exportando para Excel ####

writexl::write_xlsx(x= list(pls_juiz_de_fora_2017_2020_2, pls_lei_complementar_2017_2020,
                            pls_iniciativa_executivo_2017_2020, todos_pls_2017_2020,
                            pls_por_vereador_split, leis_2017_2020, count_vereador),
                    path = "flourish/jf_legis_2017_2020_2.xlsx", col_names= TRUE, format_headers = TRUE)


writexl::write_xlsx(x= aprovação_vereador_por_impacto_facet,
                    path = "flourish/aprovação_vereador_por_impacto_facet.xlsx", col_names= TRUE, format_headers = TRUE)

writexl::write_xlsx(x= list(count_vereador, aprovação_vereador_por_impacto, aprovação_vereador_por_tema),
                    path = "flourish/count_vereador_wide_2.xlsx",
                    col_names= TRUE, format_headers = TRUE)


writexl::write_xlsx(x= list(impacto_geral,impacto_geral_aprovados),
                    path = "flourish/count_geral.xlsx",
                    col_names= TRUE, format_headers = TRUE)

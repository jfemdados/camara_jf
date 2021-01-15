library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)
pls_juiz_de_fora_2017_2020 <- read_excel("pls juiz de fora2.xlsx", skip= 2)

#faxinando os dados para tirar os NAs e colunas vazias
pls_juiz_de_fora_2017_2020_2 <- pls_juiz_de_fora_2017_2020%>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
  filter(Projeto != is.na(Projeto))%>%
  #fazendo os filtros
  mutate(Ementa= str_squish(Ementa))%>%
  mutate(Ementa= str_to_lower(Ementa))%>%
  mutate(tipo_de_lei = case_when(
    str_detect(Ementa, "próprio") ~ "nome de prédios",
    str_detect(Ementa, "próprios") ~ "nome de prédios",
    str_detect(Ementa, "logradou") ~ "nome de rua",
    str_detect(Ementa, "dia |calendário|semana|mês") ~ "calendario",
    str_detect(Ementa, "real|imóv(el)(éis)|be(m)(ns)") ~ "imoveis",
    str_detect(Ementa, "benemérito|honorário|honorífico|título de utilidade pública") ~ "cidadão benemérito ou honorário",
    TRUE ~ "outros"))%>%
  #####contando a inutilidade
  mutate(Utilidade = case_when(
    tipo_de_lei%in% pl_inuteis_classe ~ "Com certeza Inútil",
    tipo_de_lei == "outros" ~ "Utilidade Questionável",  
  ))

#conta o tipo de leis
table(pls_juiz_de_fora_2017_2020_2$tipo_de_lei)

#data frame interativo
DT::datatable(pls_juiz_de_fora_2017_2020_2, extensions = "Responsive")

#contando numero de processos inuteis
pl_inuteis_classe <- c("cidadão benemérito ou honorário", "calendario", "nome de rua", "nome de prédios", "imoveis")
projetos_inuteis_df <- pls_juiz_de_fora_2017_2020_2%>%
  mutate(Utilidade = case_when(
    tipo_de_lei%in% pl_inuteis_classe ~ "Com certeza Inútil",
    tipo_de_lei == "outros" ~ "Utilidade Questionável",  
      ))


#projetos_inuteis_num <- pl_c


DT::datatable(projetos_inuteis_df, extensions = "Responsive")


###################3 GRAFICOS ######################3


#grafico geral de tipo de lei

ggplot(pls_juiz_de_fora_2017_2020_2, aes(x=tipo_de_lei, fill= Situação)) +geom_bar()
#+
#geom_text(aes(label = count(tipo_de_lei))) 



################# - Por Vereador ######################33



#Filtrando a base por Vereador

pls_por_vereador_split <- pls_juiz_de_fora_2017_2020_2%>%
  mutate(Autor = str_split(Autor, ","))%>%
  unnest(Autor)%>%
  mutate(Autor=str_squish(Autor))

#grafico de leis inuteis por vereador
ggplot(pls_por_vereador_split, aes(x=tipo_de_lei)) +geom_bar() + facet_wrap("Autor")
  

#############################3 - Pls que Viraram Lei

leis_2017_2020 <- pls_juiz_de_fora_2017_2020_2%>%
  filter(Situação == "Transformado em Norma Jurídica")

leis_2017_2020%>%
  ggplot(aes(x=tipo_de_lei)) + geom_bar() + theme_bw() +
  labs(title= "Qual conteúdo das Leis Aprovadas pela Câmara Municipal de Juiz de Fora?",
        subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")

#+ geom_text(label = table(leis_2017_2020$tipo_de_lei))

#### Grafico Leis Inúteis


leis_2017_2020%>%
  ggplot(aes(x=Utilidade, fill= tipo_de_lei)) + geom_bar(width = 0.5) + theme_bw() +
  labs(title= "Qual conteúdo das Leis Aprovadas pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")

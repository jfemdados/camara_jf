library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)
library(magick)
library(showtext)

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


#Df Porcentagem de Projetos Inúteis
count_vereador <- pls_por_vereador_split%>%
  group_by(Autor)%>%
  count(ImpactoLegislativo)%>%
  mutate(porcentagem = n/sum(n))




###### - PLs Aprovados ####

leis_2017_2020 <- todos_pls_2017_2020%>%
  filter(Situação == "Aprovada")

table(leis_2017_2020$ImpactoLegislativo)


### exportando para Excel ####

writexl::write_xlsx(x= list(pls_juiz_de_fora_2017_2020_2, pls_lei_complementar_2017_2020,
                         pls_iniciativa_executivo_2017_2020, todos_pls_2017_2020,
                         pls_por_vereador_split, leis_2017_2020, count_vereador),
                         path = "flourish/jf_legis_2017_2020_2.xlsx", col_names= TRUE, format_headers = TRUE)

writexl::write_xlsx(x= pls_lei_complementar_2017_2020, path = "flourish/pls_juiz_de_fora_2017_2020_2.xlsx", col_names= TRUE)
writexl::write_xlsx(x= pls_iniciativa_executivo_2017_2020, path = "flourish/pls_juiz_de_fora_2017_2020_2.xlsx", col_names= TRUE)
writexl::write_xlsx(x= todos_pls_2017_2020, path = "flourish/pls_juiz_de_fora_2017_2020_2.xlsx", col_names= TRUE)
writexl::write_xlsx(x= pls_juiz_de_fora_2017_2020_2, path = "flourish/pls_juiz_de_fora_2017_2020_2.xlsx", col_names= TRUE)






xlsx::write.xlsx(x= pls_juiz_de_fora_2017_2020_2, file= "flourish/pls_juiz_de_fora_2017_2020_2.xlsx", col.names= TRUE)



########## GRÁFICOS #################

imagem<- magick::image_read("JFEMDADOS2.png")

vermelho<- "#a50c0c"
verde<- "#06a35d"
amarelo <- "#f3d40c"
azul <- "#36489e"

paleta<- c("#619CFF", "#00BA38", "#F8766D", "#619CFF", "red")



font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")

#GERAIS
 
###Tipos de Leis####

leis_2017_2020%>%
  ggplot(aes(x=Tipo)) + geom_bar() + theme_bw() +
  labs(title= "Qual Tipos de Projetos de Lei aprovados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Restrito aos PLs de Autoria dos próprios vereadores, excluidos autoria do Executivo \n Legislatura 2017-2020",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")


### Temas das Leis ###

leis_2017_2020%>%
  ggplot(aes(x=Tema)) + geom_bar() + theme_bw() +
  labs(title= "Qual conteúdo dos Projetos de Lei aprovados pela Câmara Municipal de Juiz de Fora?",
        subtitle = "Restrito aos PLs de Autoria dos próprios vereadores, excluidos autoria do Executivo \n Legislatura 2017-2020",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")

#+ geom_text(label = table(leis_2017_2020$Tema))
####Impatcto Relativo vs Legislação Simbólica ou Irrelevante ####

####Taxa de Aprovação por Tema####

#grafico geral de Tema do Pl vs Aprovação

ggplot(todos_pls_2017_2020, aes(x=Tema, fill= Situação)) +geom_bar(width = 0.7)+
  labs(title= "Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
   theme_minimal() +theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_fill_manual(values=c(verde, azul, amarelo)) +
  scale_x_discrete(name = "Tema dos Projetos", label= c("Dias Comemorativos", "Cidadãos Honorários", "Alteração Nome de Prédios", "Alteração de Nomes de Ruas", "Outros PLs em geral")) + ylab(label = "Nº de PLs") 
#+
#geom_text(aes(label = count(Tema))) 

# Grafico Leis Aprovados por Tema

leis_2017_2020%>%
  ggplot(aes(x=ImpactoLegislativo, fill= Tema)) + geom_bar(width = 0.5) + theme_minimal() +
  labs(title= "Qual conteúdo dos Projetos de Lei aprovados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Restrito aos PLs de Autoria dos próprios vereadores, excluidos os de autoria do Executivo \nLegislatura 2017-2020",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  scale_x_discrete(name = "", labels = c("Legislação Simbólica ou Irrelevante", "Outros Projetos em Geral")) + ylab(label = "Nº de Projetos Aprovados") +
  scale_fill_discrete(name= "Tema de Projeto", labels= c("Criação de Dias Comemorativos","Concessão de título de Cidadão honorário","Alteração de Nomes de Prédios", "Alteração de Nomes de Ruas", "Outros PLs em Geral"))

####Executivo vs Legislativo####
####Executivo vs Legislativo - Por Ano####
#### Taxa de Aprovação - Exec vs Legislativo #####



#POR VEREADOR
#Impacto Variado vs Legislação Simbólica ou Irrelevante por Vereador


#fill
ggplot(pls_por_vereador_split, aes(x=Autor, fill= ImpactoLegislativo, label= )) +geom_bar(position = "fill", width = 0.5)+
  #colocando label
  #geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 1) ),
   #         stat="count", position=position_fill(0.5), vjust=0.5, check_overlap=TRUE) +
   ggplot2::theme_minimal() + 
  labs(title= "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + scale_y_continuous(name = "Porcentagem entre nº Projetos", labels = c("0%","25%", "50%","75%" , "100%")) +
  scale_fill_manual(values=c(azul, verde), name= "Tipo de Projeto", labels= c("Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário","Outros PLs em geral")) +
  geom_hline(yintercept = 0.5, color = "grey40", linetype = 2) + theme(legend.position = "top") 

#, text=element_text(family="Montserrat"))+

pl_vereador_fill<- image_read("pl_vereador_tema.png")%>%
  image_composite(imagem, offset = "+22+1826")

ggplot(pls_por_vereador_split, aes(x=Autor, fill= ImpactoLegislativo)) +geom_bar(position = "dodge", width = 0.5)+
  ggplot2::theme_minimal() +
  labs(title= "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + ylab(label = "Nº Projetos") +
  scale_fill_discrete(name= "Tipo de Projeto", labels= c("Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário","Outros PLs em geral"))

#grafico de leis inuteis por vereador - facet_Wrap
ggplot(pls_por_vereador_split, aes(x=Tema, fill = factor( ..count..))) +geom_bar() + 
   facet_wrap("Autor") #+ scale_fill_manual(paleta)

  #facetado nao deu muito certo
#ggplot(pls_por_vereador_split, aes(Tema, fill = count)) + geom_bar()  + 
 # facet_wrap(~coluna) + scale_fill_manual(paleta)

###Taxa de Aprovação por Vereador###

####Taxa de Aprovação por Tema Por Vereador####



# Exemplo útil

#decisões_merito_prop <- stay_final_merito%>%
 # ggplot(aes(x=tipo_parte, fill=decisão)) + geom_bar(position="fill", width = 0.5) +
  #labs(x = "Natureza da Parte",y="Porcentagem de Decisões em cada Parte", fill = "Decisão do Juízo",
   #    title = "Proporção de Provimentos ou Improvimentos em Agravos sobre Prorrogação do Stay Period", subtitle = "Porcentagem por Posição do Agravante em Credor ou Recuperando", caption = "Fonte: TJSP") +
#  scale_x_discrete(labels = c("Credor", "Empresa Recuperanda")) +
 # scale_fill_discrete(labels= c("Improvido", "Parcialmente Provido", "Provido")) + theme_classic()
#decisões_merito_prop

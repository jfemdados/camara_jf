library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)

######################################################################
################## TEMAS DA CÂMARA####################################

### PLs de Autoria dos Vereadores ######

pls_juiz_de_fora_2017_2020 <- read_excel("camara_jf/pls juiz de fora2.xlsx", skip= 2)

#faxinando os dados para tirar os NAs e colunas vazias
pls_juiz_de_fora_2017_2020_2 <- pls_juiz_de_fora_2017_2020%>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
  filter(Projeto != is.na(Projeto))%>%
  mutate(Ementa= str_squish(Ementa))%>%
  mutate(Ementa= str_to_lower(Ementa))%>%
  #Fazendo Filtro
  mutate(tipo_de_lei = case_when(
    str_detect(Ementa, "próprio") ~ "nome de prédios",
    str_detect(Ementa, "próprios") ~ "nome de prédios",
    str_detect(Ementa, "logradou") ~ "nome de rua",
    str_detect(Ementa, "dia |calendário|semana|mês") ~ "calendario",
    #str_detect(Ementa, "\\breal\\b|\\bimóv(el)(éis)\\b|\\bbe(m)(ns)\\b") ~ "imoveis",
    str_detect(Ementa, "benemérit|honorári|honorífico") ~ "cidadão benemérito ou honorário",
    TRUE ~ "outros"))%>%
  #####contando a inImpactoLegislativo
  mutate(ImpactoLegislativo = case_when(
    tipo_de_lei%in% pl_inuteis_classe ~ "Baixo Impacto",
    tipo_de_lei == "outros" ~ "Alto Impacto",  
  ))

#conta o tipo de leis
table(pls_juiz_de_fora_2017_2020_2$ImpactoLegislativo)

#data frame interativo
DT::datatable(pls_juiz_de_fora_2017_2020_2, extensions = "Responsive")

#contando numero de processos inuteis
pl_inuteis_classe <- c("cidadão benemérito ou honorário", "calendario", "nome de rua", "nome de prédios", "imoveis")

#Qual diferença desse df?

#projetos_inuteis_df <- pls_juiz_de_fora_2017_2020_2%>%
 # mutate(ImpactoLegislativo = case_when(
  #  tipo_de_lei%in% pl_inuteis_classe ~ "Baixo Impacto",
   # tipo_de_lei == "outros" ~ "Alto Impacto",  
    #  ))

DT::datatable(projetos_inuteis_df, extensions = "Responsive")

####  INICIATIVA DO EXECUTIVO #########

pls_iniciativa_executivo_2017_2020 <- read_excel("camara_jf/pl_iniciativa_executivo_2017_2020.xlsx")

pls_iniciativa_executivo_2017_2020 <- pls_iniciativa_executivo_2017_2020%>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
  filter(Projeto != is.na(Projeto))%>%
  mutate(Autor = replace_na(Autor, "Executivo"), Tipo = "Projeto de Lei")%>%
  #fazendo os filtros
  mutate(Ementa= str_squish(Ementa))%>%
  mutate(Ementa= str_to_lower(Ementa))

### LEI COMPLEMENTAR ###

pls_lei_complementar <- read_excel("camara_jf/lei_complementar.xlsx")

pls_lei_complementar <-  pls_lei_complementar%>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
  filter(Projeto != is.na(Projeto))%>%
  #fazendo os filtros
  mutate(Ementa= str_squish(Ementa))%>%
  mutate(Ementa= str_to_lower(Ementa))

pls_lei_complementar_2017_2020 <- pls_lei_complementar%>%
  filter(Ano >= 2017)


#TODOS OS PLS DISCUTIDOS NA CÂMARA

todos_pls_2017_2020 <- full_join(pls_juiz_de_fora_2017_2020_2, pls_iniciativa_executivo_2017_2020)%>%
full_join(pls_lei_complementar_2017_2020)%>%
mutate(tipo_de_lei = case_when(
  str_detect(Ementa, "próprio") ~ "nome de prédios",
  str_detect(Ementa, "próprios") ~ "nome de prédios",
  str_detect(Ementa, "logradou") ~ "nome de rua",
  str_detect(Ementa, "dia |calendário|semana|mês") ~ "calendario",
  #str_detect(Ementa, "\\breal\\b|\\bimóv(el)(éis)\\b|\\bbe(m)(ns)\\b") ~ "imoveis",
  str_detect(Ementa, "benemérit|honorári|honorífico") ~ "cidadão benemérito ou honorário",
  TRUE ~ "outros"))%>%
  #####contando a inImpactoLegislativo
  mutate(ImpactoLegislativo = case_when(
    tipo_de_lei%in% pl_inuteis_classe ~ "Baixo Impacto",
    tipo_de_lei == "outros" ~ "Alto Impacto",  
  ))

#conta o tipo de leis
table(todos_pls_2017_2020$ImpactoLegislativo)
#### Gráficos Temas Discutidos ####


#grafico geral de tipo de lei e aprovação

ggplot(todos_pls_2017_2020, aes(x=tipo_de_lei, fill= Situação)) +geom_bar()+
  labs(title= "Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
   theme_minimal() +theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_x_discrete(name = "Tema dos Projetos", label= c("Dias Comemorativos", "Cidadãos Honorários", "Alteração Nome de Prédios", "Alteração de Nomes de Ruas", "Outros PLs em geral")) + ylab(label = "Nº de PLs") 
#+
#geom_text(aes(label = count(tipo_de_lei))) 


############################################################
################# - * PROJETOS Por VEREADOR * ######################33


#Filtrando a base por Vereador
#numero de PLs maior do que o numero real de projetos, 
#porque Projetos com Mais de um autor foram divididos em linhas diferentes


paleta<- c("#619CFF", "#00BA38", "#F8766D", "#619CFF", "red")


pls_por_vereador_split <- pls_juiz_de_fora_2017_2020_2%>%
  mutate(Autor = str_split(Autor, ","))%>%
  unnest(Autor)%>%
  mutate(Autor=str_squish(Autor))%>%
  mutate(ImpactoLegislativo = case_when(
    tipo_de_lei%in% pl_inuteis_classe ~ "Baixo Impacto",
    tipo_de_lei == "outros" ~ "Alto Impacto",  
  ))%>%
  mutate( Autor = forcats::fct_reorder(Autor, ImpactoLegislativo == "Baixo Impacto", .fun = "mean")
  )%>%
  filter(!Autor %in% c("Jucelio"))

#grafico de leis inuteis por vereador - facet_Wrap
ggplot(pls_por_vereador_split, aes(x=tipo_de_lei, fill = factor( ..count..))) +geom_bar() + 
   facet_wrap("Autor") + scale_fill_manual(paleta)

#grafico de geom col
count_vereador <- pls_por_vereador_split%>%
  group_by(Autor)%>%
  count(ImpactoLegislativo)%>%
  mutate(porcentagem = n/sum(n))

ggplot(count_vereador, aes(x=Autor, fill= n)) +geom_bar(position = "fill", width = 0.5)+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_fill(0.5), vjust=0.5)

ggplot(pls_por_vereador_split, aes(x=Autor, fill= ImpactoLegislativo, label= )) +geom_bar(position = "fill", width = 0.5)+
  #colocando label
  #geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 1) ),
   #         stat="count", position=position_fill(0.5), vjust=0.5, check_overlap=TRUE) +
   ggplot2::theme_minimal() + 
  labs(title= "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + scale_y_continuous(name = "Porcentagem entre nº Projetos", labels = c("0%","25%", "50%","75%" , "100%")) +
  scale_fill_discrete(name= "Tipo de Projeto", labels= c("Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário","Outros PLs em geral")) +
  geom_hline(yintercept = 0.5, color = "grey40", linetype = 2) 

ggplot(pls_por_vereador_split, aes(x=Autor, fill= ImpactoLegislativo)) +geom_bar(position = "dodge", width = 0.5)+
  ggplot2::theme_minimal() +
  labs(title= "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + ylab(label = "Nº Projetos") +
  scale_fill_discrete(name= "Tipo de Projeto", labels= c("Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário","Outros PLs em geral"))

  
#ggplot(pls_por_vereador_split, aes(tipo_de_lei, fill = count)) + geom_bar()  + 
 # facet_wrap(~coluna) + scale_fill_manual(paleta)

#############################3 - PLs APROVADOS

leis_2017_2020 <- pls_juiz_de_fora_2017_2020_2%>%
  filter(Situação == "Transformado em Norma Jurídica")

table(leis_2017_2020$ImpactoLegislativo)


leis_2017_2020%>%
  ggplot(aes(x=tipo_de_lei)) + geom_bar() + theme_bw() +
  labs(title= "Qual conteúdo dos Projetos de Lei aprovados pela Câmara Municipal de Juiz de Fora?",
        subtitle = "Restrito aos PLs de Autoria dos próprios vereadores, excluidos autoria do Executivo \n Legislatura 2017-2020",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")

#+ geom_text(label = table(leis_2017_2020$tipo_de_lei))

#### Grafico Leis Inúteis


leis_2017_2020%>%
  ggplot(aes(x=ImpactoLegislativo, fill= tipo_de_lei)) + geom_bar(width = 0.5) + theme_minimal() +
  labs(title= "Qual conteúdo dos Projetos de Lei aprovados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Restrito aos PLs de Autoria dos próprios vereadores, excluidos os de autoria do Executivo \nLegislatura 2017-2020",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  scale_x_discrete(name = "Temas dos Projetos", labels = c("De maior número", "Outros Projetos em Geral")) + ylab(label = "Nº de Projetos Aprovados") +
  scale_fill_discrete(name= "Tipo de Projeto", labels= c("Criação de Dias Comemorativos","Concessão de título de Cidadão honorário","Alteração de Nomes de Prédios", "Alteração de Nomes de Ruas", "Outros PLs em Geral"))



### exemplo

decisões_merito_prop <- stay_final_merito%>%
  ggplot(aes(x=tipo_parte, fill=decisão)) + geom_bar(position="fill", width = 0.5) +
  labs(x = "Natureza da Parte",y="Porcentagem de Decisões em cada Parte", fill = "Decisão do Juízo",
       title = "Proporção de Provimentos ou Improvimentos em Agravos sobre Prorrogação do Stay Period", subtitle = "Porcentagem por Posição do Agravante em Credor ou Recuperando", caption = "Fonte: TJSP") +
  scale_x_discrete(labels = c("Credor", "Empresa Recuperanda")) +
  scale_fill_discrete(labels= c("Improvido", "Parcialmente Provido", "Provido")) + theme_classic()
decisões_merito_prop

library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)



pls_juiz_de_fora_2017_2020 <- read_excel("camara_jf/pls juiz de fora2.xlsx", skip= 2)

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
    str_detect(Ementa, "\\breal\\b|\\bimóv(el)(éis)\\b|\\bbe(m)(ns)\\b") ~ "imoveis",
    str_detect(Ementa, "benemérit|honorári|honorífico") ~ "cidadão benemérito ou honorário",
    TRUE ~ "outros"))%>%
  #####contando a inutilidade
  mutate(Utilidade = case_when(
    tipo_de_lei%in% pl_inuteis_classe ~ "Com certeza Inútil",
    tipo_de_lei == "outros" ~ "Utilidade Questionável",  
  ))

#conta o tipo de leis
table(pls_juiz_de_fora_2017_2020_2$Utilidade)

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


#grafico geral de tipo de lei e aprovação

ggplot(pls_juiz_de_fora_2017_2020_2, aes(x=tipo_de_lei, fill= Situação)) +geom_bar()+
  labs(title= "Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
   theme_minimal() +theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_x_discrete(name = "Tema dos Projetos", label= c("Dias Comemorativos", "Cidadãos Honorários", "Alteração Nome de Prédios", "Alteração de Nomes de Ruas", "Outros PLs em geral")) + ylab(label = "Nº de PLs") 



#+
#geom_text(aes(label = count(tipo_de_lei))) 



################# - Por Vereador ######################33



#Filtrando a base por Vereador


paleta<- c("#619CFF", "#00BA38", "#F8766D", "#619CFF", "red")


pls_por_vereador_split <- pls_juiz_de_fora_2017_2020_2%>%
  mutate(Autor = str_split(Autor, ","))%>%
  unnest(Autor)%>%
  mutate(Autor=str_squish(Autor))%>%
  mutate(Utilidade = case_when(
    tipo_de_lei%in% pl_inuteis_classe ~ "Com certeza Inútil",
    tipo_de_lei == "outros" ~ "Utilidade Questionável",  
  ))

#grafico de leis inuteis por vereador
ggplot(pls_por_vereador_split, aes(x=tipo_de_lei, fill = factor( ..count..))) +geom_bar() + 
  scale_fill_manual(paleta) + facet_wrap("Autor")

#grafico de geom col
count_vereador <- pls_por_vereador_split%>%
  group_by(Autor)%>%
  count(Utilidade)

ggplot(pls_por_vereador_split, aes(x=Autor, fill= Utilidade)) +geom_bar(position = "fill", width = 0.5)+
   ggplot2::theme_minimal() +
  labs(title= "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + ylab(label = "Porcentagem entre nº Projetos") +
  scale_fill_discrete(name= "Tipo de Projeto", labels= c("Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário","Outros PLs em geral"))

ggplot(pls_por_vereador_split, aes(x=Autor, fill= Utilidade)) +geom_bar(position = "dodge", width = 0.5)+
  ggplot2::theme_minimal() +
  labs(title= "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Legislatura 2017-2020", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + ylab(label = "Nº Projetos") +
  scale_fill_discrete(name= "Tipo de Projeto", labels= c("Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário","Outros PLs em geral"))

  
#ggplot(pls_por_vereador_split, aes(tipo_de_lei, fill = count)) + geom_bar()  + 
 # facet_wrap(~coluna) + scale_fill_manual(paleta)

#############################3 - Pls que Viraram Lei

leis_2017_2020 <- pls_juiz_de_fora_2017_2020_2%>%
  filter(Situação == "Transformado em Norma Jurídica")

table(leis_2017_2020$Utilidade)


leis_2017_2020%>%
  ggplot(aes(x=tipo_de_lei)) + geom_bar() + theme_bw() +
  labs(title= "Qual conteúdo dos Projetos de Lei aprovados pela Câmara Municipal de Juiz de Fora?",
        subtitle = "Restrito aos PLs de Autoria dos próprios vereadores, excluidos autoria do Executivo \n Legislatura 2017-2020",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")

#+ geom_text(label = table(leis_2017_2020$tipo_de_lei))

#### Grafico Leis Inúteis


leis_2017_2020%>%
  ggplot(aes(x=Utilidade, fill= tipo_de_lei)) + geom_bar(width = 0.5) + theme_minimal() +
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

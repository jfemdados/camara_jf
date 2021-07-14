# Analisando TODOS PLs da Câmara de Juiz de Fora
# Autor: Marcello Filguieras

library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)
library(tidyr)


# Agora não precisa de Scrapper =D. Câmara fez um "Exportar Excel"



# Importing ---------------------------------------------------------------

todos_pls_vereador<- read_delim("camara_jf/pls_completos/data_raw/todos_pls_vereador.xls.csv", 
                                        ";",
                                     escape_double = FALSE,
                                     trim_ws = TRUE)
todos_pls_executivo <-  read_delim("camara_jf/pls_completos/data_raw/todos_pls_executivo.xls.csv", 
                                       ";",
                                       escape_double = FALSE,
                                       trim_ws = TRUE)

todos_pls_vereador_complementar <- read_delim("camara_jf/pls_completos/data_raw/todos_pls_vereador_complementar.xls.csv", 
                                ";",
                                escape_double = FALSE,
                                trim_ws = TRUE)

todos_pls_executivo_complementar <-  read_delim("camara_jf/pls_completos/data_raw/todos_pls_executivo_complementar.xls.csv", 
                                   ";",
                                   escape_double = FALSE,
                                   trim_ws = TRUE)



# Tidying -----------------------------------------------------------------

tidyer_camara_jf <- function(x) {
  
  x %>%
  select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação")%>%
    filter(Projeto != is.na(Projeto)) %>%
    janitor::clean_names() %>%
    mutate(ementa_alterada= str_to_lower(str_squish(ementa)),
           situacao = str_replace(situacao,"Transformado em Norma Jurídica","Aprovada"),
           tipo = str_replace(tipo,
                              "Mensagem do Executivo \\(Projeto de Lei Complementar\\)",
                              "Projeto de Lei Complementar - Executivo"),
           tipo = str_replace(tipo,
                              "Mensagem do Executivo \\(Projeto de Lei\\)",
                              "Projeto de Lei - Executivo"),)
}



df_tidys <- purrr::map_df( list(todos_pls_executivo,
                             todos_pls_vereador,
                             todos_pls_vereador_complementar,
                             todos_pls_executivo_complementar),
                        tidyer_camara_jf)


table(df_tidys $situacao)



# Classificação ------------------------------------------

# Impacto Legislativo 

#Vetor Com Classe de processos inuteis
pl_inuteis_classe <- c("Cidadão Benemérito ou Honorário",
                       "Criação de Dias Comemorativos",
                       "Nome de Rua",
                       "Nome de Prédios",
                       "imoveis")

todos_pls <- df_tidys %>%
  mutate(
    #Subgrupo de Impacto Legislativo
      tema = case_when(
          autor == "Executivo" ~ "Outros",
          str_detect(ementa_alterada, "próprio|próprios") ~ "Nome de Prédios",
          str_detect(ementa_alterada, "logradou") ~ "Nome de Rua",
          str_detect(ementa_alterada, "dia |calendário|semana|mês") ~ "Criação de Dias Comemorativos",
          #str_detect(Ementa_Alterada, "\\breal\\b|\\bimóv(el)(éis)\\b|\\bbe(m)(ns)\\b") ~ "imoveis",
          str_detect(ementa_alterada, "benemérit|honorári|honorífico") ~ "Cidadão Benemérito ou Honorário",
          TRUE ~ "Outros"),
    #Grande Grupo Impacto Legislativo
      impacto_legislativo = case_when(
          tema%in% pl_inuteis_classe ~ "Legislação Simbólica ou Irrelevante",
          tema == "Outros" ~ "Impacto Variado",  
        ))


#PLs dividudos por cada vereador

todos_pls_unnest <- todos_pls %>%
  mutate(autor = str_split(autor, ",")) %>%
  unnest(autor)%>%
  mutate(autor=str_squish(autor))

todos_pls_unnest %>%
  count(autor) %>%
  arrange(desc(n))


# Filtrando Legislatura de 2021


todos_pls_2021 <- todos_pls %>%
  filter(ano == 2021)

todos_pls_2021_por_autor <- todos_pls_unnest %>%
  filter(ano == 2021) %>%
  mutate(
    autor = forcats::fct_reorder(autor,
                                 impacto_legislativo == "Legislação Simbólica ou Irrelevante",
                                 .fun = "mean")
  )



# Indicadores - Resultados ------------------------------------------------


#Contando por tema  dos apresentados 

todos_pls_2021%>%
  filter(autor != "Executivo") %>%
  count(tema) %>%
  arrange(desc(n)) #%>% view()
  

#Contando por tema por aprovado ou reprovado 
todos_pls_2021%>%
  filter(autor != "Executivo") %>%
   group_by(tema) %>%
  count(situacao)#%>% view()




# Visualização ------------------------------------------------------------


vermelho<- "#a50c0c"
verde<- "#06a35d"
amarelo <- "#f3d40c"
azul <- "#36489e"

paleta<- c("#619CFF", "#00BA38", "#F8766D", "#619CFF", "red")


#GERAIS

###Tipos de Leis####

todos_pls_2021%>%
  ggplot(aes(x=tipo)) + geom_bar() + theme_bw() +
  labs(title= "Relatório 6 Meses da Câmara \nQual Tipos de Projetos de Lei aprovados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021/2024 - Desde o início de 2021 - Divididos Autoria Vereadores e Executivo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")


### Temas das Leis - Sem executivo

todos_pls_2021%>%
  filter(autor != "Executivo") %>%
  ggplot(aes(x=tema)) + geom_bar() + theme_bw() +
  labs(title= "Relatório 6 Meses da Câmara \nQual conteúdo dos Projetos de Lei apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021/2024 - Autoria do Legislativo, sem Executivo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")


### Temas das Leis - Com executivo
todos_pls_2021%>%
  ggplot(aes(x=tema)) + geom_bar() + theme_bw() +
  labs(title= "Relatório 6 Meses da Câmara \nQual conteúdo dos Projetos de Lei apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021/2024 -  Autoria do Executivo e Legislativo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados")

#+ geom_text(label = table(leis_2017_2020$Tema))

####Impatcto Relativo vs Legislação Simbólica ou Irrelevante ####

####  Taxa de Aprovação por Tema ---------------


#HORIZONTAL

#Gráfico geral de Tema do Pl vs Aprovação

todos_pls_2021 %>%
  ggplot( aes(x=tema, fill= situacao)) +geom_bar(width = 0.7, color= "black")+
  labs(title= "Relatório 6 Meses da Câmara - Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura de 2021 - Autoria Executivo e Legislativo", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_fill_manual(values=c(verde, azul, amarelo)) +
  scale_x_discrete(name = "Tema dos Projetos",
                   label= c("Dias Comemorativos",
                            "Cidadãos Honorários",
                            "Alteração Nome de Prédios",
                            "Alteração de Nomes de Ruas",
                            "Outros PLs em geral")) + 
  ylab(label = "Nº de PLs")

# #Gráfico geral de Tema do Pl vs Aprovação Percentual - position "fill"

todos_pls_2021 %>%
  ggplot(aes(x=tema, fill= situacao)) +
  geom_bar(width = 0.7, color= "black", position = "fill")+
  labs(title= "Relatório 6 Meses da Câmara - Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura de 2021 - Autoria Executivo e Legislativo", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme_minimal() +theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_fill_manual(values=c(verde, azul, amarelo)) +
  scale_x_discrete(name = "Tema dos Projetos",
                   label= c("Dias Comemorativos",
                            "Cidadãos Honorários",
                            "Alteração Nome de Prédios",
                            "Alteração de Nomes de Ruas",
                            "Outros PLs em geral")) + 
  ylab(label = "Nº de PLs") #+
#geom_text(aes(label = count(tema))) 


#GRÁFICO VERTICAL

# Grafico Leis Apresentados por Tema

todos_pls_2021%>%
  #filter(autor != "Executivo") %>%
  ggplot(aes(x=impacto_legislativo, fill= tema)) + geom_bar(color= "black", width = 0.5) +
  theme_minimal() +
  labs(title= "Relatório 6 Meses da Câmara\nQual conteúdo dos Projetos de Lei Apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021-2024 - Restrito aos PLs de Autoria dos próprios vereadores, excluidos os de autoria do Executivo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  scale_x_discrete(name = "", 
                   labels = c("Legislação Simbólica ou Irrelevante",
                              "Outros Projetos em Geral")) +
  ylab(label = "Nº de Projetos Apresentados") +
  scale_fill_discrete(name= "Tema de Projeto",
                      labels= c("Criação de Dias Comemorativos",
                                "Concessão de título de Cidadão honorário",
                                "Alteração de Nomes de Prédios",
                                "Alteração de Nomes de Ruas",
                                "Outros PLs em Geral"))


# Grafico Leis Aprovados por Tema

todos_pls_2021%>%
  #filter(autor != "Executivo") %>%
  filter(situacao == "Aprovada") %>%
  ggplot(aes(x=impacto_legislativo, fill= tema)) + geom_bar(color= "black", width = 0.5) +
  theme_minimal() +
  labs(title= "Relatório 6 Meses da Câmara\nQual conteúdo dos Projetos de Lei Aprovados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021-2024 - Restrito aos PLs de Autoria dos próprios vereadores, excluidos os de autoria do Executivo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  scale_x_discrete(name = "", 
                   labels = c("Legislação Simbólica ou Irrelevante",
                              "Outros Projetos em Geral")) +
  ylab(label = "Nº de Projetos Aprovados") +
  scale_fill_discrete(name= "Tema de Projeto",
                      labels= c("Criação de Dias Comemorativos",
                                "Concessão de título de Cidadão honorário",
                                "Alteração de Nomes de Prédios",
                                "Alteração de Nomes de Ruas",
                                "Outros PLs em Geral"))

####Executivo vs Legislativo####
####Executivo vs Legislativo - Por Ano####
#### Taxa de Aprovação - Exec vs Legislativo #####



### Gráficos por VEREADOR -----------------------------


#Impacto Variado vs Legislação Simbólica ou Irrelevante por Vereador

todos_pls_2021_por_autor %>%
  filter(autor != "Executivo") %>%
  ggplot(aes(x=autor, fill= impacto_legislativo)) +
  geom_bar(position = "fill", width = 0.5)




#fill
todos_pls_2021_por_autor %>%
  filter(autor != "Executivo") %>%
ggplot(aes(x=autor, fill= impacto_legislativo )) +
  geom_bar(position = "fill", width = 0.5)+
  #colocando label
  #geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 1) ),
  #         stat="count", position=position_fill(0.5), vjust=0.5, check_overlap=TRUE) +
  ggplot2::theme_minimal() + 
  labs(title= "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Relatório 6 Meses da Câmara -Legislatura 2021-2024", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") +
  scale_y_continuous(name = "Porcentagem entre nº Projetos",
                     labels = c("0%","25%", "50%","75%" , "100%")) +
  scale_fill_manual(values=c( verde, vermelho),
                    name= "Tipo de Projeto", 
                    labels= c("Outros PLs em geral",
                             "Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário")
                    ) +
  geom_hline(yintercept = 0.5, color = "black", linetype = 2) + theme(legend.position = "top") 

#, text=element_text(family="Montserrat"))+

#pl_vereador_fill<- image_read("pl_vereador_tema.png")%>%
  #image_composite(imagem, offset = "+22+1826")


# Position = Dodge

todos_pls_2021_por_autor %>%
  filter(autor != "Executivo") %>%
    ggplot( aes(x=autor, fill= impacto_legislativo)) +
    geom_bar(position = "dodge", width = 0.5)+
    ggplot2::theme_minimal() +
    labs(title= "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
         subtitle = "Relatório 6 Meses da Câmara - Legislatura 2021-2024",
         caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    xlab(label = "") + ylab(label = "Nº Projetos") +
    scale_fill_discrete(name= "Tipo de Projeto",
                        labels= c("Outros PLs em geral", "Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário")
                        )

#grafico de leis inuteis por vereador - facet_Wrap
#ggplot(todos_pls_2021_por_autor, aes(x=tema, fill = factor( ..count..))) +geom_bar() + 
 # facet_wrap("autor") #+ scale_fill_manual(paleta)






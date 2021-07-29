# Analisando TODOS PLs da Câmara de Juiz de Fora
# Autor: Marcello Filguieras

library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)

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
           situacao = str_replace(situacao,"Transformado em Norma Jurídica","Aprovado"),
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


table(df_tidys $ situacao)



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
  mutate( autor = str_split(autor, ",")) %>%
  unnest( autor)%>%
  mutate( autor=str_squish(autor))

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

pls_2021_count_tema_apresentados<- todos_pls_2021%>%
                        count(tema, impacto_legislativo) %>%
                        mutate(porcentagem = n/sum(n)) %>%
                        arrange(desc(n))

#Contando por tema por aprovado ou reprovado 
pls_2021_count_tema_aprovados<- todos_pls_2021%>%
                    filter(situacao == "Aprovado") %>%
                    count(tema, impacto_legislativo) %>%
                    mutate(porcentagem = n/sum(n))

# Análises por Vereador, não da Câmara inteira


##  Impacto Legislativo por Vereador
pls_2021_count_impacto_por_vereador <- todos_pls_2021_por_autor%>%
  group_by(autor)%>%
  count(impacto_legislativo)%>%
  mutate(porcentagem = n/sum(n))




# Exporting  normal -------------------------------------------------

library(writexl)


#### AQUI VALENTIM ESSE QUE TEM QUE IR PRO FLOURISH ##########

writexl::write_xlsx(todos_pls_2021%>%
                      filter(autor != "Executivo") %>%
                      filter(tipo == "Projeto de Lei"),
                    path= "camara_jf/pls_completos/exports/todos_pls_2021_vereadores.xlsx")


# Exporting para flourish -------------------------------------------------



#PLs Discutidos

writexl::write_xlsx(pls_2021_count_tema_apresentados %>%
                      pivot_wider(names_from = tema, values_from= c(n, porcentagem))
                    ,
                    path= "camara_jf/pls_completos/exports/pls_2021_count_temas_apresentados.xlsx")


#Pls Aprovados

writexl::write_xlsx(pls_2021_count_tema_aprovados %>%
                      pivot_wider(names_from = tema, values_from= c(n, porcentagem)),
                    path= "camara_jf/pls_completos/exports/pls_2021_count_temas_aprovados.xlsx")


# Impacto Legislativo por Vereador

writexl::write_xlsx(pls_2021_count_impacto_por_vereador %>%
                      pivot_wider(names_from = impacto_legislativo, values_from= c(n, porcentagem) ),
                    path= "camara_jf/pls_completos/exports/pls_2021_count_impacto_por_vereador.xlsx")


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
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: JF em Dados")



### Temas das Leis - Sem executivo

todos_pls_2021%>%
  filter(autor != "Executivo") %>%
  ggplot(aes(x=tema)) + geom_bar() + theme_bw() +
  labs(title= "Relatório 6 Meses da Câmara \nQual conteúdo dos Projetos de Lei apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021/2024 - Autoria do Legislativo, sem Executivo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: JF em Dados")


### Temas das Leis - Com executivo
todos_pls_2021%>%
  ggplot(aes(x=tema)) + geom_bar() + theme_bw() +
  labs(title= "Relatório 6 Meses da Câmara \nQual conteúdo dos Projetos de Lei apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021/2024 -  Autoria do Executivo e Legislativo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: JF em Dados")

#+ geom_text(label = table(leis_2017_2020$Tema))

####Impatcto Relativo vs Legislação Simbólica ou Irrelevante ####

####  Taxa de Aprovação por Tema ---------------


#HORIZONTAL

#Gráfico geral de Tema do Pl vs Aprovação

todos_pls_2021 %>%
  ggplot( aes(x=tema, fill= situacao)) +geom_bar(width = 0.7, color= "black")+
  labs(title= "Relatório 6 Meses da Câmara - Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura de 2021 - Autoria Executivo e Legislativo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
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

# Grafico Leis Apresentados por Tema - Com Executivo

todos_pls_2021%>%
  #filter(autor != "Executivo") %>%
  ggplot(aes(x=impacto_legislativo, fill= tema)) + geom_bar(color= "black", width = 0.5) +
  theme_minimal() +
  labs(title= "Relatório 6 Meses da Câmara\nQual conteúdo dos Projetos de Lei Apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021-2024 - Autoria dos Vereadores e Executivo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  scale_x_discrete(name = "", 
                   labels = c("Legislação Simbólica ou Irrelevante",
                              "Outros Projetos em Geral")) +
  scale_y_continuous(name = "Nº de Projetos Apresentados",
                     #limits = c(0,150)
                     ) +
  scale_fill_discrete(name= "Tema de Projeto",
                      labels= c("Criação de Dias Comemorativos",
                                "Concessão de título de Cidadão honorário",
                                "Alteração de Nomes de Prédios",
                                "Alteração de Nomes de Ruas",
                                "Outros PLs em Geral"))
todos_pls_2021%>%
  #filter(autor != "Executivo") %>%
  count(tema, impacto_legislativo) %>%
  mutate(porcentagem = n/sum(n))


############### AQUI VALENTIM ESSE QUE TEM QUE IR PRO FLOURISH - 

#Sem Executivo

todos_pls_2021%>%
  filter(autor != "Executivo") %>%
  filter(tipo == "Projeto de Lei") %>%
  ggplot(aes(x=impacto_legislativo, fill= tema)) + geom_bar(color= "black", width = 0.5) +
  theme_minimal() +
  labs(title= "Relatório 6 Meses da Câmara\nQual conteúdo dos Projetos de Lei Apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021-2024 - Somente PLs ordinários deAutoria dos Vereadores,  excluido os Executivos e LC") +
  scale_x_discrete(name = "", 
                   labels = c("Legislação Simbólica ou Irrelevante",
                              "Outros Projetos em Geral")) +
  scale_y_continuous(name = "Nº de Projetos Apresentados",
                     #limits = c(0,150)
                     )+
  scale_fill_discrete(name= "Tema de Projeto",
                      labels= c("Criação de Dias Comemorativos",
                                "Concessão de título de Cidadão honorário",
                                "Alteração de Nomes de Prédios",
                                "Alteração de Nomes de Ruas",
                                "Outros PLs em Geral"))

todos_pls_2021%>%
  filter(autor != "Executivo") %>%
  filter(tipo == "Projeto de Lei") %>%
  count(tema, impacto_legislativo) %>%
  mutate(porcentagem = n/sum(n))

# Grafico Leis Aprovados por Tema

todos_pls_2021%>%
  #filter(autor != "Executivo") %>%
  filter(situacao == "Aprovado") %>%
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

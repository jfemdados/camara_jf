# Camara JF- Análise 2024 - Eleições
# Autor - Marcello Filgueiras
#
#
# Base de Dados de PLs, Moções e Requerimentos


library(tidyverse)
library(tidyr)


# Importing ---------------------------------------------------------------
# Sei que esse é um jeito burro mas to com preguiça de arrumar. 

pls_vereador_21 <- readxl::read_excel("camara_jf/2024/data_raw/pls_vereador_2021.xlsx")
pls_vereador_22 <- readxl::read_excel("camara_jf/2024/data_raw/pls_vereador_2022.xlsx")
pls_vereador_23 <- readxl::read_excel("camara_jf/2024/data_raw/pls_vereador_2023.xlsx")
pls_vereador_24 <- readxl::read_excel("camara_jf/2024/data_raw/pls_vereador_2024.xlsx")
pls_executivo <- readxl::read_excel("camara_jf/2024/data_raw/pls_executivo.xlsx")
pls_organica <- readxl::read_excel("camara_jf/2024/data_raw/pls_organica.xlsx")
pls_comp <- readxl::read_excel("camara_jf/2024/data_raw/pls_comp.xlsx")

# Tidying -----------------------------------------------------------------

tidyer_camara_jf <- function(x) {
  
  x %>%
    dplyr::select("Projeto", "Ano", "Tipo", "Autor", "Ementa", "Situação") %>%
    filter(Projeto != is.na(Projeto)) %>%
    janitor::clean_names() %>%
    dplyr::mutate(ementa_alterada= str_to_lower(str_squish(ementa)),
           situacao = str_replace(situacao,"Transformado em Norma Jurídica","Aprovado"),
           tipo = str_replace(tipo,
                              "Mensagem do Executivo \\(Projeto de Lei Complementar\\)",
                              "Projeto de Lei Complementar - Executivo"),
           tipo = str_replace(tipo,
                              "Mensagem do Executivo \\(Projeto de Lei\\)",
                              "Projeto de Lei - Executivo"),
           ano = as.numeric(ano),
           projeto = as.numeric(projeto),
           autor = str_replace(autor,
                               "Luiz Otávio Fernandes Coelho - Pardal",
                               "Pardal"))
}



df_tidys <- purrr::map_df( list(pls_vereador_21,
                                pls_vereador_22,
                                pls_vereador_23,
                                pls_vereador_24,
                                pls_executivo,
                                pls_organica,
                                pls_comp),
                           tidyer_camara_jf)


table(df_tidys $ situacao)
table(df_tidys $ tipo)
table(df_tidys $ ano)


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
      tipo == "Projeto de Lei Complementar" ~ "Outros",
      str_detect(ementa_alterada, "logradou") ~ "Nome de Rua",
      str_detect(ementa_alterada, "próprio municipal|nomenclatura de be") ~ "Nome de Prédios",
      str_detect(ementa_alterada, "dia |calendário|semana|mês") ~ "Criação de Dias Comemorativos",
      #str_detect(Ementa_Alterada, "\\breal\\b|\\bimóv(el)(éis)\\b|\\bbe(m)(ns)\\b") ~ "imoveis",
      str_detect(ementa_alterada, "benemérit|honorári|honorífico") ~ "Cidadão Benemérito ou Honorário",
      TRUE ~ "Outros"),
    #Grande Grupo Impacto Legislativo
    impacto_legislativo = case_when(
      tema %in% pl_inuteis_classe ~ "Legislação Simbólica ou Irrelevante",
      tema == "Outros" ~ "Impacto Variado",  
    ))



#PLs dividudos por cada vereador

todos_pls_unnest <- todos_pls %>%
  mutate( autor = str_split(autor, ",")) %>%
  unnest( autor) %>%
  mutate( autor=str_squish(autor))


# Filtrando Legislatura de 2021


pls_21_24 <- todos_pls %>%
  filter(ano %in% c(2021,2022,2023,2024))

pls_21_24_por_autor <- todos_pls_unnest %>%
  filter(ano %in% c(2021,2022,2023,2024)) %>%
  mutate(
    autor = forcats::fct_reorder(autor,
                                 impacto_legislativo == "Legislação Simbólica ou Irrelevante",
                                 .fun = "mean")
  )


# Indicadores - Resultados ------------------------------------------------


# Pra ver e checar - Tabela Interativa

todos_pls %>%
  DT::datatable(extensions = "Responsive", filter = "top")


#Contando por tema  dos apresentados 

  pls_21_24%>%
  count(tema, impacto_legislativo) %>%
  mutate(porcentagem = n/sum(n)) %>%
  arrange(desc(n))# %>%
  #pivot_wider(names_from = tema, values_from= c(n, porcentagem)) %>%
  #writexl::write_xlsx( path= "camara_jf/2024/exports/pls_2124_count_temas_apresentados.xlsx")

#Contando por tema por aprovado ou reprovado 
  pls_21_24%>%
  filter(situacao == "Aprovado") %>%
  count(tema, impacto_legislativo) %>%
  mutate(porcentagem = n/sum(n))# %>%
  #pivot_wider(names_from = tema, values_from= c(n, porcentagem)) %>%
   #writexl::write_xlsx( path = "camara_jf/2024/exports/pls_2124_count_temas_aprovados.xlsx")
  
  

# Análises por Vereador, não da Câmara inteira


##  Impacto Legislativo por Vereador
pls_21_24_por_autor %>%
  group_by(autor) %>%
  count(impacto_legislativo) %>%
  mutate(porcentagem = n/sum(n)) %>%
  pivot_wider(names_from = impacto_legislativo, values_from= c(n, porcentagem)) %>%
  writexl::write_xlsx( path= "camara_jf/2024/exports/pls_2124_count_impacto_por_vereador.xlsx")


# Visualização ------------------------------------------------------------


vermelho<- "#a50c0c"
verde<- "#06a35d"
amarelo <- "#f3d40c"
azul <- "#36489e"

paleta<- c("#619CFF", "#00BA38", "#F8766D", "#619CFF", "red")


#GERAIS

## Tipos de Leis

pls_21_24%>%
  ggplot(aes(x=tipo)) + geom_bar() + theme_bw() +
  labs(title= "Qual Tipos de Projetos de Lei aprovados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021/2024 - Divididos Autoria Vereadores e Executivo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: JF em Dados")



### Temas das Leis - Sem executivo

pls_21_24 %>%
  filter(autor != "Executivo") %>%
  ggplot(aes(x=tema)) + geom_bar() + theme_bw() +
  labs(title= "Qual conteúdo dos Projetos de Lei apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021/2024 - Autoria do Legislativo, sem Executivo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: JF em Dados")


### Temas das Leis - Com executivo
pls_21_24 %>%
  ggplot(aes(x=tema)) + geom_bar() + theme_bw() +
  labs(title= "Qual conteúdo dos Projetos de Lei apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021/2024 - Autoria do Executivo e Legislativo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: JF em Dados")
#+ geom_text(label = table(leis_2017_2020$Tema))

## Impacto Relativo vs Legislação Simbólica ou Irrelevante

#### Taxa de Aprovação por Tema 

# HORIZONTAL

#Gráfico geral de Tema do Pl vs Aprovação

pls_21_24 %>%
  ggplot( aes(x=tema, fill= situacao)) +geom_bar(width = 0.7, color= "black")+
  labs(title= "Relatório 6 Meses da Câmara - Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura de 2021 - Autoria Executivo e Legislativo",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_fill_manual(values=c(verde, azul, amarelo, vermelho)) +
  scale_x_discrete(name = "Tema dos Projetos",
                   label= c("Dias Comemorativos",
                            "Cidadãos Honorários",
                            "Alteração Nome de Prédios",
                            "Alteração de Nomes de Ruas",
                            "Outros PLs em geral")) + 
  ylab(label = "Nº de PLs")

# Gráfico geral de Tema do Pl vs Aprovação Percentual - position "fill"

pls_21_24 %>%
  ggplot(aes(x=tema, fill= situacao)) +
  geom_bar(width = 0.7, color= "black", position = "fill")+
  labs(title= "Dos PLs apresentados por tema, quais a câmara mais rejeita?",
       subtitle = "Legislatura de 2021 - Autoria Executivo e Legislativo", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme_minimal() +theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_fill_manual(values=c(verde, azul, amarelo, vermelho)) +
  scale_x_discrete(name = "Tema dos Projetos",
                   label= c("Dias Comemorativos",
                            "Cidadãos Honorários",
                            "Alteração Nome de Prédios",
                            "Alteração de Nomes de Ruas",
                            "Outros PLs em geral")) #+ 
#  ylab(label = "Nº de PLs") #+
#geom_text(aes(label = count(tema))) 

#GRÁFICO VERTICAL

# Grafico Leis Apresentados por Tema - Com Executivo

pls_21_24 %>%
  filter(autor != "Executivo") %>%
  ggplot(aes(x=impacto_legislativo, fill= tema)) + geom_bar(color= "black", width = 0.5) +
  theme_minimal() +
  labs(title= "Qual conteúdo dos Projetos de Lei Apresentados na Câmara de Juiz de Fora?",
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
pls_21_24 %>%
  #filter(autor != "Executivo") %>%
  count(tema, impacto_legislativo) %>%
  mutate(porcentagem = n/sum(n))


## AQUI VALENTIM ESSE QUE TEM QUE IR PRO FLOURISH  

#Sem Executivo

pls_21_24 %>%
  filter(autor != "Executivo") %>%
  #filter(tipo == "Projeto de Lei") %>%
  ggplot(aes(x=impacto_legislativo, fill= tema)) + geom_bar(color= "black", width = 0.5) +
  theme_minimal() +
  labs(title= "Qual conteúdo dos Projetos de Lei Apresentados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021-2024 - Autoria de Vereadores, sem Executivo") +
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

pls_21_24%>%
  filter(autor != "Executivo") %>%
  filter(tipo == "Projeto de Lei") %>%
  count(tema, impacto_legislativo) %>%
  mutate(porcentagem = n/sum(n))

# Grafico Leis Aprovados por Tema

pls_21_24 %>%
  #filter(autor != "Executivo") %>%
  filter(situacao == "Aprovado") %>%
  ggplot(aes(x=impacto_legislativo, fill= tema)) + geom_bar(color= "black", width = 0.5) +
  theme_minimal() +
  labs(title= "Qual conteúdo dos Projetos de Lei Aprovados pela Câmara Municipal de Juiz de Fora?",
       subtitle = "Legislatura 2021-2024 - Autoria de Vereadores, sem Executivo",
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



### Gráficos por VEREADOR 


# Impacto Variado vs Legislação Simbólica ou Irrelevante por Vereador

pls_21_24_por_autor %>%
  filter(autor != "Executivo") %>%
  ggplot(aes(x=autor, fill= impacto_legislativo)) +
  geom_bar(position = "fill", width = 0.5)




# Fill
pls_21_24_por_autor %>%
  filter(autor != "Executivo") %>%
  ggplot(aes(x=autor, fill= impacto_legislativo )) +
  geom_bar(position = "fill",
           width = 0.5) +
  #colocando label
  #geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 1) ),
  #         stat="count", position=position_fill(0.5), vjust=0.5, check_overlap=TRUE) +
  ggplot2::theme_minimal() + 
  labs(title = "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Relatório 6 Meses da Câmara - Legislatura 2021-2024", caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
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

pls_21_24_por_autor %>%
  filter(autor != "Executivo") %>%
  ggplot( aes(x = autor, fill = impacto_legislativo)) +
  geom_bar(position = "dodge", width = 0.5)+
  ggplot2::theme_minimal() +
  labs(title = "Qual o conteúdo dos Projetos de Lei Apresentados por Cada Vereador em Juiz de Fora?",
       subtitle = "Legislatura 2021-2024",
       caption = "Fonte: Site Oficial Câmara Municipal\n Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + ylab(label = "Nº Projetos") +
  scale_fill_discrete(name = "Tipo de Projeto",
                      labels = c("Outros PLs em geral",
                                 "Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário")
                        )

#grafico de leis inuteis por vereador - facet_Wrap
#ggplot(pls_21_24_por_autor, aes(x=tema, fill = factor( ..count..))) +geom_bar() + 
# facet_wrap("autor") #+ scale_fill_manual(paleta)



# Exporting   -------------------------------------------------

library(writexl)



 writexl::write_xlsx(pls_21_24,
                   path = "camara_jf/2024/exports/pls_21_24_classificada.xlsx")







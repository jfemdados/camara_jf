# Analisando TODOS PLs da Câmara de Juiz de Fora
# Autor: Marcello Filguieras

library(tidyverse)

# Agora não precisa de Scrapper =D. Câmara fez um "Exportar Excel"



# Importing ---------------------------------------------------------------

library(readxl)
mocoes_24 <- read_excel("camara_jf/2024/data_raw/mocoes/mocoes_24.xlsx")




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
           situacao = str_replace(situacao, situacao_regex, "Arquivada" ),
           ano= str_replace(ano, "201\\b", "2010") %>%
             as.numeric()
           )
}


mocoes_tidy <- mocoes_24 %>%
  tidyer_camara_jf_mocoes()


# Tidy vs raw
table(mocoes_tidy $ situacao)
table(mocoes_24 $ Situação)

table(mocoes_tidy $ ano)
table(mocoes_24 $ Ano)


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
      grande_tema == "Moção de Repúdio" ~ "Moções Políticas",
      str_detect(ementa_alterada, "falecimento|morte do|post mortem|pesar") ~ "Falecimento",
      str_detect(ementa_alterada, "anima(l|is)|veteriná") ~ "Moções à Causa Animal",
      str_detect(ementa_alterada, "\\bmiss\\b|\\bmister\\b|musa") ~ "Moções a Miss e Mister",
      str_detect(ementa_alterada, "penintenc|ariosvaldo") ~ "Moções a Policiais e Militares",
      str_detect(ementa_alterada, "esport|atlet|vôlei|basquete|futebol|bom pastor|malha clube|natação|jiu") ~ "Moções a Esportistas",
      str_detect(ementa_alterada, "lanch|restaurante|pizza|cachorro|hamb|cervej|caf[ée]|\\bbar\\b|linguiçaria|churras|sushi") ~ "Moções a Restaurantes",
      str_detect(ementa_alterada, "samba") ~ "Moções a Líderes Culturais",
      str_detect(ementa_alterada, "escola|col[eé]gio|professor|alun[oa]|faculdade|universidade") ~ "Moções a Escolas, Prof. e Alunos",
      str_detect(ementa_alterada, "evangél|relig|\\bfé\\b|orixá|padre|bispo|pastor|igreja|senhor da terra|paróquia|diocese") ~ "Moções a Autoridades Religiosas",
      str_detect(ementa_alterada, "capitão|brigada|bombe|militar|guarda(s) municipais|delega|pol[ií]cia|sgt|sargento|\\bcabo\\b|\\bcb\\b|batalhão|\\bpm|soldado|coronel|tenente") ~ "Moções a Policiais e Militares",
      str_detect(ementa_alterada, "resol|sind|classe|govern|\\blei\\b|projeto de lei|\\bpl\\b|\\blula\\b|\\bdilma\\b|movimento negro|sind") ~ "Moções Políticas",
      str_detect(ementa_alterada, "hospita|clín|odont|covid|câncer|saúde|m[ée]dic|ubs|regional leste|hps|leite humano|ascomcer|santa casa|albert sabin") ~ "Moções à Médicos e Agentes de Saúde",
      str_detect(ementa_alterada, "emprego|indústria|com[ée]rc|agro|leite|empres[áa]|materia[li]|arquitet|escritório|laboratório|academia|piscinas|barbe|cabele|da beleza|mercado|borra|oficina") ~ "Moções à Empresas",
            str_detect(ementa_alterada, "servidor|trabalhador") ~ "Moções a Servidores",
      str_detect(ementa_alterada, "cultura|capoeira|samba|coral|m[úu]sica|poe[ts]|berimbau|banda|fest[ai]|hip") ~ "Moções a Líderes Culturais",
      str_detect(ementa_alterada, "associa|socia[li]|comunitá") ~ "Moções a Associações e Projetos Sociais",
      #str_detect(ementa_alterada, "projeto") ~ "Moções a Líderes Culturais",
            TRUE ~ "Outros"
      ))


mocoes_classificado %>%
  count(tema_especifico) %>%
  arrange(desc(n))

mocoes_classificado %>%
  count(ano) %>%
  arrange(desc(ano))

mocoes_classificado %>%
  DT::datatable(extensions = "Responsive",
                filter = "top")

#Moções dividas por cada vereador

mocoes_unnest <- mocoes_classificado %>%
  mutate(autor = str_split(autor, ",")) %>%
  unnest(autor) %>%
  mutate(autor = str_squish(autor))

mocoes_unnest %>%
  count(autor) %>%
  arrange(desc(n))


# Filtrando Legislatura de 221-24 ------



mocoes_2124 <- mocoes_classificado %>%
  filter(ano >= 2021)

mocoes_2124_por_autor <- mocoes_unnest %>%
  filter(ano >= 2021)



#%>%
 # mutate(
  #  autor = forcats::fct_reorder(autor,
   #                              impacto_legislativo == "Legislação Simbólica ou Irrelevante",
    #                             .fun = "mean")
  #)



# Indicadores - Resultados ------------------------------------------------


#Contando por tema  dos apresentados 

mocoes_2124 %>%
  count(tema_especifico) %>%
  arrange(desc(n)) #%>% view()


#Contando p or tema por aprovado ou reprovado 
mocoes_2124%>%
  count(situacao)#%>% view()


#Moções por grande tema Vereador
mocoes_2124_g_tema_autor <- mocoes_2124_por_autor %>%
  group_by(grande_tema) %>%
  count(autor) %>%
  pivot_wider(names_from = grande_tema, values_from = n)


# Exporting ---------------------------------------------------------------



#writexl::write_xlsx(mocoes_2124_por_autor %>%
#                      count(autor) %>%
#                      arrange(desc(n)),
#                 path= "camara_jf/2024/exports/mocoes_por_autor_2124.xlsx")


#writexl::write_xlsx(mocoes_2124_g_tema_autor,
 #                   path= "camara_jf/2024/exports/mocoes_2124_g_tema_autor.xlsx")


#writexl::write_xlsx(mocoes_2124 %>%
 #                     count(tema_especifico) %>%
  #                    arrange(desc(n)),
   #             path= "camara_jf/2024/exports/mocoes_2124_temas.xlsx")


#writexl::write_xlsx(mocoes_2124 %>% count(ano),
 #                   path= "camara_jf/2024/exports/mocoes_2124_ano.xlsx")


# Visualização ----------------------------------------------------------------


# Por Vereador ------------------------------------------------------------
library(RColorBrewer)

mocoes_2124_por_autor%>%
ggplot(aes(y=autor, fill= tema_especifico )) +
  geom_bar( width = 0.5, color="black", position = "fill") +
  ggplot2::theme_minimal() + 
  labs(title= "Qual tema das Moções que seu Vereador apresentou?",
       subtitle = "Relatório Câmara JF - Legislatura 2021-2024",
       caption = "Fonte: Site Oficial Câmara Municipal - Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "Percentual de Moções") + ylab(label = "") +
  scale_fill_brewer(palette = "Paired",
                    name = "Tema Específico")
  #scale_y_continuous(name = "Porcentagem entre nº Projetos",
   #                  labels = c("0%","25%", "50%","75%" , "100%")) +
  #scale_fill_manual(values=c( verde, vermelho),
   #                 name= "Tipo de Projeto", 
    #                labels= c("Outros PLs em geral",
     #                         "Criação de Dias Comemorativos,\nAlteração de Nomes de Ruas/Prédios e \nConcessão de título de Cidadão honorário")
  #geom_hline(yintercept = 0.5, color = "black", linetype = 2) 


mocoes_2124_por_autor%>%
  ggplot(aes(x=autor)) +
  geom_bar( width = 0.5, fill = "#ac050c", color= "black") +
  ggplot2::theme_minimal() + 
  labs(title= "Quantas Moções seu Vereador apresentou?",
       subtitle = "Relatório Câmara JF - Legislatura 2021-2024",
       caption = "Fonte: Site Oficial Câmara Municipal - Elaboração e Classificação: Projeto JF em Dados") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  xlab(label = "") + ylab(label = "Número de Moções")

# Geral  --------------------------------------------------------------

# Tema
mocoes_2124 %>%
  ggplot( aes(y=tema_especifico)) +
  geom_bar(width = 0.7, fill = "#ac050c", color= "black")+
  labs(title= "4 anos de Câmara Municipal - Temas da Moções",
       subtitle = "Legislatura de 2021-2024",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 50, hjust = 1))

# ano
mocoes_2124 %>%
  ggplot( aes(y=ano)) +
  geom_bar(width = 0.7, fill = "#ac050c", color= "black")+
  labs(title= "4 anos deCâmara Municipal - Temas da Moções",
       subtitle = "Legislatura de 2021-2024",
       caption = "Fonte: Site Oficial Câmara Municipal \n Elaboração e Classificação: Projeto JF em Dados") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 50, hjust = 1))

# Export ------------------------------------------------------------------
#
##CSV
#rio::export(mocoes_classificado, file= "camara_jf/mocoes/exports/mocoes_classificado.csv")
#rio::export(mocoes_2021, file= "camara_jf/mocoes/exports/mocoes_classificado_2021.csv")
#
#write_csv(mocoes_2021, file= "camara_jf/mocoes/exports/mocoes_classificado_2021_2.csv")
#
##XLSX
#
#writexl::write_xlsx(mocoes_classificado, path= "camara_jf/mocoes/exports/mocoes_classificado.xlsx")
#writexl::write_xlsx(mocoes_2021, path = "camara_jf/mocoes/exports/mocoes_classificado_2021.xlsx")
#

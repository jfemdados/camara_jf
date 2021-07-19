# Camara JF- Requerimentos
# Autor - Marcello Filgueiras

library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)


# Agora não precisa de Scrapper =D. Câmara fez um "Exportar Excel"



# Importing ---------------------------------------------------------------

requerimentos_raw <- read_delim("camara_jf/requerimentos/data_raw/requerimentos.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)


# Tidying -----------------------------------------------------------------


tidyer_camara_jf_requerimento <- function(x) {
  
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
           situacao = str_replace(situacao, situacao_regex, "Arquivada" ),
           situacao = str_replace(situacao, "Aprovado", "Aprovada"),
           situacao = str_remove (situacao, " com Encaminhamento para Executivo"))
}


requerimentos_tidy <- requerimentos_raw %>%
  tidyer_camara_jf_requerimento()



table(requerimentos_tidy $ ano)

table(requerimentos_tidy $ situacao)




# Classificação -----------------------------------------------------------------

#por tema

requerimentos_classificado <- requerimentos_tidy %>%
  mutate(tema= case_when(
    str_detect(ementa_alterada, "capina|\\bro[cç]a|\\bpod[ae](s)*\\b|[áa]rvore") ~ "Capina e Podas de Árvores",
    str_detect(ementa_alterada, "buraco|esc[oó]ria|asf[áa]lt|tampa|cascalh|patrol") ~ "Tapar Buracos e Asfaltamento",
    str_detect(ementa_alterada, "m[ée]dico|\\bubs\\b|sa[úu]de|vacina|covid") ~  "Saúde e Vacinação",
    str_detect(ementa_alterada, "ilumina[cç][aã]o|poste|l[âa]mpada|\\bled\\b") ~ "Troca de Lâmpadas em Postes",
    str_detect(ementa_alterada, "\\blinha\\b|[ôo]nibus") ~ "Ônibus e Transporte Público",
    str_detect(ementa_alterada, "transito|trânsito|mão dupla|mão única|radar|quebra[- ]molas|pedestre|redutor(es)* de velocidade|sinal|placa|rotat[óo]ria") ~ "Trânsito",
    str_detect(ementa_alterada, "esgoto|\\b[aá]gua|canal|manilha") ~ "Saneamento Básico",
    str_detect(ementa_alterada, "limpeza|entulho|boca de lobo") ~ "Limpeza de Vias Públicas",
    str_detect(ementa_alterada, "pavimenta[cç][aã]o") ~ "Tapar Buracos e Asfaltamento",
    TRUE ~  "Outros"))



requerimentos_classificado %>%
  filter(ano==2021) %>%
  count(tema) %>%
  arrange(desc(n))


requerimentos_classificado %>%
  filter(ano==2021) %>%
  select(!ementa) %>%
  #view()
  #DT::datatable(extensions= "Responsive")

# Por Autor

requerimentos_autor <- requerimentos_tidy %>%
  mutate(autor = str_split(autor, ",")) %>%
  unnest(autor) %>%
  mutate(autor=str_squish(autor))


# Filtro de 2021


requerimentos_2021 <- requerimentos_tidy %>%
  filter(ano == 2021)


requerimentos_autor_2021 <- requerimentos_autor %>%
  filter(ano == 2021)

req_autor_2021_count <-requerimentos_autor_2021  %>%
  count(autor, name = "n_requerimentos") %>%
  arrange(desc(n_requerimentos)) %>%
  mutate(
   autor = forcats::fct_reorder(autor,
                                n_requerimentos))



#Requerimento Discutido


#requerimentos_count <- requerimentos_2021 %>%
 # count(ImpactoLegislativo, Tema) %>%
#  mutate(porcentagem = n/sum(n)) %>%
 # pivot_wider(names_from = Tema, values_from= c(n, porcentagem))


# Requerimentos Aprovados
#requerimentos_count_aprovados <- requerimentos_2021 %>%
#  filter(Situação == "Aprovado") %>%
#  count(ImpactoLegislativo, Tema) %>%
#  mutate(porcentagem = n/sum(n)) %>%
#  pivot_wider(names_from = Tema, values_from= c(n, porcentagem))



# Visualização ------------------------------------------------------------

library(ggplot2)

vermelho<- "#a50c0c"

req_autor_2021_count %>%
  ggplot(aes(y=autor, x= n_requerimentos)) +
  geom_col( fill= vermelho, color = "gray20") +
  theme_light() +
  labs(title= "Numero de Requerimentos por Vereador",
       subtitle= "Na função de fiscalização da Prefeitura, requerimentos são pedidos dos Vereadores à Prefeitura\npara tomar providências",
       caption = " Fonte: Câmara JF - Elaboração: JF em Dados")


#Por tema

requerimentos_classificado %>%
  filter(ano==2021) %>%
  mutate(autor = str_split(autor, ",")) %>%
  unnest(autor) %>%
  mutate(autor=str_squish(autor))%>%
  group_by(tema) %>%
  count(autor) %>%
  mutate(autor = forcats::fct_reorder(autor,n)) %>%
  ggplot(aes(y=autor, x= n, fill= tema)) +
  geom_col( position = "fill", color = "gray20") +
  theme_light() +
  labs(title= "Numero de Requerimentos por Vereador",
       subtitle= "Na função de fiscalização da Prefeitura, requerimentos são pedidos dos Vereadores à Prefeitura\npara tomar providências",
       caption = " Fonte: Câmara JF - Elaboração: JF em Dados")


# Exporting para Flourish -------------------------------------------------


rio::export( req_autor_2021_count #%>%  pivot_wider(names_from = autor, values_from= n_requerimentos)
            ,file= "camara_jf/requerimentos/exports/requerimentos_por_autor.csv" )



# Mapa de JF e Lista de Bairros -----------------------------------------------------------

library(tidyverse)
library(geobr)
library(sf)


##mapa geobr

#juiz_de_fora <- geobr::read_municipality(code_muni = 3136702)

#juiz_de_fora_zonas <- geobr::read_census_tract(code_tract  = 3136702,
#                                               year = 2000 )

#juiz_de_fora_zonas_urbanas <- geobr::read_census_tract(code_tract  = 3136702,
#                                                      year = 2000,
#                                                     zone = "urban" )

#juiz_de_fora_zonas_rurais <- geobr::read_census_tract(code_tract  = 3136702,
#                                                  year = 2000,
#                                                   zone = "rural" )

#ggplot() + geom_sf(data= juiz_de_fora_zonas_urbanas,
#                 fill= "steelblue4",
#                color= "#FEBF57",
#               size= .15,
#              show.legend = F)

#grupamento de bairros IBGE

library(readxl)
library(ggplot2)

#base_ibge_<- read_excel("base_ibge_setor_censitario_2010_mg.xls")

#saveRDS(Base_informacoes_setores2010_sinopse_MG, file = "base_ibge.rds")

base_ibge_raw <- readRDS("camara_jf/requerimentos/data_raw/base_ibge.rds")

base_ibge_select<- base_ibge_raw %>%
  select(Cod_setor, Cod_municipio:Nome_do_bairro)

base_ibge_jf<- base_ibge_select%>%
  filter(Nome_do_municipio == "JUIZ DE FORA") %>%
  rename(code_tract = Cod_setor)

base_ibge_jf_bairro <- base_ibge_jf %>%
  janitor::clean_names() %>%
  group_by( nome_do_municipio, nome_do_subdistrito, nome_do_bairro) %>%
  count(nome_do_bairro) %>%
  mutate(nome_do_bairro = paste(
    "\\b",
    nome_do_bairro,
    "\\b",
    sep = ""))


regex_bairros <- base_ibge_jf_bairro %>%
  pull(nome_do_bairro )

requerimentos_tidy %>%
  mutate()




#juntando geobr X IBGE com inner_join - DÁ TRUEERRADO 

#juiz_de_fora_agregado_inner <- inner_join(juiz_de_fora_zonas, base_ibge_jf, by= "code_tract")


#Cruzando geobr e ibge com full_join

juiz_de_fora_agregado_full <- full_join(juiz_de_fora_zonas, base_ibge_jf, by= "code_tract")


# agrupando por nome do bairro
# st_simplify() deixa o mapa menos detalhado para aumentar em número mais bonito

juiz_de_fora_bairros_full<- juiz_de_fora_agregado_full%>%
  group_by(Nome_do_bairro)%>%
  summarize()%>%
  st_simplify(dTolerance = 0.0007)

# Visualizando

ggplot() + geom_sf(data= juiz_de_fora_bairros_full,
                   fill= "steelblue4",
                   color= "#FEBF57",
                   size= .15,
                   show.legend = F)

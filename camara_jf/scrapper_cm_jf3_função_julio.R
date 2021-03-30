#' Author: Julio Trecenti
#' Subject: Webscraping Camara Municipal pedido no Discourse

 library(tidyverse)
library(magrittr)

# Import -----------------------------------------------------------------------

mocoes_aplauso_site = httr::GET(url= "http://www.camarajf.mg.gov.br/sal/listapropos.php?orig=&numero=&ano=&autor=&ementa=&palavra=&codtipo=MOC&btnprocurar=Procurar")

quantas_paginas <- function(r) {
  r %>% 
    xml2::read_html() %>% 
    xml2::xml_find_first("//td[contains(text(), 'Foram encontradas')]") %>% 
    xml2::xml_text() %>% 
    readr::parse_number() %>% 
    magrittr::divide_by_int(15) %>% 
    magrittr::add(1)
}

quantas_paginas(mocoes_aplauso_site)

baixar_pagina <- function(pg, path) {
  fs::dir_create(path)
  f <- sprintf("%s/%04d.html", path, pg)
  if (!file.exists(f)) {
    u <- "http://www.camarajf.mg.gov.br/sal/listapropos.php"
    query <- list(
      "pg" = pg - 1,
      "orig" = "",
      "numero" = "",
      "ano" = "",
      "autor" = "",
      "ementa" = "",
      "palavra" = "",
      "codtipo" = "MOC",
      "btnprocurar" = "Procurar"
    )
    r <- httr::GET(
      u, query = query,
      httr::write_disk(f, TRUE)
    )
  }
  f
}

baixar_pagina(pg= 466, path= "camara_jf/mocoes")

primeira_pagina <- baixar_pagina(1, "ws_discourse")
paginas <- quantas_paginas(primeira_pagina)
purrr::walk(seq_len(paginas), baixar_pagina, "ws_discourse")

parse_pagina <- function(html_file) {
  html_file %>% 
    xml2::read_html() %>% 
    xml2::xml_find_first("//table[@width='970']") %>% 
    rvest::html_table(fill = TRUE) %>% 
    tibble::as_tibble() %>%
    dplyr::select(dplyr::all_of(0:4*2 + 1)) %>% 
    purrr::set_names(as.character(.[3,])) %>% 
    janitor::clean_names() %>% 
    dplyr::slice(-c(1:3)) %>% 
    dplyr::filter(
      !projeto %in% c(""),
      !stringr::str_detect(projeto, "Proximo|Anterior")
    )
}

da_camara <- fs::dir_ls("ws_discourse") %>% 
  purrr::map_dfr(parse_pagina, .id = "file")

mocoes_aplauso<- da_camara%>%
  mutate(autor = str_split(autor, ","))%>%
  unnest(autor)%>%
  mutate(autor=str_squish(autor))%>%
  count(autor)



# Tidy -------------------------------------------------------------------------

# Visualize --------------------------------------------------------------------

# Model ------------------------------------------------------------------------

# Export -----------------------------------------------------------------------

readr::write_rds(da_camara, "da_camara.rds")

mocoes_aplauso<- da_camara%>%
  filter(ano>= 2017)%>%
  mutate(autor = str_split(autor, ","))%>%
  unnest(autor)%>%
  mutate(autor=str_squish(autor))%>%
  count(autor)

# Export -----------------------------------------------------------------------

readr::write_rds(mocoes_aplauso, "da_mocoes_aplauso.rds")
rio::export(mocoes_aplauso, "mocoes_aplauso.csv")

library(tidyverse)

da_camara%>%
  ggplot(aes(x= ano)) + geom_histogram(stat= "count") +labs(title = "Nº de Moções de Aplauso por Ano na Câmara JF")

library(tidyverse)
library(rvest)

leis_jf_2020 <- read_html("leis_jf_2020.txt")

leis_jf_2020_nodes<- leis_jf_2020%>%
  html_node('table')%>%
  html_table()

leis_jf_2020_df <- leis_jf_2020_nodes[3:135,]%>%
  mutate( X2= str_squish(X2))

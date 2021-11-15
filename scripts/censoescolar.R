#biblio
library(tidyverse)
library(basedosdados)

#query

basedosdados::set_billing_id("double-voice-305816")


racacor <- read_sql("SELECT ano, raca_cor, rede, COUNT(id_matricula)
FROM `basedosdados.br_inep_censo_escolar.matricula` 
WHERE id_municipio = '3136702' and ano in (2020, 2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009)
GROUP BY raca_cor, rede, ano") 

racacor_priv <- racacor %>%
  rename( alunos = f0_) %>% 
  filter(rede == "privada") %>%
  mutate(raca_cor = case_when(raca_cor == "0" ~ "nao_declarada",
                              raca_cor == "1" ~ "branca",
                              raca_cor == "2" ~ "preta",
                              raca_cor == "3" ~ "parda",
                              raca_cor == "4" ~ "amarela",
                              raca_cor == "5" ~ "indigena")) %>% 
  pivot_wider(id_cols = c("ano", "raca_cor"),
              values_from = "alunos",
              names_from = "raca_cor") %>% 
  mutate(ppi = preta+parda+indigena) %>% 
  arrange(ano)

rio::export(racacor_priv, "racacor_priv.csv") 

#Libraries ------------------------------------------------
options(scipen = 999)
library(tidyverse)
library(rio)
library(forcats)

# Setting up bd -------------------------------
basedosdados::set_billing_id("double-voice-305816")

#Abrindo dados dos docentes------------------------------
bd_table <- basedosdados::bdplyr(
  "basedosdados.br_inep_censo_escolar.docente")

docentes <- bd_table %>% 
  select(id_docente, id_municipio, ano, rede, raca_cor, id_escola, id_turma) %>% 
  filter(id_municipio == "3136702", ano == 2020) 

 bd_docentes<- docentes %>% 
  basedosdados::bd_collect() %>% 
   distinct(id_docente,id_escola, .keep_all = TRUE)



# Tratando os dados de docentes------------------------------------
  
 #Base de docentes agregados por raca e por rede
bd_docentes_ag<-bd_docentes %>% 
   mutate(rede = forcats::fct_recode(bd_docentes$rede, publica = "federal",
                                       publica = "estadual",publica = "municipal"),
          raca_cor = case_when(raca_cor == "0" ~ "nao_declarada",
                            raca_cor == "1" ~ "branca",
                            raca_cor == "2" ~ "preta",
                            raca_cor == "3" ~ "parda",
                            raca_cor == "4" ~ "amarela",
                            raca_cor == "5" ~ "indigena")) %>% 
   #group_by(rede) %>%
   count(raca_cor) %>% 
   mutate(total_docentes = sum(n))
 
 #Base especifica para pretos pardos e brancos - foi usada para um gráfico!
 bd_docentes_pp <- bd_docentes_ag %>% 
   filter(raca_cor %in% c("branca", "preta", "parda", "nao_declarada")) %>% 
   mutate(perc_pp = n/total_docentes)
 

 #Base para checar em quantas escolas trabalham os professores-------------------
 bd_docentes_nesc <- bd_docentes %>% 
   group_by(id_docente) %>% 
   count()

 bd_docentes_nesc_raca <- bd_docentes %>% 
   distinct(id_docente, .keep_all = T) %>% 
   inner_join(bd_docentes_nesc, by = 'id_docente') %>% 
   mutate(raca_cor = case_when(raca_cor == "0" ~ "nao_declarada",
                                          raca_cor == "1" ~ "branca",
                                          raca_cor == "2" ~ "preta",
                                          raca_cor == "3" ~ "parda",
                                          raca_cor == "4" ~ "amarela",
                                          raca_cor == "5" ~ "indigena"))
 
 #Base para todos os professores
 bd_docentes_nesc_todos <- bd_docentes_nesc_raca %>% 
   mutate(nesc = case_when(n == 1 ~ "uma",
                           n > 1 & n < 4 ~ "duas a três",
                           n >= 4 ~ "quatro ou mais")) %>% 
   group_by(nesc) %>% 
   count()
 

 
 
 bd_docentes_nesc_pp <- bd_docentes_nesc_raca %>% 
   filter(raca_cor %in% c('preta','parda')) %>% 
   mutate(nesc = case_when(n == 1 ~ "uma",
                           n > 1 & n < 4 ~ "duas a três",
                           n >= 4 ~ "quatro ou mais")) %>% 
   group_by(nesc) %>% 
   count()
 
 bd_docentes_nesc_branca <- bd_docentes_nesc_raca %>% 
   filter(raca_cor %in% c('branca')) %>% 
   mutate(nesc = case_when(n == 1 ~ "uma",
                           n > 1 & n < 4 ~ "duas a três",
                           n >= 4 ~ "quatro ou mais")) %>% 
   group_by(nesc) %>% 
   count()
 






#Script que realmente usa as bases
library(tidyverse)

matriculasjf<- rio::import("cadamatriculajf.csv")

matriculasjf_priv_ag <- matriculasjf %>% 
  mutate(raca_cor = case_when(raca_cor == "0" ~ "nao_declarada",
                              raca_cor == "1" ~ "branca",
                              raca_cor == "2" ~ "preta",
                              raca_cor == "3" ~ "parda",
                              raca_cor == "4" ~ "amarela",
                              raca_cor == "5" ~ "indigena")) %>% 
  filter(rede != 'privada') %>% 
  group_by(id_escola) %>% 
  count(raca_cor) %>% 
  pivot_wider(id_cols = "id_escola",
              values_from = "n",
              names_from = "raca_cor") %>% 
  mutate(pp = preta+parda)%>%
  #assumindo que NA significa nao ter aluno - hipotese nao tao forte assim
  replace(is.na(.), 0) %>% 
  mutate(total_alunos = branca+nao_declarada+preta+parda+amarela+indigena,
         perc_pp = ((preta+parda)/total_alunos)*100)

#descobrindo os quartis
summary(matriculasjf_priv_ag$perc_pp)


matriculasjf_priv_ag_quartis <- matriculasjf_priv_ag %>% 
  filter(perc_pp>= 63.407 | perc_pp <= 43.972) %>% 
  mutate(grupo = case_when(perc_pp >= 63.407 ~ "alto",
                           perc_pp <= 43.972 ~ "baixo"),
         id_escola = as.character(id_escola))



# Avaliacao das escolas ------------------

aval_escolas<- rio::import("censoescolarjf/avaliacao-escolas-jf.csv") %>% 
  filter(id_escola %in% matriculasjf_priv_ag_quartis$id_escola) %>% 
  group_by(id_escola)

tdi <- aval_escolas%>% 
  summarise(tdi_em = mean(tdi_em, na.rm = TRUE))

atu <- aval_escolas %>% 
  summarise(atu_em = mean (atu_em, na.rm = T))

abandono <- aval_escolas %>% 
  summarise(abandono = mean(taxa_abandono_em, na.rm = TRUE))



# Infraestrutura das escolas ----------
bd_escolasjf2 <- bd_escolasjf %>% 
  filter(id_escola %in%  matriculasjf_priv_ag_quartis$id_escola) 

#juntando dados
final_biblio <- bd_escolasjf2 %>% 
  inner_join(matriculasjf_priv_ag_quartis, by = 'id_escola')


#diferença de grupos
final_biblio %>% 
  group_by(grupo) %>% 
  summarise(bibpaluno = (sum(biblioteca)/sum(total_alunos)),
            labinfopaluno = (sum(laboratorio_informatica)/sum(total_alunos)),
            labciepaluno = (sum(laboratorio_ciencias)/sum(total_alunos)))


####exportando
rio::export(final_biblio, "redepublica-piores-melhores-quartis.csv")

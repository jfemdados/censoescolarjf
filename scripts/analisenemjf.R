#

basedosdados::set_billing_id("double-voice-305816")


# Olhando para se as escolas são públicas ou privadas
bd_enemjf<- basedosdados::read_sql("SELECT * FROM `basedosdados-dev.br_inep_enem.escola` 
                                   WHERE id_municipio = '3136702' and ano = 2015")

#Consertando a rede
bd_enemjf2 <- bd_enemjf %>% 
  mutate(rede = case_when(rede %in% 1:3 ~ "Publica",
                          rede == 4 ~ 'Privada'))

rio::export(bd_enemjf2, "enemjf.csv")






# Olhando para a porcentagem de pretos nas escolas
bd_escolasjf <- basedosdados::read_sql("SELECT id_escola
                                       FROM `basedosdados.br_inep_censo_escolar.escola
                                       WHERE ")

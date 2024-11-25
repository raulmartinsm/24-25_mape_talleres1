
# prelim ------------------------------------------------------------------
rm(list = ls())
library(pacman)
p_load(tidyverse, haven)

nov <- read_dta("03_Datos_noviembre_2023.dta")

nov1 <- nov %>% 
  mutate(recuerdo_voto = case_when(p6==1 ~ "PSOE",
                                   p6==2 ~ "PP",
                                   p6==3 ~ "VOX",
                                   p6==4 ~"SUMAR",
                                   is.na(p6)~NA),
         intencion_voto = case_when(p3==1 ~ "PSOE",
                                    p3==2 ~ "PP",
                                    p3==3 ~ "VOX",
                                    p3==4 ~"SUMAR",
                                    p3>16 & p3<22 ~"BAI",
                                    is.na(p3)~NA)) %>% 
  group_by(recuerdo_voto,intencion_voto) %>%
  summarise(count = n()) %>% 
  drop_na() %>% 
  group_by(recuerdo_voto) %>% 
  mutate(total = sum(count)) %>% 
  mutate(porc = count/total*100)

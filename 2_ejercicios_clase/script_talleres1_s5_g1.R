
# Prelim ------------------------------------------------------------------
rm(list = ls())

library(pacman)
p_load(tidyverse,readxl,writexl)


# Loop --------------------------------------------------------------------
numeros <- 1:25

for (i in numeros){
  x <- i^2
  print(paste(i,"al cuadrado es igual a:",x))
  
  rm(x)
}
rm(i,numeros)

#### Ejercicio ####

# Cargo la información electoral
elec23 <- read_xlsx("202307_elec_congr_municipios.xlsx",
                    # Especificando el rango de celdas
                    range = "A6:BT8137") |> 
  # Cambiar el nombre de las variables
  rename(
    CA = `Nombre de Comunidad`,
    IdProv = `Código de Provincia`,
    Prov = `Nombre de Provincia`,
    IdMuni = `Código de Municipio`,
    Muni = `Nombre de Municipio`,
    Pob = Población,
    Mesas = `Número de mesas`,
    Censo = `Total censo electoral`,
    Votantes = `Total votantes`,
    VotVal = `Votos válidos`,
    VotCand = `Votos a candidaturas`,
    VotBl = `Votos en blanco`,
    VotNul = `Votos nulos`,
    JxCAT = `JxCAT - JUNTS`,
    BILDU = `EH Bildu`,
    PNV = `EAJ-PNV`,
    BNG = B.N.G.,
    UPN = U.P.N.
  ) %>% 
  # Necesito crear un Id único de municipio
  mutate(IdProv_c = str_pad(IdProv,width = 2,pad ="0"),
         IdMuni_c = str_pad(IdMuni,width = 3,pad ="0"),
         mun_ine = paste0(IdProv_c,IdMuni_c))


# Hago el loop para guardar la base de datos

# 1. Sacamos un vector con los nombres de las CCAA
CA <- elec23 %>% group_by(CA) %>% summarise() %>% unlist()
CA <- unique(elec23$CA)


# 2. Defino el loop

for (ca in CA){
  # 3.Reducir la bbbdd
  bd <- elec23 %>% filter(CA == ca)
  # 4. Escribir la bbdd
  write_xlsx(bd,path = paste0("resultados2023_",ca,".xlsx"))
}

# Ejemplo de loop con while

seed(1234)
numero_aleatorio <- 0

set.seed(123)
while (numero_aleatorio<=0.5){
  numero_aleatorio <- runif(1)
  print(numero_aleatorio)
}

# Gestionar NAs -----------------------------------------------------------
p_load(CEOdata)

# Cargo el barómetro 2 de 2024

df_ceo <- CEOdata(reo = "1101",
                  raw = F,
                  extra_variables = T)


table(df_ceo$INGRESSOS_1_15,useNA = "ifany")

# 0. Crear una nueva variable donde los datos perdido sean NAs

df_ceo <- df_ceo %>% 
  mutate(ingresos0 = if_else(INGRESSOS_1_15=="No ho sap" | INGRESSOS_1_15 == "No contesta",
                             NA,INGRESSOS_1_15) %>% as.numeric())

# 1. Imputamos el valor de la mediana
df_ceo <- df_ceo %>% 
  mutate(ingresos1 = if_else(is.na(ingresos0),
                             median(ingresos0,na.rm=T),
                             ingresos0))

table(df_ceo$ingresos0)
table(df_ceo$ingresos1)


# 2. Hot Deck
sample_size <- df_ceo %>% filter(is.na(ingresos0)) %>% nrow()

df_ceo <- df_ceo %>% 
  mutate(ingresos2 = ifelse(is.na(ingresos0),
                            ingresos0[!is.na(ingresos0)] %>% 
                              sample(size = sample_size,
                                     replace = T),
                            ingresos0))

table(df_ceo$ingresos0)
table(df_ceo$ingresos2)

# 3. Imputación predictiva
p_load(nnet)


df_ceo <- df_ceo %>% 
  mutate(ingresos0 = as.factor(ingresos0),
         edad = as.numeric(EDAT))

fit <- multinom(ingresos0~ edad + SEXE,
                data = df_ceo,
                na.action = na.exclude)

df_ceo <- df_ceo %>% 
  mutate(ingresos3 = if_else(is.na(ingresos0),
                             predict(fit,newdata = ., type="class"),
                             ingresos0))


table(df_ceo$ingresos0)
table(df_ceo$ingresos3)

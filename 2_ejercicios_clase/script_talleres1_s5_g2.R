
# Prelim ------------------------------------------------------------------
rm(list = ls())

library(pacman)
p_load(tidyverse,readxl,writexl)


# Loop --------------------------------------------------------------------

# creo un vector
numeros <- 1:25

for (i in numeros){
  x <- i^2
  print(paste(i,"al cuadrado es igual a:",x))
}


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

# 1. Sacamos el vector con valores únicos
# a.
vector <- elec23 %>% group_by(CA) %>% summarise() %>% unlist()

# b.
CA <- unique(elec23$CA)


# 2. Hacemos el loop

for (i in CA){
  resultado <- elec23 %>% filter(CA == i)
  
  write_xlsx(resultado,path = paste0("resultados_",i,".xlsx"))
  
}

# 2bis. Hacemos el loop y guardamos cada resultado en una hoja distinta

for (i in CA){
  resultado <- elec23 %>% filter(CA == i)
  
  write_xlsx(resultado,path = "resultados.xlsx",
             sheet =paste0("CA_",i))
  
}


# Imputación --------------------------------------------------------------
p_load(CEOdata)

# Cargamos datos
df_ceo <- CEOdata(reo = "1101",raw = F,extra_variables = T)

censo <- read_xlsx("cens_pob_es_30-50.xlsx")


# Comprobamos si hay NAs
df_ceo <- df_ceo %>% 
  mutate(ingresos = if_else(INGRESSOS_1_15=="No ho sap" | INGRESSOS_1_15=="No contesta",
                            NA,INGRESSOS_1_15))

summary(df_ceo$ingresos)

df_ceo <- df_ceo |> 
  mutate(derecha = case_when(REC_PARLAMENT_VOT %in% c("Junts per Catalunya","PP","VOX","Aliança Catalana","Ciutadans/Ciudadanos") ~ 1,
                             REC_PARLAMENT_VOT %in% c("PSC/PSOE","ERC","Comuns Sumar","CUP","PACMA","Podemos") ~ 0,
                             T ~NA) |> factor(labels = c("Izquierda","Derecha")),
         ingresos0 = if_else(ingresos=="No contesta"|ingresos=="No ho sap",NA,ingresos))

table(df_ceo$ingresos,df_ceo$derecha,useNA = "always")


# No podemos no hacer nada

# # Opción 1: Asignar el valor de la media o mediana

df_ceo <- df_ceo |> 
  mutate(ingresos1 = if_else(is.na(ingresos0),
                             median(as.numeric(ingresos0),na.rm = T),
                             as.numeric(ingresos0)) |> 
           factor(levels = 1:15,
                  labels= c("0€","<300€","301-600","601-900","901-1000","1001-1200","1201-1800","1801-2000",
                            "2001-2400","2401-3000","3001-4000","4001-4500","4501-5000","5001-6000",">6000")))
table(df_ceo$ingresos1)


# # Opción 2: Con datos tipo panel
# Vamos a interpolar

censo_bcn <- censo %>% 
  filter(prov=="Barcelona") %>% 
  mutate(year = censo) %>% 
  group_by(cod_mun) %>% 
  complete(year = full_seq(year,1)) %>% 
  fill(mun,ccaa,cod_ccaa,prov,.direction = "downup") %>% 
  ungroup()

# Interpolamos, ahora sí
p_load(zoo)
censo_bcn_int <- censo_bcn %>% 
  group_by(cod_mun) %>% 
  arrange(cod_mun,year) %>% 
  mutate(pder = na.approx(pder,na.rm = T,rule = 2) %>%
           round(digits = 0))


# Opción 3: Imputar usando HOT DECK
sample_size <- df_ceo %>% filter(is.na(ingresos0)) %>% nrow

df_ceo <- df_ceo %>% 
  mutate(ingresos3 = ifelse(is.na(ingresos0),
                             ingresos0[!is.na(ingresos0)] %>%
                               sample(size = sample_size,
                                      replace = T),
                             ingresos0))


# Opción 4: Imputación predictiva
p_load(nnet)

df_ceo <- df_ceo %>% mutate(ingresos0 = as.numeric(ingresos0) %>% factor(),
                            EDAT = as.numeric(EDAT))

fit <- multinom(ingresos0 ~ EDAT + SEXE,
                data = df_ceo,na.action = na.exclude)


df_ceo <- df_ceo %>% 
  mutate(ingresos4 = if_else(is.na(ingresos0),
                             predict(fit,newdata = ., type = "class"),
                             ingresos0))


# Opción 5: Imputación múltiple
p_load(mice)
# Usamos el paquete mice

# Calculamos la variable ideologia como numérica
df_ceo1 <- df_ceo %>% 
  mutate(ideologia = if_else(as.numeric(IDEOL_0_10)>11,NA,as.numeric(IDEOL_0_10)))

# Hacemos la estimación para 5 bases de datos distintas
df_ceo1_mice <- df_ceo1 %>% 
  select(ideologia,ingresos0,EDAT,SEXE) %>% 
  mice(m=5,seed = 123)

# Hacemos la estimación para cada base de datos
df_ceo1_with <- with(df_ceo1_mice, lm(ideologia ~ ingresos0))

# Resumen de los coeficientes obtenidos en cada base de datos
summary(df_ceo1_with) %>% print(n=Inf)

# Resumen de todas las imputaciones

df_ceo1_pool <- pool(df_ceo1_with)
summary(df_ceo1_pool)

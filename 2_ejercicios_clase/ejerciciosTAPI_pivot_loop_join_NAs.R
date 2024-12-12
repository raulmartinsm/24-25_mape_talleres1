# Preliminares ------------------------------------------------------------
rm(list = ls())
library(pacman)
p_load(tidyverse,readxl,forcats,haven,CEOdata)

# Cargar los datos de las elecciones de 2023 ------------------------------

# No carga los datos correctamente
elec23 <- read_xlsx("datos/02_202307_1.xlsx")

# Hay que hacer algunos ajustes
elec23 <- 
  # Cargar los datos
  read_xlsx("datos/02_202307_1.xlsx",
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
  ) |> 
  # Crear una variable que recoja los votos a otras candidaturas
  mutate(Otros = VotCand- PP - PSOE	- VOX	- SUMAR	- 
           ERC	- JxCAT	- BILDU	- PNV	- BNG	- CCa	- UPN) |> 
  # Reubicar esta variable
  relocate(Otros,.after = UPN) |> 
  # Seleccionar las variables relevantes, hasta la variable PACMA
  select(1:26) |> # o which(names(elec23) == "PACMA"
  # Crear IdUnico para cada municipio
  mutate(
  IdProv_2 = str_pad(IdProv, width = 2, pad = "0"),
  IdMuni_3 = str_pad(IdMuni, width = 3, pad = "0"),
  IdUnico = paste0(IdProv_2,IdMuni_3))

# Joins -------------------------------------------------------------------
p_load(haven)
muni23 <- read_rds(file = "datos/demografia23.rds") |> 
  rename(IdUnico = codine)

join1 <- elec23 |> 
  left_join(y = muni19,
            by = "IdMuniINE")


# Pivot data --------------------------------------------------------------
# De long a wide

e23long <- elec23 |> 
  pivot_longer(cols = 14:25, # de PP a Otros
               names_to = "partido",
               values_to = "votos") |> 
  select(IdUnico,Muni,partido,votos) |> 
  filter(votos!=0)

# Creamos variable de orden

e23long <- e23long |> 
  group_by(IdUnico) |> 
  arrange(IdUnico,-votos) |> 
  mutate(orden = row_number()) |> 
  ungroup()


# Long a wide

e23orden <- e23long |> 
  select(-votos) |> 
  pivot_wider(names_from = "partido",
              values_from = "orden") |> 
  rename_with( ~ paste0("or_", .x)) |> 
  rename(IdUnico = or_IdUnico)


# Unimos datos

elec23 <- elec23 |> 
  left_join(e23orden,by = "IdUnico")

# Loop --------------------------------------------------------------------
##### Sabemos cuantas veces queremos repetirlo #####
# Crear un vector de números
numeros <- 1:25

# Imprimir cada número elevado al cuadrado
for (i in numeros) {
  x <- i^2
  
  print(paste(i,"al cuadrado es igual a:",x))
}

##### No sabemos cuantas veces queremos repetirlo pero sí sabemos lo que queremos conseguir #####
set.seed(9998) # Para saber los números que saldrán, clave en replicabilidad
numero_aleatorio <- 0 # creamos el objeto que vamos a evaluar

# Loop
while (numero_aleatorio <= 0.5) {
  numero_aleatorio <- runif(1) # número aleatorio entre 0 y 1
  print(numero_aleatorio)
}


## Por ejemplo, queremos crear un objeto con los resultados para cada CA por separado

# 1. Necesitamos un vector con los nombres de las CCAA
CA_vector <- elec23 |> group_by(CA) |> summarise() |> unlist()

# 2. Creamos una lista vacía 
df_list <- list()

# 3. Hacemos un loop para que cada elemento de la lista corresponda con una CA
for (i in CA_vector){
  df_list[[i]]  <- elec23 |> 
  filter(CA==i)
}


# 3.bis Hacemos un loop para que cada bbdd se guarde por separado

for (i in CA_vector){
  df_p  <- elec23 |> 
    filter(CA==i)
  
  write.csv(df_p,file = paste0("ejercicios_clase/loop/",i,".csv"))
}


# Imputar NAs ----------------------------------------------
df_ceo <- CEOdata(reo = "1101", raw = F, extra_variables = TRUE)

# En la variable INGRESSOS_1_15 hay NAs
df_ceo <- df_ceo |> 
  mutate(ingresos = INGRESSOS_1_15)

table(df_ceo$ingresos,useNA = "always")

#### Opción 1: No hacer nada ####

# ... pero

# Hay una cierta correlación entre votar a ciertos partidos y no declarar cual es el nivel de ingresos
df_ceo <- df_ceo |> 
  mutate(derecha = case_when(REC_PARLAMENT_VOT %in% c("Junts per Catalunya","PP","VOX","Aliança Catalana","Ciutadans/Ciudadanos") ~ 1,
                             REC_PARLAMENT_VOT %in% c("PSC/PSOE","ERC","Comuns Sumar","CUP","PACMA","Podemos") ~ 0,
                             T ~NA) |> factor(labels = c("Izquierda","Derecha")),
         ingresos0 = if_else(ingresos=="No contesta"|ingresos=="No ho sap",NA,ingresos))

table(df_ceo$ingresos,df_ceo$derecha,useNA = "always")

#### Opcion 2: Imputar con la mediana ####
df_ceo <- df_ceo |> 
  mutate(ingresos1 = if_else(is.na(ingresos0),
                             median(as.numeric(ingresos0),na.rm = T),
                             as.numeric(ingresos0)) |> 
           factor(levels = 1:15,
                  labels= c("0€","<300€","301-600","601-900","901-1000","1001-1200","1201-1800","1801-2000",
                            "2001-2400","2401-3000","3001-4000","4001-4500","4501-5000","5001-6000",">6000")))

table(df_ceo$ingresos1)


#### Opción 2bis: si se trata de datos panel lo más habitual es interpolar datos
df_ine <- read_xlsx("datos/cens_pob_es_30-50.xlsx")

# Selección de una provincia y expansion del número de observaciones
df_ine_bcn <- df_ine |> 
  filter(prov=="Barcelona") |>
  group_by(cod_mun) |> 
  mutate(year = censo) |> 
  complete(year = full_seq(year, 1)) |> # Añade una observación por cada año
  fill(mun,cod_ccaa,ccaa,prov, .direction = "downup") |> 
  mutate(censo = if_else(is.na(censo),0,1))|> 
  ungroup()

# Interpolación
p_load(zoo)
df_ine_int <- df_ine_bcn |> 
  group_by(cod_mun) |> # agrupa por municipios para que la interpolación sea dentro del municipio
  arrange(cod_mun,year) |>  # ordena por municipio y año
  # Interpola dentro de cada municipio
  mutate(hogares = na.approx(hogares,na.rm = FALSE, rule = 2)|> round(digits = 0), # rule = 2 para que se extrapolen los valores
         pder = na.approx(pder, na.rm = FALSE, rule = 2) |> round(digits = 0),
         phec = na.approx(phec, na.rm = FALSE, rule = 2) |> round(digits = 0)) |> 
  ungroup()

#### Opción 3: Imputar usando un método Hot Deck ####
set.seed(123)
sample_size <- df_ceo |> filter(is.na(ingresos0)) |> nrow()

df_ceo <- df_ceo |> 
  mutate(
    ingresos2 = ifelse(
      is.na(ingresos0),
      # De entre todos los que no tienen "No contesta"
      ingresos0[!is.na(ingresos0)] %>% 
        # Elige uno al azar
        sample(size = sample_size, # Tiene que hacerlo tantas veces como NAs haya
               replace = TRUE), # Se puede seleccionar una observación más de una vez
      ingresos0
    )|> 
      factor(levels = 1:15,
             labels= c("0€","<300€","301-600","601-900","901-1000","1001-1200","1201-1800","1801-2000",
                       "2001-2400","2401-3000","3001-4000","4001-4500","4501-5000","5001-6000",">6000"))
  )


# El resultado es que la distribución de la variable imputada es similar a la original
  # No NA in the original variable
  no_na <- df_ceo |> filter(is.na(ingresos0)) |> group_by(ingresos2) |> summarise(count_no_na = n()) |>
    mutate(percent_no_na = count_no_na/sum(count_no_na)*100)
  
  # NA in the original variable
  na <- df_ceo |> filter(is.na(ingresos0)) |> group_by(ingresos2) |> summarise(count_na = n()) |>
    mutate(percent_na = count_na/sum(count_na)*100)
  
  # Comprobamos
  all <- no_na |> left_join(na)
  
  
#### Opción 4: Imputación predictiva #### 
# Básicamente, a partir de un modelo de regresión (en este caso multinomial)
p_load(nnet)
  
# Selecciono los datos
df_ceo1 <- df_ceo |> 
  mutate(ingresos0 = as.numeric(ingresos0) |> factor(),
         EDAT = as.numeric(EDAT))

# Fit a multinomial logistic regression model
fit <- multinom(ingresos0 ~ EDAT + SEXE + OCUPACIO_CNO11_1 + COMARCA, 
                data = df_ceo1, 
                na.action = na.exclude)

# Create a new variable `ingresos4`
df_ceo1 <- df_ceo1 %>%
  mutate(ingresos4 = if_else(is.na(ingresos0),
                             predict(fit, newdata = ., type = "class"),
                             ingresos0)
         )
  
# Check the result
table(as.numeric(df_ceo$ingresos0), useNA = "ifany")
table(df_ceo1$ingresos4, useNA = "ifany")

hist(as.numeric(df_ceo1$ingresos4[is.na(df_ceo1$ingresos0)]),breaks = 1:15)
hist(as.numeric(df_ceo1$ingresos))
  
#### Opción 5: Imputación múltiple: mice & Amelia ####
p_load(mice)

### Mice: 
df_ceo1 <- df_ceo1 |> 
  # Convertimos los valores de la VD que son perdidos en NA
  mutate(ideologia = if_else(as.numeric(IDEOL_0_10)>11,NA,as.numeric(IDEOL_0_10)-1) )

df_ceo1_mice <- df_ceo1 |> 
  # Seleccionamos las variables que queremos usar en nuestro análisis
  select(ideologia,ingresos0,EDAT,SEXE,OCUPACIO_CNO11_1,COMARCA) |> 
  # Imputamos valores de las variables con NAs en 5 bases de datos equivalentes distintas
  mice(m=5,seed = 123)

# Estimamos el valor de nuestra VD a partir de los diferentes valores imputados en cada una de las 5 bases de datos

df_ceo1_with <- with(df_ceo1_mice, lm(ideologia ~ ingresos0))
summary(df_ceo1_with) |> print(n= Inf)


# Combinamos los resultados de todas las bases de datos
df_ceo1_pool <- pool(df_ceo1_with)

summary(df_ceo1_pool)

summary(lm(ideologia ~ ingresos0))

# Ejercicio de Lluís Orriols ----------------------------------------------
rm(list = ls())

df21 <- read_dta("datos/fusion enero-marzo2021.dta")

df21 <- df21 |> 
  mutate(id_i = ESCIDEOL,
         id_ps = ESCAIDEOLLIDERES_1,
         id_pi = ESCAIDEOLLIDERES_3) |> 
  mutate(id_i = if_else(id_i>10,NA,id_i),
         id_ps = if_else(id_ps>10,NA,id_ps),
         id_pi = if_else(id_pi>10,NA,id_pi))

# Creamos variables de distancia a los lideres
df21 <- df21 |> 
  mutate(dist_ps = abs(id_i-id_ps),
         dist_pi = abs(id_i-id_pi),
         dist_cat = case_when(dist_ps<dist_pi ~ 1,
                              dist_ps==dist_pi ~ 2,
                              dist_ps>dist_pi ~ 3))


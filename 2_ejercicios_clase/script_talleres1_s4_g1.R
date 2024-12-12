rm(list = ls())
# Prelim ------------------------------------------------------------------
library(pacman)

p_load(tidyverse,readxl)


# Cargo los datos ---------------------------------------------------------

elec23 <- 
  # Cargar los datos
  read_xlsx("202307_elec_congr_municipios.xlsx",
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
  )

# Combinar bases de datos -------------------------------------------------
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
  
  
# Alcaldes

alcaldes <- read_xlsx("alcaldes_2023.xlsx") %>% 
  separate(NombreCompleto,
           ", ",
           into = c("Apellidos","Nombre")) %>% 
  mutate(Apellidos = str_to_title(Apellidos),
         Nombre = str_to_title(Nombre))



# Combinamos bases de datos -----------------------------------------------

# Combinamos elec23 con alcaldes

elec23_alcaldes <- elec23 %>% 
  left_join(alcaldes,)

elec23_alcaldes <- left_join(x=elec23, y= alcaldes, by = "mun_ine")

# Voy a comprobar cuantas observaciones por municipio tenemos
# y si hay duplicados

# Alcaldes tiene duplicados, por eso aparecen más observaciones
alcaldes %>%
  group_by(mun_ine) %>% 
  summarise(count = n()) %>% 
  filter(count>1)

# Probamos los diferentes resultados en función de los distintos join
elec23_alcaldes <- right_join(x=elec23, y= alcaldes, by = "mun_ine")
elec23_alcaldes <- inner_join(x=elec23, y= alcaldes, by = "mun_ine")
elec23_alcaldes <- full_join(x=elec23, y= alcaldes, by = "mun_ine")

### Voto por correo ###
# Combinamos los datos a nivel municipal con información 
# de voto por correo

# Importo la información de voto por correo
correo <- read_xlsx(path = "votos_correo_2020s.xlsx") %>% 
  filter(election=="GENERALES 23-07-2023")

elec23_p_correo <- elec23 %>% 
  group_by(IdProv_c) %>% 
  summarise(Censo = sum(Censo,na.rm = T),
            Votantes = sum(Votantes,na.rm = T)) %>% 
  left_join(y = correo,by = c("IdProv_c" = "idprov"))
  



# Pivot -------------------------------------------------------------------

# Pasamos la base de datos de wide a long

elec23_long <- elec23 %>% 
  pivot_longer(cols = c("PSOE","PP","VOX","SUMAR"),
               names_to = "Partido",
               values_to = "Votos"
               ) %>% 
  select(mun_ine,Partido,Votos)


# Creamos la variable que indica en qué posición queda el partido
elec23_long <- elec23_long %>% 
  group_by(mun_ine) %>% 
  arrange(mun_ine,desc(Votos)) %>% 
  mutate(orden = row_number()) %>% 
  ungroup()

# Pasamos de la base de datos long a la wide

elec23_wide <- elec23_long %>% 
  select(-Votos) %>% 
  pivot_wider(names_from = "Partido",
              # necesario para que los nombres de variables no se repitan
              names_prefix = "or_", 
              values_from = "orden")
  
  
  
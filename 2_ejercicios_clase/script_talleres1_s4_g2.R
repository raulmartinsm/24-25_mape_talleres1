# Prelim ------------------------------------------------------------------
rm(list = ls())

library(pacman)

p_load(tidyverse,readxl)


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

# Comprobamos si hay municipios con más de un alcalde
alcaldes %>% 
  group_by(mun_ine) %>% 
  summarise(count = n()) %>% 
  filter(count>1)


# Combino alcaldes con resultados electorales
elec23_alcaldes <- elec23 %>% right_join(alcaldes)

##### Elecciones con votos por correo ####

# Paso elec23 a nivel municipal
elec23_p <- elec23 %>% 
  select(IdProv_c,Censo,Votantes) %>% 
  group_by(IdProv_c) %>% 
  summarise(Censo = sum(Censo, na.rm = T),
            Votantes = sum(Votantes, na.rm = T))

# Importo la información de voto por correo
correo <- read_xlsx(path = "votos_correo_2020s.xlsx") %>% 
  filter(election=="GENERALES 23-07-2023")

# Ahora sí, combino

elec23_p_correo <- elec23_p %>% 
  full_join(correo, by = c("IdProv_c" = "idprov"))



# Pivot -------------------------------------------------------------------

# De wide a long
elec23_l <- elec23 %>% 
  pivot_longer(cols = 14:24,
               names_to = "Partido",
               values_to = "Votos") %>% 
  select(mun_ine,Partido,Votos) %>% 
  group_by(mun_ine) %>% 
  arrange(mun_ine,-Votos) %>% 
  filter(Votos!=0) %>% 
  mutate(orden = row_number())


# De long a wide

elec23_w <- elec23_l %>% 
  select(mun_ine,Partido,orden) %>% 
  pivot_wider(names_from = "Partido",
              names_prefix = "or_",
              values_from = "orden")

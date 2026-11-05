# ==========================================================================
# 00_limpieza_datos.R
# Proyecto grupal — CA-0204
# Grupo 5: Alessia Aguilar Badilla, Alonso José Alpízar Brenes,
#          Mariano Morales Carvajal, Derek Escobar Jaramillo,
#          Deilany Fuentes Barquero
# Fecha: noviembre 2026
#
# Descripción: carga el registro de defunciones del INEC (2005–2024),
# revisa la calidad de los datos y prepara la base para el análisis:
# corrige el sexo y la edad, clasifica cada defunción en cinco grandes
# causas de muerte, separa el COVID-19 y crea el grupo de edad, el periodo
# respecto a la pandemia y la fecha del mes. Guarda en data/:
#   - defunciones_limpio.rds: base limpia, una fila por defunción
#   - conteo_crudo.rds:       conteos que se citan en el informe
#
# Datos: Instituto Nacional de Estadística y Censos [INEC]. (2025).
#        Registro de Defunciones (REGDEF). Estadísticas Vitales de Costa Rica.
# ==========================================================================

# Paquetes -----------------------------------------------------------------
library(tidyverse)
library(haven)  # lectura de archivos .sav (SPSS)

# Carga de datos -----------------------------------------------------------
# Base consolidada del INEC (444 371 registros). Solo se cargan las
# variables que se usan en el análisis.
defunciones <- read_sav("data/Defunciones 2005-2024.sav",
                        col_select = c(anodef, mesdef, sexo, edads,
                                       causamuer, grgruposcb)) |>
  rename(año.defuncion = anodef,      # año de la defunción
         mes.defuncion = mesdef,      # mes de la defunción (1 a 12)
         edad          = edads,       # edad en años cumplidos
         causa.muerte  = causamuer,   # código CIE-10 de la causa básica
         gran.grupo    = grgruposcb)  # gran grupo de causa del INEC (17)

# Revisión de los datos ----------------------------------------------------
# Defunciones por año: antes de 2005 solo hay registros tardíos
defunciones |>
  count(año.defuncion)

# Faltantes por variable: no hay NA en ninguna
defunciones |>
  summarise(across(everything(), \(x) sum(is.na(x))))

# Nombres de los 17 grandes grupos de causa de muerte
print_labels(defunciones$gran.grupo)

# Limpieza -----------------------------------------------------------------
# 1. Se quitan las etiquetas de SPSS y se conservan los años completos
#    (2005–2024).
# 2. Sexo con letras: "M" (hombre) y "F" (mujer).
# 3. La edad ignorada (999) pasa a NA.
# 4. Cada defunción se clasifica en cinco grandes causas. El COVID-19
#    (códigos U07 y U09) se separa de las infecciosas y el resto de los
#    grupos del INEC se junta en "Otras".
# 5. Grupo de edad: "Menos de 65" y "65 o más".
# 6. Periodo respecto a la pandemia: antes, pandemia y después.
# 7. Fecha del mes de la defunción.
defunciones_limpio <- defunciones |>
  mutate(across(everything(), zap_labels)) |>
  filter(between(año.defuncion, 2005, 2024)) |>
  mutate(sexo  = case_when(sexo == 1 ~ "M",
                           sexo == 2 ~ "F",
                           .default  = "Sin dato"),
         edad  = na_if(edad, 999),
         causa = case_when(
           str_detect(causa.muerte, "^U0[79]") ~ "COVID-19",
           gran.grupo == 1  ~ "Infecciosas",
           gran.grupo == 2  ~ "Cáncer",
           gran.grupo == 7  ~ "Circulatorias",
           gran.grupo == 8  ~ "Respiratorias",
           gran.grupo == 17 ~ "Externas",
           .default         = "Otras"),
         grupo.edad = case_when(edad < 65  ~ "Menos de 65",
                                edad >= 65 ~ "65 o más"),
         periodo = case_when(año.defuncion <= 2019 ~ "Antes",
                             año.defuncion <= 2021 ~ "Pandemia",
                             .default              = "Después"),
         fecha = make_date(año.defuncion, mes.defuncion, 1))

# Conteos para el informe --------------------------------------------------
# Cifras de la base original y de la base limpia que se citan en el texto
conteo_crudo <- list(
  n_total    = nrow(defunciones),
  n_antes    = sum(defunciones$año.defuncion < 2005),
  n_periodo  = sum(between(defunciones$año.defuncion, 2005, 2024)),
  n_edad_999 = sum(zap_labels(defunciones$edad) == 999 &
                     between(defunciones$año.defuncion, 2005, 2024)),
  n_hombres  = sum(zap_labels(defunciones$sexo) == 1),
  n_mujeres  = sum(zap_labels(defunciones$sexo) == 2),
  n_covid    = sum(str_detect(defunciones$causa.muerte, "^U0[79]")),
  n_limpio   = nrow(defunciones_limpio),
  pct_65     = round(100 * mean(defunciones_limpio$grupo.edad == "65 o más",
                                na.rm = TRUE), 1)
)

# Exportación --------------------------------------------------------------
saveRDS(defunciones_limpio, "data/defunciones_limpio.rds")
saveRDS(conteo_crudo,       "data/conteo_crudo.rds")
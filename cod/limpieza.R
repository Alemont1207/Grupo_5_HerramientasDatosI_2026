# ==========================================================================
# limpieza.R
# Proyecto grupal — CA-0204
# Grupo 5: Alessia Aguilar Badilla, Alonso José Alpízar Brenes,
#          Mariano Morales Carvajal, Derek Escobar Jaramillo,
#          Deilany Fuentes Barquero
# Fecha: noviembre 2026
#
# Descripción: carga el registro de defunciones del INEC (2005–2024)...
#
# Datos: Instituto Nacional de Estadística y Censos [INEC]. (2025).
#        Registro de Defunciones (REGDEF). Estadísticas Vitales de Costa Rica.
# ==========================================================================

# Paquetes -----------------------------------------------------------------
library(tidyverse)
library(haven)  # lectura de archivos .sav (SPSS)

# Carga de datos -----------------------------------------------------------

# Base consolidada del INEC (444 371 registros). Solo se cargan las
# variables que se usan en el análisis. La base trae etiquetas de SPSS:
# sexo, mes y los nombres de los grandes grupos de causa de muerte.
defunciones <- read_sav("data/Defunciones 2005-2024.sav",
                        col_select = c(anodef, mesdef, sexo, edads,
                                       causamuer, grgruposcb)) |>
  rename(año.defuncion = anodef,      # año de la defunción
         mes.defuncion = mesdef,      # mes de la defunción (1 a 12)
         edad          = edads,       # edad en años cumplidos
         causa.muerte  = causamuer,   # código CIE-10 de la causa básica
         gran.grupo    = grgruposcb)  # gran grupo de causa del INEC (17)

# Revisión de los datos ----------------------------------------------------

# Cantidad de defunciones por año. La base reúne las defunciones registradas
# entre 2005 y 2024, por lo que los años anteriores a 2005 solo tienen las
# pocas defunciones que se inscribieron tarde (entre 1 y 630 por año) y
# están incompletos. Por eso el análisis empieza en 2005.

defunciones |>
  count(año.defuncion)

# Cantidad de valores faltantes (NA) en cada variable. Ninguna variable
# tiene NA, la edad usa el código 999 cuando no se conoce

defunciones |>
  summarise(across(everything(), \(x) sum(is.na(x))))

# El INEC usa el código 999 cuando no se conoce la edad de la persona 
# fallecida (473 casos entre 2005 y 2024). Este valor no es una edad real,
# así que en la limpieza se pasa a NA.

defunciones |>
  filter(edad == 999) |>
  count(año.defuncion)

# Cantidad de defunciones por sexo. Solo aparecen los códigos 1 (Hombre)
# y 2 (Mujer), sin valores raros ni NA´s. 
# Hay más defunciones de hombres (252 734) que de mujeres (191 637) ***

defunciones |>
  count(sexo)

# Lista de los 17 grandes grupos de causa de muerte con su número.

print_labels(defunciones$gran.grupo)

# Defunciones por COVID-19 por año y código. El COVID no tiene un solo
# código: U071 (virus identificado), U072 (no identificado), U079 y U099
# (condición posterior al COVID). Todos aparecen desde 2020 y el INEC los
# incluye en el grupo 1 (infecciosas), por lo que en la limpieza se
# separan en su propia causa para no distorsionar la tendencia de las
# infecciosas.

defunciones |>
  filter(str_detect(causa.muerte, "^U0[79]")) |>
  count(año.defuncion, causa.muerte)

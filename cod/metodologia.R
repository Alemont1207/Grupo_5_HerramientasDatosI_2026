# ==========================================================================
# metodologia.R
# Proyecto grupal — CA-0204
# Grupo 5: Alessia Aguilar Badilla, Alonso José Alpízar Brenes,
#          Mariano Morales Carvajal, Derek Escobar Jaramillo,
#          Deilany Fuentes Barquero
# Fecha: noviembre 2026
#
# Descripción: a partir de la base limpia (data/defunciones_limpio.rds),
# arma las series mensuales de defunciones por sexo y causa....
#
# Datos: Instituto Nacional de Estadística y Censos [INEC]. (2025).
#        Registro de Defunciones (REGDEF). Estadísticas Vitales de Costa Rica.
# ==========================================================================

# Paquetes -----------------------------------------------------------------
library(tidyverse)

# Carga de datos -----------------------------------------------------------

# Base limpia generada por cod/limpieza.R: una fila por defunción,
# de 2005 a 2024 (443 411 defunciones).

defunciones_limpio <- readRDS("data/defunciones_limpio.rds")
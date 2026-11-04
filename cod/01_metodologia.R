# ==========================================================================
# metodologia.R
# Proyecto grupal — CA-0204
# Grupo 5: Alessia Aguilar Badilla, Alonso José Alpízar Brenes,
#          Mariano Morales Carvajal, Derek Escobar Jaramillo,
#          Deilany Fuentes Barquero
# Fecha: noviembre 2026
#
# Descripción: a partir de la base limpia (data/defunciones_limpio.rds),
# arma las series de defunciones por sexo y causa, estima la tendencia de
# cada causa antes de la pandemia (2005–2019) y compara las defunciones
# observadas de 2020 a 2024 con las esperadas según esa tendencia. Guarda
# los resultados en data/ para usarlos en el informe.
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

# Series mensuales ---------------------------------------------------------

# Defunciones por mes, sexo y causa, de enero de 2005 a diciembre de 2024
# (240 meses). Si una causa no tuvo defunciones en algún mes (por ejemplo,
# COVID-19 antes de 2020), se agrega con 0 para que todas las series tengan
# los 240 meses.

series_mensuales <- defunciones_limpio |>
  count(fecha, sexo, causa, name = "defunciones") |>
  complete(fecha, sexo, causa, fill = list(defunciones = 0))

# Series anuales -----------------------------------------------------------

# Defunciones por año y sexo de las cinco grandes causas y del total de
# defunciones (todas las causas). Se usan datos anuales para que la
# estacionalidad mensual no influya en la tendencia. El total sirve de
# referencia: una causa gana peso si crece más rápido que el total.

series_anuales <- bind_rows(
  series_mensuales |>
    filter(!causa %in% c("Otras", "COVID-19")),
  series_mensuales |>
    summarise(defunciones = sum(defunciones), .by = c(fecha, sexo)) |>
    mutate(causa = "Total")
) |>
  mutate(año = year(fecha)) |>
  summarise(defunciones = sum(defunciones), .by = c(año, sexo, causa))

# Tendencia antes de la pandemia -------------------------------------------

# Para cada causa y sexo se ajusta una recta al logaritmo de las
# defunciones de 2005 a 2019. Se usa el logaritmo, como en Arnold y
# Glushko (2022), para que los cambios se midan en términos relativos.
# La pendiente se convierte en el cambio porcentual promedio por año.

tendencias <- series_anuales |>
  filter(año <= 2019) |>
  summarise(intercepto = coef(lm(log(defunciones) ~ año))[1],
            pendiente  = coef(lm(log(defunciones) ~ año))[2],
            .by = c(sexo, causa)) |>
  mutate(cambio_anual = round(100 * (exp(pendiente) - 1), 1))

# Observado contra esperado ------------------------------------------------

# Con la tendencia de 2005–2019 se calculan las defunciones que se
# esperarían de 2020 a 2024 si nada hubiera cambiado. La diferencia indica
# en qué porcentaje lo observado quedó por encima (+) o por debajo (−) de
# lo esperado.

comparacion <- series_anuales |>
  filter(año >= 2020) |>
  left_join(tendencias, by = c("sexo", "causa")) |>
  mutate(esperado   = exp(intercepto + pendiente * año),
         diferencia = round(100 * (defunciones / esperado - 1), 1)) |>
  select(año, sexo, causa, defunciones, esperado, diferencia)

# Resumen después de la pandemia (2022–2024): diferencia promedio de cada
# causa con lo esperado, para saber si volvió o no a su tendencia.

resumen_despues <- comparacion |>
  filter(año >= 2022) |>
  summarise(diferencia_promedio = round(mean(diferencia), 1),
            .by = c(sexo, causa)) |>
  left_join(select(tendencias, sexo, causa, cambio_anual),
            by = c("sexo", "causa"))

# Exportación --------------------------------------------------------------

# Se guardan los resultados para usarlos en el informe.

saveRDS(series_anuales,  "data/series_anuales.rds")
saveRDS(tendencias,      "data/tendencias.rds")
saveRDS(comparacion,     "data/comparacion.rds")
saveRDS(resumen_despues, "data/resumen_despues.rds")
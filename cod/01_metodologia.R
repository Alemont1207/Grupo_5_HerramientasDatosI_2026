# ==========================================================================
# 01_metodologia.R
# Proyecto grupal — CA-0204
# Grupo 5: Alessia Aguilar Badilla, Alonso José Alpízar Brenes,
#          Mariano Morales Carvajal, Derek Escobar Jaramillo,
#          Deilany Fuentes Barquero
# Fecha: noviembre 2026
#
# Descripción: a partir de la base limpia (data/defunciones_limpio.rds),
# estima la tendencia de cada causa de muerte antes de la pandemia
# (2005–2019), compara las defunciones observadas de 2020 a 2024 con las
# esperadas según esa tendencia y prepara los datos de los gráficos y
# tablas del informe. Guarda los resultados en data/.
#
# Datos: Instituto Nacional de Estadística y Censos [INEC]. (2025).
#        Registro de Defunciones (REGDEF). Estadísticas Vitales de Costa Rica.
# ==========================================================================

# Paquetes -----------------------------------------------------------------
library(tidyverse)

# Carga de datos -----------------------------------------------------------
# Base limpia generada por cod/00_limpieza_datos.R
defunciones_limpio <- readRDS("data/defunciones_limpio.rds")

# Cinco grandes causas en el orden en que se muestran en el informe
causas_5 <- c("Circulatorias", "Cáncer", "Externas",
              "Respiratorias", "Infecciosas")

# Clasificación de las causas ----------------------------------------------
# Defunciones y porcentaje de cada causa en todo el periodo (2005–2024)
clasificacion_causas <- defunciones_limpio |>
  count(causa) |>
  mutate(porcentaje = 100 * n / sum(n))

# Series anuales -----------------------------------------------------------
# Defunciones por año y sexo de las cinco grandes causas y del total de
# defunciones (todas las causas), que sirve de referencia.
series_anuales <- bind_rows(
  defunciones_limpio |>
    filter(causa %in% causas_5) |>
    count(año = año.defuncion, sexo, causa, name = "defunciones"),
  defunciones_limpio |>
    count(año = año.defuncion, sexo, name = "defunciones") |>
    mutate(causa = "Total")
)

# Tendencia antes de la pandemia -------------------------------------------
# Para cada causa y sexo se ajusta una recta al logaritmo de las
# defunciones de 2005 a 2019. La pendiente se convierte en el cambio
# porcentual promedio por año.
tendencias <- series_anuales |>
  filter(año <= 2019) |>
  summarise(intercepto = coef(lm(log(defunciones) ~ año))[1],
            pendiente  = coef(lm(log(defunciones) ~ año))[2],
            .by = c(sexo, causa)) |>
  mutate(cambio_anual = round(100 * (exp(pendiente) - 1), 1))

# Observado contra esperado ------------------------------------------------
# Defunciones esperadas de 2020 a 2024 según la tendencia de 2005–2019 y
# diferencia porcentual con las observadas.
comparacion <- series_anuales |>
  filter(año >= 2020) |>
  left_join(tendencias, by = c("sexo", "causa")) |>
  mutate(esperado   = exp(intercepto + pendiente * año),
         diferencia = round(100 * (defunciones / esperado - 1), 1)) |>
  select(año, sexo, causa, defunciones, esperado, diferencia)

# Tendencia previa y desviación posterior ----------------------------------
# Para cada causa y sexo: cambio anual 2005–2019, diferencia con el cambio
# anual del total (puntos porcentuales) y diferencia entre las defunciones
# observadas y esperadas acumuladas en 2022–2024 (%).
dif_post <- comparacion |>
  filter(año >= 2022) |>
  summarise(dif = 100 * (sum(defunciones) / sum(esperado) - 1),
            .by = c(sexo, causa))

tabla_tendencias <- tendencias |>
  select(sexo, causa, cambio_anual) |>
  left_join(dif_post, by = c("sexo", "causa")) |>
  mutate(frente_total = cambio_anual - cambio_anual[causa == "Total"],
         .by = sexo) |>
  pivot_wider(names_from = sexo,
              values_from = c(cambio_anual, frente_total, dif)) |>
  mutate(causa = factor(causa, levels = c(causas_5, "Total"))) |>
  arrange(causa) |>
  select(causa, cambio_anual_F, cambio_anual_M,
         frente_total_F, frente_total_M, dif_F, dif_M)

# Datos para los gráficos y tablas -----------------------------------------

# Porcentaje de las defunciones de cada año por causa (ambos sexos).
# Se calcula sobre el total del año, incluyendo COVID-19 y "Otras".
porcentaje_anual <- defunciones_limpio |>
  count(año.defuncion, causa, name = "defunciones") |>
  mutate(porcentaje = defunciones / sum(defunciones),
         .by = año.defuncion) |>
  filter(causa %in% causas_5)

# Porcentaje de las defunciones de cada año por causa y sexo
porcentaje_sexo <- defunciones_limpio |>
  count(año.defuncion, sexo, causa, name = "defunciones") |>
  mutate(porcentaje = defunciones / sum(defunciones),
         .by = c(año.defuncion, sexo)) |>
  filter(causa %in% causas_5) |>
  mutate(sexo = if_else(sexo == "F", "Mujeres", "Hombres"),
         sexo = factor(sexo, levels = c("Mujeres", "Hombres")))

# Participación (%) de cada causa en el total, mujeres y hombres para
# 2005, 2019, 2021 y 2024
participacion <- defunciones_limpio |>
  mutate(grupo = if_else(sexo == "F", "Mujeres", "Hombres")) |>
  bind_rows(defunciones_limpio |> mutate(grupo = "Total")) |>
  count(grupo, año.defuncion, causa, name = "defunciones") |>
  mutate(porcentaje = 100 * defunciones / sum(defunciones),
         .by = c(grupo, año.defuncion)) |>
  filter(año.defuncion %in% c(2005, 2019, 2021, 2024),
         causa %in% causas_5) |>
  select(grupo, causa, año.defuncion, porcentaje) |>
  pivot_wider(names_from = año.defuncion, values_from = porcentaje) |>
  mutate(grupo = factor(grupo, levels = c("Total", "Mujeres", "Hombres")),
         causa = factor(causa, levels = causas_5)) |>
  arrange(grupo, causa)

# Defunciones observadas (2005–2024) y esperadas (2020–2024) por causa y
# sexo. El valor observado de 2019 se repite como inicio de la línea
# esperada para que ambas líneas queden unidas en el gráfico.
observado_esperado <- bind_rows(
  series_anuales |>
    filter(causa %in% causas_5) |>
    transmute(año, sexo, causa, valor = defunciones, tipo = "Observado"),
  series_anuales |>
    filter(causa %in% causas_5, año == 2019) |>
    transmute(año, sexo, causa, valor = defunciones, tipo = "Esperado"),
  comparacion |>
    filter(causa %in% causas_5) |>
    transmute(año, sexo, causa, valor = esperado, tipo = "Esperado")
) |>
  mutate(sexo  = if_else(sexo == "F", "Mujeres", "Hombres"),
         serie = factor(paste(sexo, "-", tipo),
                        levels = c("Hombres - Observado", "Hombres - Esperado",
                                   "Mujeres - Observado", "Mujeres - Esperado")),
         causa = factor(causa, levels = causas_5))

# Exportación --------------------------------------------------------------
saveRDS(clasificacion_causas, "data/clasificacion_causas.rds")
saveRDS(tendencias,           "data/tendencias.rds")
saveRDS(comparacion,          "data/comparacion.rds")
saveRDS(tabla_tendencias,     "data/tabla_tendencias.rds")
saveRDS(porcentaje_anual,     "data/porcentaje_anual.rds")
saveRDS(porcentaje_sexo,      "data/porcentaje_sexo.rds")
saveRDS(participacion,        "data/participacion.rds")
saveRDS(observado_esperado,   "data/observado_esperado.rds")
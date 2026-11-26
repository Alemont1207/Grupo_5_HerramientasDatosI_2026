# ==========================================================================
# 01_metodologia.R
# Proyecto grupal — CA-0204
# Grupo 5: Alessia Aguilar Badilla, Alonso José Alpízar Brenes,
#          Mariano Morales Carvajal, Derek Escobar Jaramillo,
#          Deilany Fuentes Barquero
# Fecha: noviembre 2026
#
# Descripción: a partir de la base limpia (data/defunciones_limpio.rds) y
# de la población del INEC (data/poblacion_inec.csv), calcula tasas de
# mortalidad estandarizadas por edad, estima la tendencia de cada causa de
# muerte antes de la pandemia (2005–2019), compara las tasas observadas de
# 2020 a 2024 con las esperadas según esa tendencia y prepara los datos de
# los gráficos y tablas del informe. Guarda los resultados en data/.
#
# Datos: Instituto Nacional de Estadística y Censos [INEC]. (2025).
#        Registro de Defunciones (REGDEF). Estadísticas Vitales de Costa Rica.
#        INEC. (2025). Estimaciones y proyecciones nacionales de población
#        1950–2100 (cuadro 2: población por sexo y grupos de edad).
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

# Población -----------------------------------------------------------------
# Población al 30 de junio por año, sexo y grupo quinquenal de edad
# (INEC, estimaciones y proyecciones 1950–2100, cuadro 2), en formato largo.
poblacion <- read_csv("data/poblacion_inec.csv", show_col_types = FALSE) |>
  pivot_longer(-c(grupo.quinquenal, sexo), names_to = "año",
               values_to = "poblacion", names_transform = as.integer)

grupos_quinquenales <- unique(poblacion$grupo.quinquenal)

# Población estándar: población total (ambos sexos) de 2019 por grupo de
# edad. Sus pesos suman 1 y son los mismos para todos los años y sexos.
poblacion_estandar <- poblacion |>
  filter(año == 2019) |>
  summarise(poblacion = sum(poblacion), .by = grupo.quinquenal) |>
  mutate(peso = poblacion / sum(poblacion)) |>
  select(grupo.quinquenal, peso)

# Tasas de mortalidad -------------------------------------------------------
# Para las tasas se usan las defunciones con edad conocida. Cada defunción
# se asigna al mismo grupo quinquenal que usa el INEC (0-4, ..., 100+).
defunciones_edad <- defunciones_limpio |>
  filter(!is.na(edad)) |>
  mutate(grupo.quinquenal = if_else(edad >= 100, "100+",
                                    paste0(5 * (edad %/% 5), "-",
                                           5 * (edad %/% 5) + 4)))

# Defunciones por año, sexo, causa y grupo de edad de las cinco grandes
# causas y del total de defunciones (todas las causas), que sirve de
# referencia. Los grupos sin defunciones se completan con cero.
defunciones_grupo <- bind_rows(
  defunciones_edad |>
    filter(causa %in% causas_5) |>
    count(año = año.defuncion, sexo, causa, grupo.quinquenal,
          name = "defunciones"),
  defunciones_edad |>
    count(año = año.defuncion, sexo, grupo.quinquenal,
          name = "defunciones") |>
    mutate(causa = "Total")
) |>
  complete(año, sexo, causa, grupo.quinquenal = grupos_quinquenales,
           fill = list(defunciones = 0))

# Tasa estandarizada por edad (por 100 000 habitantes): promedio de las
# tasas de cada grupo de edad, ponderado con la población estándar. Así,
# los cambios en el tamaño y en la estructura por edad de la población no
# afectan la comparación entre años.
series_anuales <- defunciones_grupo |>
  left_join(poblacion, by = c("año", "sexo", "grupo.quinquenal")) |>
  left_join(poblacion_estandar, by = "grupo.quinquenal") |>
  summarise(tasa        = 100000 * sum(defunciones / poblacion * peso),
            defunciones = sum(defunciones),
            .by = c(año, sexo, causa))

# Tendencia antes de la pandemia -------------------------------------------
# Para cada causa y sexo se ajusta una recta al logaritmo de la tasa
# estandarizada de 2005 a 2019. La pendiente se convierte en el cambio
# porcentual promedio por año.
tendencias <- series_anuales |>
  filter(año <= 2019) |>
  summarise(intercepto = coef(lm(log(tasa) ~ año))[1],
            pendiente  = coef(lm(log(tasa) ~ año))[2],
            .by = c(sexo, causa)) |>
  mutate(cambio_anual = 100 * (exp(pendiente) - 1))

# Observado contra esperado ------------------------------------------------
# Tasas esperadas de 2020 a 2024 según la tendencia de 2005–2019 y
# diferencia porcentual con las observadas.
comparacion <- series_anuales |>
  filter(año >= 2020) |>
  left_join(tendencias, by = c("sexo", "causa")) |>
  mutate(esperado   = exp(intercepto + pendiente * año),
         diferencia = round(100 * (tasa / esperado - 1), 1)) |>
  select(año, sexo, causa, defunciones, tasa, esperado, diferencia)

# Tendencia previa y desviación posterior ----------------------------------
# Para cada causa y sexo: cambio anual 2005–2019, diferencia con el cambio
# anual del total (puntos porcentuales) y diferencia entre las tasas
# observadas y esperadas acumuladas en 2022–2024 (%). La diferencia con el
# total se calcula antes de redondear.
dif_post <- comparacion |>
  filter(año >= 2022) |>
  summarise(dif = 100 * (sum(tasa) / sum(esperado) - 1),
            .by = c(sexo, causa))

tabla_tendencias <- tendencias |>
  select(sexo, causa, cambio_anual) |>
  left_join(dif_post, by = c("sexo", "causa")) |>
  mutate(frente_total = cambio_anual - cambio_anual[causa == "Total"],
         .by = sexo) |>
  pivot_wider(names_from = sexo,
              values_from = c(cambio_anual, frente_total, dif)) |>
  mutate(causa = factor(causa, levels = c(causas_5, "Total")),
         across(-causa, \(x) round(x, 1))) |>
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

# Tasas estandarizadas observadas (2005–2024) y esperadas (2020–2024) por
# causa y sexo. El valor observado de 2019 se repite como inicio de la
# línea esperada para que ambas líneas queden unidas en el gráfico.
observado_esperado <- bind_rows(
  series_anuales |>
    filter(causa %in% causas_5) |>
    transmute(año, sexo, causa, valor = tasa, tipo = "Observado"),
  series_anuales |>
    filter(causa %in% causas_5, año == 2019) |>
    transmute(año, sexo, causa, valor = tasa, tipo = "Esperado"),
  comparacion |>
    filter(causa %in% causas_5) |>
    transmute(año, sexo, causa, valor = esperado, tipo = "Esperado")
) |>
  mutate(sexo  = if_else(sexo == "F", "Mujeres", "Hombres"),
         serie = factor(paste(sexo, "-", tipo),
                        levels = c("Hombres - Observado", "Hombres - Esperado",
                                   "Mujeres - Observado", "Mujeres - Esperado")),
         causa = factor(causa, levels = causas_5))

# Grupos de edad -----------------------------------------------------------
# Se usan los grupos de edad (menos de 65 y 65 o más) y los periodos
# respecto a la pandemia creados en la limpieza. Se excluyen las
# defunciones con edad ignorada.
periodos <- c("Antes", "Pandemia", "Después")
edad_conocida <- defunciones_limpio |>
  filter(!is.na(grupo.edad))

# Porcentaje de las defunciones de cada periodo que corresponde a cada
# grupo de edad
edad_periodo <- edad_conocida |>
  count(periodo, grupo.edad, name = "defunciones") |>
  mutate(porcentaje = 100 * defunciones / sum(defunciones),
         .by = periodo) |>
  mutate(periodo    = factor(periodo, levels = periodos),
         grupo.edad = factor(grupo.edad, levels = c("Menos de 65", "65 o más"))) |>
  arrange(periodo, grupo.edad)

# Participación (%) de cada causa en las defunciones de cada grupo de edad
# y periodo. El porcentaje se calcula sobre todas las defunciones del grupo,
# incluyendo COVID-19 y "Otras".
causas_edad <- edad_conocida |>
  count(grupo.edad, periodo, causa, name = "defunciones") |>
  mutate(porcentaje = 100 * defunciones / sum(defunciones),
         .by = c(grupo.edad, periodo)) |>
  filter(causa %in% causas_5) |>
  select(grupo.edad, causa, periodo, porcentaje) |>
  pivot_wider(names_from = periodo, values_from = porcentaje) |>
  mutate(grupo.edad = factor(grupo.edad, levels = c("Menos de 65", "65 o más")),
         causa      = factor(causa, levels = causas_5)) |>
  arrange(grupo.edad, causa) |>
  select(grupo.edad, causa, all_of(periodos))

# Exportación --------------------------------------------------------------
saveRDS(clasificacion_causas, "data/clasificacion_causas.rds")
saveRDS(series_anuales,       "data/series_anuales.rds")
saveRDS(tendencias,           "data/tendencias.rds")
saveRDS(comparacion,          "data/comparacion.rds")
saveRDS(tabla_tendencias,     "data/tabla_tendencias.rds")
saveRDS(porcentaje_anual,     "data/porcentaje_anual.rds")
saveRDS(porcentaje_sexo,      "data/porcentaje_sexo.rds")
saveRDS(participacion,        "data/participacion.rds")
saveRDS(observado_esperado,   "data/observado_esperado.rds")
saveRDS(edad_periodo,         "data/edad_periodo.rds")
saveRDS(causas_edad,          "data/causas_edad.rds")
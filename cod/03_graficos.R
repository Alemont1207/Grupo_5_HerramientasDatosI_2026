
library(tidyverse)

# Datos de la metodología
series_anuales <- readRDS("data/series_anuales.rds")
tendencias <- readRDS("data/tendencias.rds")
comparacion <- readRDS("data/comparacion.rds")
resumen_despues <- readRDS("data/resumen_despues.rds")

# Categorías del estudio
causas <- c("Infecciosas", "Cáncer", "Circulatorias",
            "Respiratorias", "Externas")

colores <- c(
  "Infecciosas" = "#009E73",
  "Cáncer" = "#CC79A7",
  "Circulatorias" = "#0072B2",
  "Respiratorias" = "#E69F00",
  "Externas" = "#D55E00"
)

tema_informe <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_blank()
  )

# Agrupación nacional: sumar ambos sexos
anuales <- series_anuales |>
  summarise(defunciones = sum(defunciones),
            .by = c(año, causa))

# Figura 1: evolución histórica
g_evolucion <- ggplot(
  filter(anuales, causa %in% causas),
  aes(año, defunciones, color = causa)
) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = colores) +
  scale_x_continuous(breaks = seq(2005, 2024, 3)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Año", y = "Defunciones") +
  tema_informe

# Figura 2: participación relativa, se incluye 
# COVID-19 y otras causas por lo que las cinco categorías
# no necesariamente sumarán 100%
participacion <- anuales |>
  filter(causa %in% causas) |>
  left_join(
    anuales |>
      filter(causa == "Total") |>
      transmute(año, total = defunciones),
    by = "año"
  ) |>
  mutate(porcentaje = 100 * defunciones / total)

g_participacion <- ggplot(
  participacion,
  aes(año, porcentaje, color = causa)
) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = colores) +
  scale_x_continuous(breaks = seq(2005, 2024, 3)) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%")
  ) +
  labs(x = "Año", y = "Porcentaje del total de defunciones") +
  tema_informe

# Figura 3: tendencias anteriores a 2020
tendencias_nacional <- anuales |>
  filter(año <= 2019, causa %in% causas) |>
  group_by(causa) |>
  summarise(
    pendiente = coef(lm(log(defunciones) ~ año))[2],
    .groups = "drop"
  ) |>
  mutate(cambio_anual = 100 * (exp(pendiente) - 1))

g_tendencias <- ggplot(
  tendencias_nacional,
  aes(x = cambio_anual,
      y = reorder(causa, cambio_anual),
      fill = causa)
) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = "grey40") +
  scale_fill_manual(values = colores, guide = "none") +
  labs(
    x = "Cambio porcentual anual estimado (2005-2019)",
    y = NULL
  ) +
  tema_informe

# Figura 4: observado frente a esperado
# Se agregan las predicciones calculadas por sexo.
comparacion_nacional <- comparacion |>
  filter(causa %in% causas) |>
  summarise(
    observado = sum(defunciones),
    esperado = sum(esperado),
    .by = c(año, causa)
  ) |>
  pivot_longer(
    cols = c(observado, esperado),
    names_to = "serie",
    values_to = "defunciones"
  )

g_comparacion <- ggplot(
  comparacion_nacional,
  aes(año, defunciones, color = serie, linetype = serie)
) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ causa, scales = "free_y") +
  scale_color_manual(
    values = c("observado" = "#0072B2",
               "esperado" = "#D55E00"),
    labels = c("Observado", "Esperado")
  ) +
  scale_linetype_manual(
    values = c("observado" = "solid",
               "esperado" = "dashed"),
    labels = c("Observado", "Esperado")
  ) +
  scale_x_continuous(breaks = 2020:2024) +
  labs(x = "Año", y = "Defunciones") +
  tema_informe

# Tabla de síntesis
tabla_resultados <- tendencias_nacional |>
  select(causa, cambio_anual) |>
  left_join(
    comparacion |>
      filter(año >= 2022, causa %in% causas) |>
      summarise(
        observado = sum(defunciones),
        esperado = sum(esperado),
        .by = causa
      ) |>
      mutate(
        diferencia = 100 * (observado / esperado - 1)
      ) |>
      select(causa, diferencia),
    by = "causa"
  ) |>
  mutate(
    cambio_anual = round(cambio_anual, 1),
    diferencia = round(diferencia, 1)
  )


library(tidyverse)

# Datos de la metodología
defunciones_limpio <- readRDS("data/defunciones_limpio.rds")

# Fig 1. Porcentaje anual de las defunciones por causa y sexo. Se calcula sobre el
# total de cada año y sexo, incluyendo COVID-19 y "Otras", que no se grafican.
porcentaje_sexo <- defunciones_limpio |>
  count(año.defuncion, sexo, causa, name = "defunciones") |>
  mutate(porcentaje = defunciones / sum(defunciones),
         .by = c(año.defuncion, sexo)) |>
  filter(!causa %in% c("Otras", "COVID-19")) |>
  mutate(sexo = if_else(sexo == "F", "Mujeres", "Hombres"),
         sexo = factor(sexo, levels = c("Mujeres", "Hombres")))

externas <- porcentaje_sexo |> filter(causa == "Externas")

# Etiquetas al final de cada línea. En mujeres, respiratorias y externas
# quedan muy cerca en 2024, así que se separan un poco para que no choquen.
etiquetas_sexo <- porcentaje_sexo |>
  filter(año.defuncion == 2024) |>
  mutate(y_etiqueta = porcentaje + case_when(
    sexo == "Mujeres" & causa == "Respiratorias" ~  0.012,
    sexo == "Mujeres" & causa == "Externas"      ~ -0.012,
    .default = 0))

g_sexo <- ggplot(porcentaje_sexo,
       aes(x = año.defuncion, y = porcentaje, group = causa)) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
           fill = "grey93") +
  geom_text(data = tibble(sexo = factor("Mujeres",
                                        levels = c("Mujeres", "Hombres"))),
            aes(x = 2020.5, y = 0.33, label = "Pandemia"),
            inherit.aes = FALSE, colour = "grey45", size = 3) +
  geom_line(colour = "grey75", linewidth = 0.6) +
  geom_line(data = externas, colour = "#2B5C8A", linewidth = 1.2) +
  geom_text(data = filter(etiquetas_sexo, causa != "Externas"),
            aes(y = y_etiqueta, label = causa),
            hjust = -0.1, size = 3, colour = "grey45") +
  geom_text(data = filter(etiquetas_sexo, causa == "Externas"),
            aes(y = y_etiqueta, label = causa),
            hjust = -0.1, size = 3, colour = "#2B5C8A") +
  facet_wrap(~ sexo) +
  scale_x_continuous(breaks = seq(2005, 2024, 5),
                     expand = expansion(mult = c(0.02, 0.35))) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.34)) +
  labs(title = "Las causas externas pesan tres veces más en los hombres",
       x = NULL, y = "Porcentaje de las defunciones") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot")


# Figura 2
# Porcentaje anual de las defunciones por causa (ambos sexos)
porcentaje_anual <- defunciones_limpio |>
  count(año.defuncion, causa, name = "defunciones") |>
  mutate(porcentaje = defunciones / sum(defunciones), .by = año.defuncion) |>
  filter(!causa %in% c("Otras", "COVID-19"))

respiratorias <- porcentaje_anual |> filter(causa == "Respiratorias")
etiquetas     <- porcentaje_anual |> filter(año.defuncion == 2024)

g_evolucion <- ggplot(porcentaje_anual, aes(x = año.defuncion, y = porcentaje, group = causa)) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
           fill = "grey93") +
  annotate("text", x = 2020.5, y = 0.32, label = "Pandemia",
           colour = "grey45", size = 3.5) +
  geom_line(colour = "grey75", linewidth = 0.7) +
  geom_line(data = respiratorias, colour = "#2B5C8A", linewidth = 1.2) +
  geom_text(data = filter(etiquetas, causa != "Respiratorias"),
            aes(label = causa), hjust = -0.1, size = 3.5, colour = "grey45") +
  geom_text(data = filter(etiquetas, causa == "Respiratorias"),
            aes(label = causa), hjust = -0.1, size = 3.5, colour = "#2B5C8A") +
  scale_x_continuous(breaks = seq(2005, 2024, 5),
                     expand = expansion(mult = c(0.02, 0.20))) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
  labs(title = "Las respiratorias no recuperaron su peso después de la pandemia",
       x = NULL, y = "Porcentaje de las defunciones") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot")


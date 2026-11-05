# ¿Qué causas de muerte ganan peso en Costa Rica?

Proyecto grupal del curso CA-0204 Herramientas de Ciencia de Datos I (UCR, II-2026).

**Grupo 5:** Alessia Aguilar Badilla, Alonso José Alpízar Brenes, Mariano Morales Carvajal, Derek Escobar Jaramillo y Deilany Fuentes Barquero.

Análisis de la evolución de las cinco grandes causas de muerte en Costa Rica (infecciosas, cáncer, circulatorias, respiratorias y externas) entre 2005 y 2024, a partir del Registro de Defunciones del INEC. Se estima la tendencia de cada causa entre 2005 y 2019 y se compara con las defunciones observadas entre 2020 y 2024.

## Estructura

| Carpeta | Contenido |
|---|---|
| `cod/` | Informe en Quarto, con la carga, limpieza y análisis de los datos |
| `data/` | Base de defunciones del INEC (2005–2024) |
| `info/` | Documentos de apoyo (enunciado, rúbrica, artículos) |
| `config/` | Lista de paquetes del proyecto |
| `res/` | Informe final en PDF |

## Cómo reproducirlo

1. Abrir `Grupo_5_HerramientasDatosI_2026.Rproj` en RStudio.
2. Instalar los paquetes de `config/paquetes.txt`: `install.packages(c("tidyverse", "haven"))`.
3. Renderizar el informe desde la terminal de RStudio:
   `quarto render cod/Grupo_5_HerramientasDatosI_2026.qmd && mv cod/Grupo_5_HerramientasDatosI_2026.pdf res/`

## Fuente de datos

Instituto Nacional de Estadística y Censos [INEC]. (2025). *Registro de Defunciones (REGDEF): Estadísticas Vitales de Costa Rica* [Conjunto de datos]. https://sistemas.inec.cr/nada5.4/index.php/catalog/REGDEF/
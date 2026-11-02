# Grupo_5_HerramientasDatosI_2026
# ¿Qué causas de muerte ganan peso en Costa Rica?

Proyecto grupal del curso CA-0204 Herramientas de Ciencia de Datos I (UCR, II-2026).

**Grupo 5:** Alessia Aguilar Badilla, Alonso José Alpízar Brenes, Mariano Morales Carvajal, Derek Escobar Jaramillo y Deilany Fuentes Barquero.

Análisis de la evolución de las cinco grandes causas de muerte en Costa Rica (infecciosas, cáncer, circulatorias, respiratorias y externas) entre 2005 y 2024, a partir del Registro de Defunciones del INEC.
## Estructura

| Carpeta | Contenido |
|---|---|
| `cod/` | Scripts de R y el informe en Quarto |
| `data/` | Datos originales del INEC y bases generadas por los scripts |
| `info/` | Documentos de apoyo (enunciado, rúbrica, artículos) |
| `config/` | Lista de paquetes del proyecto |
| `res/` | Informe final en PDF |

## Cómo reproducirlo

Los scripts se corren en este orden:

1. `cod/limpieza.R`: lee `data/Defunciones 2005-2024.sav` y genera `data/defunciones_limpio.rds`.
2. `cod/metodologia.R`: lee `data/defunciones_limpio.rds` y genera los resultados del análisis.
3. `cod/Grupo_5_HerramientasDatosI_2026.qmd`: lee esos resultados y genera `res/Grupo_5_HerramientasDatosI_2026.pdf`.

Pasos:

1. Abrir `Grupo_5_HerramientasDatosI_2026.Rproj` en RStudio.
2. Instalar los paquetes de `config/paquetes.txt`: `install.packages(c("tidyverse", "haven"))`.
3. Correr los scripts en el orden indicado.
4. Renderizar el informe desde la terminal:
   `quarto render cod/Grupo_5_HerramientasDatosI_2026.qmd --output-dir res`

## Fuente de datos

Instituto Nacional de Estadística y Censos [INEC]. (2025). *Registro de Defunciones (REGDEF): Estadísticas Vitales de Costa Rica* [Conjunto de datos]. https://sistemas.inec.cr/nada5.4/index.php/catalog/REGDEF/
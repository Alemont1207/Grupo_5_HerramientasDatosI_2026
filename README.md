# ¿Qué causas de muerte ganan peso en Costa Rica?

Proyecto grupal del curso CA-0204 Herramientas de Ciencia de Datos I (UCR, II-2026).

**Grupo 5:** Alessia Aguilar Badilla, Alonso José Alpízar Brenes, Mariano Morales Carvajal, Derek Escobar Jaramillo y Deilany Fuentes Barquero.

Análisis de la evolución de las cinco grandes causas de muerte en Costa Rica (infecciosas, cáncer, circulatorias, respiratorias y externas) entre 2005 y 2024, a partir del Registro de Defunciones del INEC. Se estima la tendencia de cada causa entre 2005 y 2019 y se compara con las defunciones observadas entre 2020 y 2024.

## Estructura

| Carpeta | Contenido |
|---|---|
| `cod/` | Scripts de limpieza y metodología, y el informe en Quarto |
| `data/` | Base de defunciones del INEC (2005–2024) y bases generadas por los scripts |
| `info/` | Documentos de apoyo (enunciado, rúbrica, artículos) |
| `config/` | Lista de paquetes del proyecto |
| `res/` | Informe final en PDF |

## Cómo reproducirlo

Los archivos se corren en este orden:

1. `cod/00_limpieza_datos.R`: lee `data/Defunciones 2005-2024.sav`, limpia los datos y genera `data/defunciones_limpio.rds` y `data/conteo_crudo.rds`.
2. `cod/01_metodologia.R`: estima las tendencias, compara lo observado con lo esperado y genera en `data/` los resultados que usa el informe.
3. `cod/Grupo_5_HerramientasDatosI_2026.qmd`: genera el informe en PDF.

Pasos:

1. Abrir `Grupo_5_HerramientasDatosI_2026.Rproj` en RStudio.
2. Instalar los paquetes de `config/paquetes.txt`: `install.packages(c("tidyverse", "haven", "kableExtra"))`.
3. Desde la terminal de RStudio, correr:

```
Rscript cod/00_limpieza_datos.R
Rscript cod/01_metodologia.R
quarto render cod/Grupo_5_HerramientasDatosI_2026.qmd && mv cod/Grupo_5_HerramientasDatosI_2026.pdf res/
```

## Fuente de datos

Instituto Nacional de Estadística y Censos [INEC]. (2025). *Registro de Defunciones (REGDEF): Estadísticas Vitales de Costa Rica* [Conjunto de datos]. https://sistemas.inec.cr/nada5.4/index.php/catalog/REGDEF/
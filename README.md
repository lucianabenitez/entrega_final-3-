# Entrega Final — Ciencia de Datos para Economía y Negocios

**Facultad de Ciencias Económicas — UBA**

## Descripción

Este repositorio contiene el análisis empírico de los determinantes del desarrollo económico en una muestra de 12 países para los años 2005, 2015 y 2022.

La hipótesis principal sostiene que los países con mayores niveles de capital humano y una estructura económica más desarrollada tienden a presentar mayores niveles de PBI per cápita, mientras que una mayor desigualdad se asocia con menores niveles de desarrollo económico. Como hipótesis secundaria, se plantea que existen diferencias significativas en el PBI per cápita entre regiones del mundo.

## Países de la muestra

| Región                          | Países                        |
|---------------------------------|-------------------------------|
| Europa                          | Francia, Italia, España       |
| América Latina                  | Brasil, Colombia, Perú        |
| África Subsahariana             | Kenia, Sudáfrica              |
| Medio Oriente y Norte de África | Egipto, Israel, Irán          |
| Asia Oriental                   | China                         |

## Variables

| Variable                      | Rol         | Fuente                        | Código        |
|-------------------------------|-------------|-------------------------------|---------------|
| PBI per cápita (USD 2015)     | Dependiente | Banco Mundial (WDI)           | NY.GDP.PCAP.KD |
| Índice de Gini                | Explicativa | Banco Mundial (WDI)           | SI.POV.GINI   |
| Años esperados de escolaridad | Explicativa | Our World in Data / UNDP      | —             |
| Manufactura (% del PBI)       | Explicativa | Banco Mundial (WDI)           | NV.IND.MANF.ZS |
| Región                        | Control     | Elaboración propia            | —             |

**Nota sobre el Gini:** para Egipto (2005 y 2022) y Sudáfrica (2015) no existe dato exacto en el año objetivo; se utilizó el año disponible más cercano.

## Estructura del repositorio

```
entrega_final-3-/
│
├── raw/               → Bases de datos crudas descargadas de WDI y Our World in Data
├── input/             → Panel limpio listo para el análisis (panel_limpio.csv)
├── scripts/           → Scripts de R numerados por orden de ejecución
├── output/
│   ├── tablas/        → Tablas de resultados exportadas (.csv)
│   └── graficos/      → Visualizaciones generadas (.png)
├── auxiliar/          → Bases complementarias (si aplica)
├── utils/             → Funciones reutilizables entre scripts (si aplica)
└── README.md
```

## Cómo reproducir el análisis

1. Clonar el repositorio o descargar el zip.
2. Abrir RStudio y establecer el directorio de trabajo en la carpeta raíz del proyecto (`entrega_final-3-/`).
3. Correr los scripts en orden:

```
scripts/01_limpieza.R       → genera input/panel_limpio.csv
scripts/02_descriptivos.R   → estadísticas descriptivas y gráficos
scripts/03_test_t.R         → test t pareado (2005 vs. 2022)
scripts/04_anova.R          → ANOVA por región
scripts/05_regresion.R      → regresión múltiple
```

**Paquetes necesarios:** `tidyverse`, `janitor`, `scales`, `ggtext`, `rstatix`

## Participantes
### Nombres y apellidos
Luciana Benitez - 911.471
Francisco Guerra - 903.014
Nayla Pajello - 912.680

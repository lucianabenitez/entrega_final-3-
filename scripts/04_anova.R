# 04_anova.R
# ANOVA: ¿existen diferencias significativas en el PBI per cápita entre regiones?

library(tidyverse)
library(scales)

instub        <- "input"
outstub_tab   <- "output/tablas"
outstub_graf  <- "output/graficos"

dir.create(outstub_tab,  recursive = TRUE, showWarnings = FALSE)
dir.create(outstub_graf, recursive = TRUE, showWarnings = FALSE)

panel <- read_csv(file.path(instub, "panel_limpio.csv"),
                  show_col_types = FALSE)

-----------
# Planteo
-----------
# La hipótesis secundaria del trabajo sostiene que existen diferencias
# significativas en el PBI per cápita entre regiones del mundo.
#
# Comparamos más de dos grupos (Europa, América Latina, África Subsahariana,
# Medio Oriente y Norte de África, Asia Oriental), por lo que NO usamos t-test
# — hacerlo inflaría el error tipo I por múltiples comparaciones. Usamos
# ANOVA de un factor.
#
# H0: mu_Europa = mu_AméricaLatina = mu_África = mu_MedioOriente = mu_Asia
#     (todas las medias regionales son iguales)
# H1: al menos una media regional difiere del resto
#
# El estadístico F compara la varianza ENTRE grupos vs DENTRO de grupos.
# F grande -> los grupos son muy distintos entre sí -> tiende a rechazar H0.

# --------------------------
# Preparación de los datos
# --------------------------

# Usamos el último año disponible por país (2022) para una foto transversal
datos_anova <- panel |>
  filter(anio == 2022, !is.na(pbi_percapita), !is.na(region)) |>
  mutate(region = factor(region))

# =============================================================================
# Descriptivos por región (foto previa al test)
# =============================================================================

datos_anova |>
  group_by(region) |>
  summarise(
    n       = n(),
    media   = round(mean(pbi_percapita),   1),
    mediana = round(median(pbi_percapita), 1),
    sd      = round(sd(pbi_percapita),     1),
    min     = round(min(pbi_percapita),    1),
    max     = round(max(pbi_percapita),    1),
    .groups = "drop"
  ) |>
  arrange(desc(media))

# =============================================================================
# Supuestos
# =============================================================================

# ANOVA clásico asume:
#   1. Normalidad de los residuos (o de la variable dentro de cada grupo)
#   2. Homocedasticidad: varianzas iguales entre grupos
#
# Con n = 12 países (pocos por grupo), chequeamos ambos supuestos antes
# de decidir qué versión del test usar.

# --- Supuesto 1: normalidad dentro de cada grupo ----------------------------
# Shapiro-Wilk por región. Con grupos tan pequeños (2-3 obs) el test tiene
# poca potencia, pero lo reportamos igual.

datos_anova |>
  group_by(region) |>
  summarise(
    shapiro_p = tryCatch(
      shapiro.test(pbi_percapita)$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

# --- Supuesto 2: homocedasticidad -------------------------------------------
# Si los desvíos por grupo son muy distintos, el ANOVA clásico es menos
# confiable. La alternativa robusta es el ANOVA de Welch (oneway.test),
# que no asume varianzas iguales — igual que el t-test de Welch.

# Comparamos los desvíos por grupo (ya los vimos arriba en los descriptivos).
# Si el SD más grande es más del doble del más chico, conviene Welch.

# =============================================================================
# ANOVA clásico
# =============================================================================

modelo_anova <- aov(pbi_percapita ~ region, data = datos_anova)
summary(modelo_anova)

# Tabla resumen del ANOVA
f_val  <- summary(modelo_anova)[[1]]$"F value"[1]
p_val  <- summary(modelo_anova)[[1]]$"Pr(>F)"[1]
df_reg <- summary(modelo_anova)[[1]]$Df[1]
df_res <- summary(modelo_anova)[[1]]$Df[2]

tabla_anova <- tibble(
  Estadistico          = c("N observaciones", "N grupos (regiones)",
                           "GL entre grupos", "GL residuales",
                           "F", "P-valor", "Conclusion"),
  Valor                = c(nrow(datos_anova), nlevels(datos_anova$region),
                           df_reg, df_res,
                           round(f_val, 3), round(p_val, 4),
                           ifelse(p_val < 0.05,
                                  "Se rechaza H0 (diferencias significativas)",
                                  "No se rechaza H0 (sin diferencias significativas)"))
)

knitr::kable(tabla_anova, format = "simple")

# Interpretación del summary():
# - Df        : grados de libertad (entre grupos = k-1, residuales = n-k)
# - F value   : estadístico F
# - Pr(>F)    : p-valor. Si < 0.05 rechazamos H0 al 5%.
#
# Si rechazamos H0, sabemos que HAY diferencias, pero no ENTRE CUÁLES regiones.
# Para eso usamos comparaciones múltiples post-hoc.

# =============================================================================
# Comparaciones post-hoc: Tukey HSD
# =============================================================================
# Si rechazamos H0, necesitamos saber ENTRE QUÉ regiones hay diferencias.
# Usamos Tukey HSD (base R): controla el error tipo I familywise.
#
# Limitación: Asia Oriental tiene un solo país (China), por lo que las
# comparaciones que involucren esa región deben interpretarse con cautela.

posthoc <- TukeyHSD(modelo_anova)
print(posthoc)

# Cada fila muestra:
# - diff          : diferencia de medias entre los dos grupos
# - lwr / upr     : IC 95%
# - p adj         : p-valor ajustado por múltiples comparaciones
#
# Si p adj < 0.05 -> esas dos regiones difieren significativamente.


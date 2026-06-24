# =============================================================================
# 05_regresion.R
# Regresión lineal múltiple: desarrollo económico, capital humano y desigualdad
# =============================================================================

library(tidyverse)
library(broom)

instub  <- "input"
outstub <- "output"

panel <- read_csv(file.path(instub, "panel_limpio.csv"),
                  show_col_types = FALSE)
base_limpia <- panel |>
  filter(!is.na(pbi_percapita),
         !is.na(gini),
         !is.na(escolaridad),
         !is.na(manufactura_pct_pbi),
         !is.na(region)) |>
  mutate(
    log_pbi = log(pbi_percapita),
    indice_ch = as.numeric(scale(escolaridad)) + as.numeric(scale(manufactura_pct_pbi))
  )

# =============================================================================
# Planteo
# =============================================================================
# El objetivo de esta regresión es evaluar si el capital humano se asocia
# positivamente con el PBI per cápita, y si esa relación cambia según el nivel
# de desigualdad medido por el índice de Gini.
#
# Modelo:
# log(PBI per cápita) = b0 + b1*capital_humano + b2*Gini
#                    + b3*(capital_humano x Gini) + controles regionales + error
modelo_reg <- lm(
  log_pbi ~ indice_ch * gini + region,
  data = base_limpia
)

summary(modelo_reg)
# =============================================================================
# Resultados ordenados
# =============================================================================

# Armamos una tabla de coeficientes para que los resultados de la regresión
# queden más legibles.

tabla_regresion <- tidy(modelo_reg) |>
  mutate(
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 3),
    p.value = round(p.value, 4)
  )

tabla_regresion
write_csv2(tabla_regresion, file.path(outstub, "tabla_regresion.csv"))
# =============================================================================
# Calidad de ajuste del modelo
# =============================================================================

# Resumimos información general del modelo:
# - r.squared: proporción de la variación explicada por el modelo
# - adj.r.squared: R2 ajustado por cantidad de variables
# - statistic y p.value: test global del modelo

resumen_modelo <- glance(modelo_reg) |>
  mutate(
    r.squared = round(r.squared, 4),
    adj.r.squared = round(adj.r.squared, 4),
    statistic = round(statistic, 3),
    p.value = round(p.value, 4)
  )

resumen_modelo

write_csv2(resumen_modelo, file.path(outstub, "resumen_modelo_regresion.csv"))
# Interpretación: Un coeficiente positivo y significativo en indice_ch indica
# que mayor capital humano se asocia con mayor PBI per cápita. El término de
# interacción indice_ch:gini permite evaluar si ese efecto se debilita o se
# refuerza en contextos de mayor desigualdad.


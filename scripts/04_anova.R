# =============================================================================
# 04_anova.R
# ANOVA: ¿existen diferencias significativas en el PBI per cápita entre regiones?
# =============================================================================

library(tidyverse)
library(scales)
library(rstatix)

instub        <- "input"
outstub_tab   <- "output/tablas"
outstub_graf  <- "output/graficos"

dir.create(outstub_tab,  recursive = TRUE, showWarnings = FALSE)
dir.create(outstub_graf, recursive = TRUE, showWarnings = FALSE)

panel <- read_csv(file.path(instub, "panel_limpio.csv"),
                  show_col_types = FALSE)

# =============================================================================
# Planteo
# =============================================================================
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

# =============================================================================
# Preparación de los datos
# =============================================================================

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

# Interpretación del summary():
# - Df        : grados de libertad (entre grupos = k-1, residuales = n-k)
# - F value   : estadístico F
# - Pr(>F)    : p-valor. Si < 0.05 rechazamos H0 al 5%.
#
# Si rechazamos H0, sabemos que HAY diferencias, pero no ENTRE CUÁLES regiones.
# Para eso usamos comparaciones múltiples post-hoc.

# =============================================================================
# ANOVA de Welch (robusto a heterocedasticidad)
# =============================================================================

oneway.test(pbi_percapita ~ region, data = datos_anova)

# Conviene reportar el de Welch si los desvíos entre grupos son muy distintos.

# =============================================================================
# Comparaciones post-hoc: Games-Howell
# =============================================================================
# Si rechazamos H0, necesitamos saber ENTRE QUÉ regiones hay diferencias.
# Usamos Games-Howell (del paquete {rstatix}): es la versión robusta de Tukey
# para cuando no se cumple homocedasticidad. Controla el error tipo I
# familywise.

posthoc <- datos_anova |>
  games_howell_test(pbi_percapita ~ region)

posthoc

# Cada fila muestra:
# - group1, group2 : las dos regiones comparadas
# - estimate       : diferencia de medias
# - conf.low/high  : IC 95% ajustado
# - p.adj          : p-valor ajustado por múltiples comparaciones
#
# Si p.adj < 0.05 -> esas dos regiones difieren significativamente.

# =============================================================================
# Gráfico: PBI per cápita por región
# =============================================================================

g_anova <- ggplot(datos_anova,
                  aes(x = reorder(region, pbi_percapita, FUN = median),
                      y = pbi_percapita)) +
  geom_boxplot(aes(fill = region), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(colour = region), width = 0.15, size = 3, alpha = 0.8) +
  geom_text(aes(label = pais_nombre), hjust = -0.15, size = 2.8,
            colour = "#5b5b5b") +
  scale_y_continuous(labels = label_dollar(prefix = "USD ", big.mark = ".",
                                            decimal.mark = ",", accuracy = 1)) +
  scale_fill_brewer(palette  = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  coord_flip() +
  labs(
    title    = "El PBI per cápita difiere marcadamente entre regiones",
    subtitle = "Distribución del PBI per cápita por región. Año 2022. USD constantes de 2015.",
    caption  = "Fuente: Banco Mundial (WDI)",
    x = NULL, y = "PBI per cápita (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(colour = "#5b5b5b"),
    axis.title.x       = element_text(colour = "#5b5b5b", size = 10),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(colour = "#5b5b5b", size = 11),
    plot.caption       = element_text(colour = "#8a8a8a", size = 9, hjust = 0)
  )

print(g_anova)
ggsave(file.path(outstub_graf, "grafico_anova.png"), g_anova,
       width = 10, height = 6, dpi = 300, bg = "white")

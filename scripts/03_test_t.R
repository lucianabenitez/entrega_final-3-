# =============================================================================
# 03_test_t.R
# Test t pareado: ¿creció significativamente el PBI per cápita entre 2005 y 2022?
# =============================================================================

library(tidyverse)
library(scales)

instub        <- "input"
outstub_tab   <- "output/tablas"
outstub_graf  <- "output/graficos"

dir.create(outstub_tab,  recursive = TRUE, showWarnings = FALSE)
dir.create(outstub_graf, recursive = TRUE, showWarnings = FALSE)

panel <- read_csv(file.path(instub, "panel_limpio.csv"),
                  show_col_types = FALSE)
# ------------
# Planteo
# ------------
# El desarrollo económico de un país está asociado a los niveles de 
# capital humano y estructura productiva. Como paso previo a la regresión, 
# testeamos si el PBI per cápita de los 12 países de la muestra aumentó de
# forma significativa entre 2005 y 2022.
#
# Usamos un test PAREADO porque cada país aparece en cada año y los datos 
# entonces están relacionados entre si. 
# El test pareado equivale a un t de una muestra sobre las diferencias
# d_i = PBI_2022 - PBI_2005.
#
# Hipotesis del test t 
# H0: media de las diferencias = 0  (no hubo cambio significativo)
# H1: media de las diferencias > 0  (hubo crecimiento significativo)
#
# Usamos H1 unilateral (mayor que) porque la hipótesis de trabajo predice
# crecimiento, no simplemente cambio.

# --------------------------
# Preparación de los datos
# --------------------------

# Armamos la base ancha: una fila por país, columnas para cada año
datos_test <- panel |>
  filter(anio %in% c(2005, 2022), !is.na(pbi_percapita)) |>
  select(pais_codigo, pais_nombre, anio, pbi_percapita) |>
  pivot_wider(names_from  = anio,
              values_from = pbi_percapita,
              names_prefix = "pbi_") |>
  filter(!is.na(pbi_2005), !is.na(pbi_2022)) |>
  mutate(diferencia = pbi_2022 - pbi_2005)

# ------------------------------
# Descriptivos de la diferencia
# ------------------------------

# Es sobre la diferencia que corre el test pareado
datos_test |>
  summarise(
    n          = n(),
    media_diff = round(mean(diferencia),   1),
    sd_diff    = round(sd(diferencia),     1),
    min_diff   = round(min(diferencia),    1),
    max_diff   = round(max(diferencia),    1)
  )

# Medias por año para contextualizar
datos_test |>
  summarise(
    media_2005 = round(mean(pbi_2005), 1),
    media_2022 = round(mean(pbi_2022), 1)
  )

# ------------
# Supuestos
# ------------

# El test t pareado asume que las DIFERENCIAS siguen una distribución
# aproximadamente normal. Con n = 12 el TCL no alcanza, así que lo chequeamos
# con el test de Shapiro-Wilk.
#
# H0: las diferencias siguen una distribución normal
# H1: no siguen una distribución normal

shapiro.test(datos_test$diferencia)

# Si p > 0.05: no rechazamos normalidad -> supuesto compatible con los datos.
# Si p < 0.05: evidencia en contra de normalidad. En ese caso el test t es
# menos confiable; una alternativa no paramétrica sería el test de Wilcoxon
# de rangos con signo (wilcox.test(..., paired = TRUE)).

# ----------------
# Test t pareado
# ----------------

# Forma (a): pasando los dos vectores con paired = TRUE
t_pareado <- t.test(datos_test$pbi_2022, datos_test$pbi_2005,
                    paired      = TRUE,
                    alternative = "greater")  # H1 unilateral: 2022 > 2005
t_pareado

# Forma (b): equivalente — t de una muestra sobre las diferencias
t.test(datos_test$diferencia, mu = 0, alternative = "greater")

# Interpretación de los elementos clave:
# - mean of the differences: cambio promedio en PBI per cápita (USD 2015)
# - t, df                  : estadístico y grados de libertad (n-1 = 11)
# - p-value                : prob. de observar un cambio al menos tan grande
#                            si H0 (sin cambio) fuese cierta
# - conf.int               : IC 95% para el cambio promedio
#
# Regla de decisión: si p-value < 0.05 rechazamos H0 al 5%.

# =============================================================================
# Gráfico: PBI per cápita 2005 vs 2022 por país
# =============================================================================

datos_grafico <- datos_test |>
  pivot_longer(cols = c(pbi_2005, pbi_2022),
               names_to  = "anio",
               values_to = "pbi") |>
  mutate(anio = recode(anio, "pbi_2005" = "2005", "pbi_2022" = "2022"))

g_test <- ggplot(datos_grafico,
                 aes(x = anio, y = pbi, group = pais_nombre)) +
  geom_line(colour = "#C9C9C9", linewidth = 0.7) +
  geom_point(aes(colour = anio), size = 3) +
  geom_text(data = datos_grafico |> filter(anio == "2022"),
            aes(label = pais_nombre), hjust = -0.1, size = 2.8,
            colour = "#5b5b5b") +
  scale_y_continuous(labels = label_dollar(prefix = "USD ", big.mark = ".",
                                            decimal.mark = ",", accuracy = 1)) +
  scale_colour_manual(values = c("2005" = "#C9C9C9", "2022" = "#4C6A9C")) +
  labs(
    title    = "El PBI per cápita aumentó en la mayoría de los países entre 2005 y 2022",
    subtitle = "Cada línea conecta el valor de un mismo país en 2005 y 2022. USD constantes de 2015.",
    caption  = "Fuente: Banco Mundial (WDI)",
    x = NULL, y = "PBI per cápita (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(colour = "#5b5b5b"),
    axis.title.y       = element_text(colour = "#5b5b5b", size = 10),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(colour = "#5b5b5b", size = 11),
    plot.caption       = element_text(colour = "#8a8a8a", size = 9, hjust = 0)
  )

print(g_test)
ggsave(file.path(outstub_graf, "grafico_test_t.png"), g_test,
       width = 10, height = 6, dpi = 300, bg = "white")


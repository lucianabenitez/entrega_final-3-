
library(WDI)
library(tidyverse)
library(ggtext)
library(scales)
instub <- "input"
outstub <- "output"

dir.create(outstub, showWarnings = FALSE)

# --- Paleta y tema estilo Our World in Data ----------------------------------
owid_azul  <- "#4C6A9C"
owid_rojo  <- "#B13507"
owid_verde <- "#578145"
owid_gris  <- "#C9C9C9"

theme_owid <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(face = "bold", size = rel(1.25),
                                       colour = "#1d1d1d", lineheight = 1.2,
                                       margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = rel(0.95), colour = "#5b5b5b",
                                       margin = margin(b = 16)),
      plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                       colour = "#8a8a8a", margin = margin(t = 14)),
      axis.title    = element_blank(),
      axis.text     = element_text(colour = "#5b5b5b"),
      axis.ticks    = element_blank(),
      panel.grid.major.y = element_line(colour = "#e6e6e6", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "none",
      plot.margin = margin(t = 14, r = 24, b = 10, l = 16)
    )
}

# --- Datos -------------------------------------------------------------------
panel <- read_csv(file.path(instub, "panel_limpio.csv"),
                  show_col_types = FALSE)

# Último año disponible por país para el gráfico
datos_pais <- panel |>
  filter(!is.na(pbi_percapita),
         !is.na(gini)) |>
  group_by(pais_codigo, pais_nombre, region) |>
  slice_max(order_by = anio, n = 1, with_ties = FALSE) |>
  ungroup()

# =============================================================================
# Tabla general de estadísticas descriptivas
# =============================================================================

tabla_general <- datos_pais |>
  select(pbi_percapita, gini, escolaridad, manufactura_pct_pbi) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) |>
  group_by(variable) |>
  summarise(
    media   = round(mean(valor, na.rm = TRUE), 2),
    mediana = round(median(valor, na.rm = TRUE), 2),
    sd      = round(sd(valor, na.rm = TRUE), 2),
    minimo  = round(min(valor, na.rm = TRUE), 2),
    maximo  = round(max(valor, na.rm = TRUE), 2),
    .groups = "drop"
  )

write_csv(tabla_general, file.path(outstub, "tabla_general.csv"))


# =============================================================================
# Tabla de estadísticas descriptivas por región
# =============================================================================

tabla_region <- datos_pais |>
  group_by(region) |>
  summarise(
    n = n(),
    media_pbi       = round(mean(pbi_percapita, na.rm = TRUE), 2),
    mediana_pbi     = round(median(pbi_percapita, na.rm = TRUE), 2),
    sd_pbi          = round(sd(pbi_percapita, na.rm = TRUE), 2),
    media_gini      = round(mean(gini, na.rm = TRUE), 2),
    mediana_gini    = round(median(gini, na.rm = TRUE), 2),
    media_educacion = round(mean(escolaridad, na.rm = TRUE), 2),
    media_industria = round(mean(manufactura_pct_pbi, na.rm = TRUE), 2),
    .groups = "drop"
  )

write_csv(tabla_region, file.path(outstub, "tabla_region.csv"))

# --- Gráfico -----------------------------------------------------------------
max_gini <- datos_pais |> slice_max(gini, n = 1, with_ties = FALSE)

g_gini <- ggplot(datos_pais, aes(x = gini, y = pbi_percapita)) +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#1d1d1d", fill = "#c9c9c9",
              linewidth = 0.8, alpha = 0.2) +
  geom_point(aes(colour = region), size = 3.5, alpha = 0.9) +
  geom_text(aes(label = pais_nombre), hjust = -0.15, size = 2.8, colour = "#5b5b5b") +
  annotate("text",
           x = max_gini$gini - 2, y = max_gini$pbi_percapita * 1.2,
           label = "Mayor desigualdad\nde la muestra",
           hjust = 1, size = 2.8, colour = owid_rojo, lineheight = 0.9) +
  annotate("curve",
           x = max_gini$gini - 1.8, y = max_gini$pbi_percapita * 1.15,
           xend = max_gini$gini - 0.3, yend = max_gini$pbi_percapita,
           curvature = -0.2, linewidth = 0.4, colour = owid_rojo,
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  scale_y_log10(
    labels = label_dollar(prefix = "USD ",
                          big.mark = ".",
                          decimal.mark = ",",
                          accuracy = 1)
  ) +
  scale_colour_brewer(palette = "Set1", name = "Región") +
  labs(
    title = "Los países de mayor ingreso presentan menor desigualdad en la muestra",
    subtitle = "PBI per cápita en USD constantes de 2015, escala logarítmica. Último año disponible por país.",
    caption  = "Fuente: Banco Mundial (WDI)",
    x        = "Índice de Gini",
    y        = "PBI per cápita (USD)"
  ) +
  theme_owid() +
  theme(
    legend.position    = "right",
    legend.title       = element_text(size = 9, colour = "#5b5b5b"),
    legend.text        = element_text(size = 8, colour = "#5b5b5b"),
    panel.grid.major.y = element_line(colour = "#e6e6e6", linewidth = 0.4),
    panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.4),
    axis.title         = element_text(colour = "#5b5b5b", size = 10)
  )

print(g_gini)
ggsave(file.path(outstub, "grafico_gini_pbi.png"), g_gini,
       width = 10, height = 6, dpi = 300, bg = "white")

# =============================================================================
# Gráfico 1: Desarrollo económico y desigualdad (connected scatter plot)
# =============================================================================

datos_g1 <- panel |>
  filter(anio %in% c(2005, 2015, 2022),
         !is.na(pbi_percapita),
         !is.na(gini)) |>
  arrange(pais_nombre, anio)

etiquetas_finales <- datos_g1 |>
  group_by(pais_codigo, pais_nombre, region) |>
  slice_max(order_by = anio, n = 1, with_ties = FALSE) |>
  ungroup()

g_gini <- ggplot(
  datos_g1,
  aes(x = pbi_percapita, y = gini,
      group = pais_nombre, colour = region)
) +
  geom_line(alpha = 0.6, linewidth = 0.7) +
  geom_point(aes(shape = factor(anio)), size = 2.5, alpha = 0.9) +
  geom_text(
    data = etiquetas_finales,
    aes(label = pais_nombre),
    hjust = -0.1, size = 2.7, colour = "#5b5b5b",
    show.legend = FALSE
  ) +
  scale_x_log10(
    labels = label_dollar(
      prefix = "USD ",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_colour_brewer(palette = "Set1", name = "Región") +
  scale_shape_discrete(name = "Año") +
  labs(
    title = "A mayor desarrollo, la desigualdad tiende a concentrarse en niveles más bajos",
    subtitle = "Cada línea conecta la trayectoria de un país entre 2005, 2015 y 2022. PBI per cápita en USD constantes de 2015, escala logarítmica.",
    caption = "Fuente: Banco Mundial (WDI)",
    x = "PBI per cápita",
    y = "Índice de Gini"
  ) +
  theme_owid() +
  theme(
    legend.position    = "right",
    legend.title       = element_text(size = 9, colour = "#5b5b5b"),
    legend.text        = element_text(size = 8, colour = "#5b5b5b"),
    panel.grid.major.y = element_line(colour = "#e6e6e6", linewidth = 0.4),
    panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.4),
    axis.title         = element_text(colour = "#5b5b5b", size = 10)
  ) +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 1, size = 3)),
    shape  = guide_legend(order = 2)
  )

print(g_gini)

ggsave(file.path(outstub, "grafico_gini_pbi.png"), g_gini,
       width = 10, height = 6, dpi = 300, bg = "white")
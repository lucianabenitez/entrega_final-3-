# 01_limpieza.R
# Limpieza y unificación de las bases de datos

# 0. Setup 

library(tidyverse)
library(janitor)

instub  <- "raw"
outstub <- "input"

# Códigos ISO de los 12 países de la muestra
paises <- c("FRA", "ITA", "ESP", "EGY", "ISR", "CHN",
            "IRN", "KEN", "PER", "BRA", "COL", "ZAF")

# Años seleccionados para realizar el análisis
anios_objetivo <- c(2005, 2015, 2022)

# 1. PBI per cápita (constante 2015 USD)

pbi_raw <- read_csv(file.path(instub, "pbi_percapita_data.csv"),
                    na = "..",
                    show_col_types = FALSE)

pbi <- pbi_raw %>%
  # Eliminar filas basura del final (filas sin Country Name)
  filter(!is.na(`Country Name`)) %>%
  # Eliminar columnas de serie (no son variables de análisis)
  select(-`Series Name`, -`Series Code`) %>%
  # Estandarizar nombres de columnas
  rename(pais_codigo = `Country Code`,
         pais_nombre = `Country Name`) %>%
  # Limpiar nombres de columnas de años: "2005 [YR2005]" -> "2005"
  rename_with(~ str_replace(., " \\[YR\\d{4}\\]", "")) %>%
  # Pivot para pasar de formato ancho a largo
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to  = "anio",
               values_to = "pbi_percapita") %>%
  mutate(anio = as.integer(anio)) %>%
  # Filtrar países y años de interés
  filter(pais_codigo %in% paises,
         anio %in% anios_objetivo)

# --- 2. Manufactura (% del PBI) ----------------------------------------------

manufactura_raw <- read_csv(file.path(instub, "manufactura_data.csv"),
                            na = "..",
                            show_col_types = FALSE)

manufactura <- manufactura_raw %>%
  filter(!is.na(`Country Name`)) %>%
  select(-`Series Name`, -`Series Code`) %>%
  rename(pais_codigo = `Country Code`,
         pais_nombre = `Country Name`) %>%
  rename_with(~ str_replace(., " \\[YR\\d{4}\\]", "")) %>%
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to  = "anio",
               values_to = "manufactura_pct_pbi") %>%
  mutate(anio = as.integer(anio)) %>%
  filter(pais_codigo %in% paises,
         anio %in% anios_objetivo)

# --- 3. Índice de Gini -------------------------------------------------------

# Nota: el Gini tiene columnas en orden distinto (Series Name primero).
# Además, algunos países no tienen dato exacto en el año objetivo:
# Egipto: se usa 2004 en lugar de 2005, y 2021 en lugar de 2022
# Sudáfrica: se usa 2014 en lugar de 2015
# Se selecciona el año con dato disponible más cercano.

gini_raw <- read_csv(file.path(instub, "gini_data.csv"),
                     na = "..",
                     show_col_types = FALSE)

gini_long <- gini_raw %>%
  filter(!is.na(`Country Name`), `Country Name` != "") %>%
  select(`Country Code`, `Country Name`,
         matches("^\\d{4}")) %>%
  rename(pais_codigo = `Country Code`,
         pais_nombre = `Country Name`) %>%
  rename_with(~ str_replace(., " \\[YR\\d{4}\\]", "")) %>%
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to  = "anio",
               values_to = "gini") %>%
  mutate(anio = as.integer(anio)) %>%
  filter(pais_codigo %in% paises, !is.na(gini))

# Para cada país y año objetivo, seleccionar el dato del año más cercano
gini <- map_dfr(anios_objetivo, function(yr) {
  gini_long %>%
    mutate(distancia = abs(anio - yr)) %>%
    group_by(pais_codigo) %>%
    slice_min(distancia, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(anio_objetivo  = yr,
           anio_real_gini = anio) %>%
    select(pais_codigo, pais_nombre,
           anio = anio_objetivo, anio_real_gini, gini)
})

# --- 4. Escolaridad esperada (OWD / UNDP) ------------------------------------

escolaridad <- read_csv(
  file.path(instub, "escolaridad_data.csv"),
  show_col_types = FALSE) %>%
  rename(pais_nombre  = Entity,
         pais_codigo  = Code,
         anio         = Year,
         escolaridad  = `Both genders`) %>%
  filter(pais_codigo %in% paises,
         anio %in% anios_objetivo)

# --- 5. Unir las cuatro bases en un panel ------------------------------------

panel <- pbi %>%
  select(pais_codigo, pais_nombre, anio, pbi_percapita) %>%
  left_join(
    manufactura %>% select(pais_codigo, anio, manufactura_pct_pbi),
    by = c("pais_codigo", "anio")
  ) %>%
  left_join(
    gini %>% select(pais_codigo, anio, anio_real_gini, gini),
    by = c("pais_codigo", "anio")
  ) %>%
  left_join(
    escolaridad %>% select(pais_codigo, anio, escolaridad),
    by = c("pais_codigo", "anio")
  )

# --- 6. Agregar variable de región -------------------------------------------

regiones <- tibble(
  pais_codigo = c("FRA", "ITA", "ESP",
                  "EGY", "ISR", "IRN",
                  "KEN", "ZAF",
                  "BRA", "COL", "PER",
                  "CHN"),
  region = c("Europa", "Europa", "Europa",
             "Medio Oriente y Norte de Africa",
             "Medio Oriente y Norte de Africa",
             "Medio Oriente y Norte de Africa",
             "Africa Subsahariana", "Africa Subsahariana",
             "America Latina", "America Latina", "America Latina",
             "Asia Oriental")
)

panel <- panel %>%
  left_join(regiones, by = "pais_codigo") %>%
  arrange(pais_codigo, anio)

# 7. Verificación rápida 

cat("Dimensiones del panel:", nrow(panel), "filas x", ncol(panel), "columnas\n")
cat("Paises:", n_distinct(panel$pais_codigo), "\n")
cat("Anios:", paste(sort(unique(panel$anio)), collapse = ", "), "\n")
cat("Valores faltantes por variable:\n")
print(colSums(is.na(panel)))

#  8. Guardar 

write_csv(panel, file.path(outstub, "panel_limpio.csv"))

cat("\npanel_limpio.csv guardado en input/\n")

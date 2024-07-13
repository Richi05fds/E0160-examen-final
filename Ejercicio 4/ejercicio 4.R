library(tidyverse)
library(readr)
library(rio)

polizas <- read.csv("base_polizas.csv")

valores_negativos <- polizas %>%
  filter(id_poliza < 0)

registros_no_encontrados <- polizas %>%
  filter(is.na(nombre_cliente) |
           is.na(fecha_fin) |
           is.na(monto_cobertura) |
           is.na(estado))

is_valid_date <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}

fecha_incorrectas <- polizas %>%
  filter(!is_valid_date(fecha_inicio) | !is_valid_date(fecha_fin))

polizas_fecha_mal <- polizas %>%
  filter(fecha_inicio >= fecha_fin | is.na(fecha_fin))

polizas <- polizas %>%
  filter(fecha_fin >= fecha_inicio | is.na(fecha_fin))

polizas <- polizas %>%
  mutate(
    fecha_inicio = as.Date(fecha_inicio, format = "%Y-%m-%d"),
    fecha_fin = as.Date(fecha_fin, format = "%Y-%m-%d"),
    antiguedad = ifelse(is.na(fecha_fin), NA, as.integer(difftime(fecha_fin, fecha_inicio, units = "days") / 365))
  )

polizas_activas <- polizas

polizas_activas <- polizas_activas %>%
  mutate(
    fecha_fin = ifelse(estado == "Activa", NA, fecha_fin)
  )

polizas_solactivas <- polizas_activas %>%
  filter(!is.na(fecha_fin) & fecha_fin >= fecha_inicio & estado == "Activa")

valores_incoherentes <- bind_rows(
  valores_negativos,
  registros_no_encontrados,
  fecha_incorrectas,
  polizas_fecha_mal
)

print(valores_incoherentes)

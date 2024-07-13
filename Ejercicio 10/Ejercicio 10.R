library(tidyverse)	
library(rio)

base_registros_medicos <- import("base_registros_medicos.csv")

negativos_id_registro <- base_registros_medicos %>%
  filter(id_registro < 0)
base_registros_medicos<- base_registros_medicos %>%
  filter(id_registro >= 0)

is_valid_date <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}

base_registros_medicos <- base_registros_medicos %>%
  filter(is_valid_date(fecha_admision) & (is.na(fecha_alta) | is_valid_date(fecha_alta))) %>%
  mutate(
    fecha_admision = as.Date(fecha_admision),
    fecha_alta = as.Date(fecha_alta)
  )

id_clientes_no_registrados <- base_registros_medicos %>%
  filter(id_paciente < 0 & !is.na(id_paciente))

base_registros_medicos <- base_registros_medicos %>%
  filter(id_paciente >= 0 & !is.na(id_paciente))

costo_no_registrados <- base_registros_medicos %>%
  filter(is.na(costo))

base_registros_medicos <- base_registros_medicos %>%
  filter(!is.na(costo))

diagnostico_no_registrados <- base_registros_medicos %>%
  filter(is.na(diagnostico))

base_registros_medicos <- base_registros_medicos %>%
  filter(!is.na(diagnostico))

estado_no_registrados <- base_registros_medicos %>%
  filter(is.na(estado))

base_registros_medicos <- base_registros_medicos %>%
  filter(!is.na(estado))

#Se filtra a los pacientes dados de alta
pacientes_alta <- base_registros_medicos %>%
  filter(estado == "Alta")
print(pacientes_alta)

#se calcula los días de estadia de cada paciente
base_registros_medicos <- base_registros_medicos %>%
  mutate(
    fecha_admision = as.Date(fecha_admision),
    fecha_alta = as.Date(fecha_alta),
    diferencia_dias = as.numeric(fecha_alta - fecha_admision)
  )

base_registros_medicos <- base_registros_medicos %>%
  filter(diferencia_dias >= 0)
print(base_registros_medicos)

visitas_paciente <- base_registros_medicos %>%
  count(id_paciente)
print(visitas_paciente)

costo_cliente <- aggregate(costo ~ id_paciente, data = base_registros_medicos, FUN = sum)

print(costo_cliente)

tabla_clientes <- merge(costo_cliente, visitas_paciente, by = "id_paciente", all = TRUE)
print(tabla_clientes)

tabla_analisis <- merge(tabla_clientes, base_registros_medicos[, c("id_paciente", "diferencia_dias")], by = "id_paciente", all.x = TRUE)
print(tabla_analisis)


valores_incoherentes <- bind_rows(
  fechas_invalidas,
  na_estado_transaccion,
  na_tipo_transaccion,
)

print(valores_incoherentes)
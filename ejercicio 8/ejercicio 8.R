library(tidyverse)
library(rio)

base_transacciones <- import("base_transacciones.csv")

is_valid_date <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}

invalid_dates <- base_transacciones %>%
  filter(!is_valid_date(fecha_transaccion))

base_transacciones <- base_transacciones %>%
  filter(is_valid_date(fecha_transaccion))

print(base_transacciones)

base_transacciones <- base_transacciones %>%
  filter(id_transaccion >= 0, id_cliente >= 0, monto >= 0)
print(base_transacciones)

na_tipo_transaccion <- base_transacciones %>%
  filter(is.na(tipo_transaccion))

na_estado_transaccion <- base_transacciones %>%
  filter(is.na(estado_transaccion))

base_transacciones <- base_transacciones %>%
  filter(!is.na(tipo_transaccion), !is.na(estado_transaccion))

print(base_transacciones)

cliente_transacciones <- base_transacciones %>%
  group_by(id_cliente)%>%
  summarise(conteo = n())

#como piden hallar la cantidad de dias desde la ultima transaccion
#se deberá eliminar los clientes con solamente una transaccion

cliente_transacciones <- cliente_transacciones %>%
  filter(conteo > 1)
print(cliente_transacciones)

#Mostramos las transacciones completadas
transacciones_completadas <- base_transacciones %>%
  filter(estado_transaccion == "Completada")
print(transacciones_completadas)

#Asumimos:
#los valores maximos encontrados para retiros es $3000
#los valores maximos encontrados para transferencias es $5000
#los valores maximos encontrados para depositos es $3000
transacciones_anormales <- base_transacciones %>%
  filter((tipo_transaccion == "Retiro" & monto > 3000) |
           (tipo_transaccion == "Transferencia" & monto > 5000) |
           (tipo_transaccion == "Deposito"& monto > 3000))

print(transacciones_anormales)

valores_incoherentes <- bind_rows(
  invalid_dates,
  na_estado_transaccion,
  na_tipo_transaccion,
)

print(valores_incoherentes)
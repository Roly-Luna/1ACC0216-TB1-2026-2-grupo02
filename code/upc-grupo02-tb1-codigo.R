# ------------------------------------------------------------
# fundamentos de data science
# tb1 - hotel booking demand
# grupo 2
# ------------------------------------------------------------


# ------------------------------------------------------------
# configuracion inicial
# ------------------------------------------------------------

rm(list = ls())
graphics.off()
cat("\014")

# instalar solo una vez
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)

# cargar librerias
library(tidyverse)
library(ggplot2)

# cambiar ruta si es necesario
# setwd("~/Desktop/FDS/TB1")


# ------------------------------------------------------------
# cargar dataset
# ------------------------------------------------------------

# cargar datos
hotel_original <- read.table(
  "data/hotel_bookings_original.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  na.strings = c("NA", "NULL")
)

# crear copia de trabajo
hotel <- hotel_original


# ------------------------------------------------------------
# 3.2 estructura inicial del dataset
# ------------------------------------------------------------

# inspeccion inicial
head(hotel)
dim(hotel)
names(hotel)
str(hotel)


# ------------------------------------------------------------
# 3.2.1 numero de filas y columnas
# ------------------------------------------------------------

dim(hotel)

nrow(hotel)
ncol(hotel)


# ------------------------------------------------------------
# 3.2.2 nombres de las variables
# ------------------------------------------------------------

names(hotel)


# ------------------------------------------------------------
# 3.2.3 estructura y tipos iniciales
# ------------------------------------------------------------

str(hotel)


# ------------------------------------------------------------
# 3.3 muestra de registros
# ------------------------------------------------------------

# primeras seis filas
head(hotel)


# ------------------------------------------------------------
# 3.6 conversiones necesarias
# ------------------------------------------------------------

# convertir variables
hotel <- hotel |>
  mutate(
    hotel = factor(hotel),
    
    arrival_date_month = factor(
      arrival_date_month,
      levels = month.name,
      ordered = TRUE
    ),
    
    meal = factor(meal),
    country = factor(country),
    market_segment = factor(market_segment),
    distribution_channel = factor(distribution_channel),
    reserved_room_type = factor(reserved_room_type),
    assigned_room_type = factor(assigned_room_type),
    deposit_type = factor(deposit_type),
    agent = factor(agent),
    company = factor(company),
    customer_type = factor(customer_type),
    reservation_status = factor(reservation_status),
    
    reservation_status_date =
      as.Date(reservation_status_date)
  )

# crear numero de mes
mes_num <- match(
  as.character(hotel$arrival_date_month),
  month.name
)

# crear fecha de llegada
hotel$fecha_llegada <- as.Date(
  sprintf(
    "%04d-%02d-%02d",
    hotel$arrival_date_year,
    mes_num,
    hotel$arrival_date_day_of_month
  )
)

# verificar conversiones
str(hotel)


# ------------------------------------------------------------
# 4.1.1 completitud
# ------------------------------------------------------------

# valores faltantes
faltantes <- data.frame(
  variable = names(hotel),
  cantidad = colSums(is.na(hotel)),
  porcentaje = round(
    colMeans(is.na(hotel)) * 100,
    3
  )
)

# mostrar faltantes
faltantes |>
  filter(cantidad > 0)


# ------------------------------------------------------------
# 4.1.2 unicidad
# ------------------------------------------------------------

# contar repetidos
sum(duplicated(hotel_original))

# mostrar repetidos
hotel_original[
  duplicated(hotel_original),
] |>
  head()


# ------------------------------------------------------------
# 4.1.3 consistencia
# ------------------------------------------------------------

# revisar cancelacion y estado
table(
  hotel$is_canceled,
  hotel$reservation_status
)

# revisar categorias
unique(hotel$hotel)
unique(hotel$market_segment)
unique(hotel$distribution_channel)
unique(hotel$deposit_type)
unique(hotel$customer_type)
unique(hotel$reservation_status)


# ------------------------------------------------------------
# 4.1.4 validez
# ------------------------------------------------------------

# reservas sin huespedes
sin_huespedes <- hotel |>
  filter(
    adults == 0,
    !is.na(children),
    children == 0,
    babies == 0
  )

# contar casos
nrow(sin_huespedes)

# mostrar algunos casos
sin_huespedes |>
  select(
    hotel,
    adults,
    children,
    babies,
    reservation_status
  ) |>
  head()

# adr negativo
adr_negativo <- hotel |>
  filter(adr < 0)

# mostrar caso
adr_negativo |>
  select(
    hotel,
    adr,
    reservation_status,
    fecha_llegada
  )


# ------------------------------------------------------------
# 4.3.4 correccion de valores invalidos
# ------------------------------------------------------------

# excluir reservas sin huespedes
hotel_preparado <- hotel |>
  filter(
    !(
      adults == 0 &
        !is.na(children) &
        children == 0 &
        babies == 0
    )
  )


# ------------------------------------------------------------
# 4.3.6 creacion de nuevas variables
# ------------------------------------------------------------

# crear variables
hotel_preparado <- hotel_preparado |>
  mutate(
    duracion_total =
      stays_in_weekend_nights +
      stays_in_week_nights,
    
    incluye_menores = case_when(
      children > 0 | babies > 0 ~ "Con menores",
      
      children == 0 &
        babies == 0 ~ "Sin menores",
      
      TRUE ~ NA_character_
    ),
    
    incluye_menores =
      factor(incluye_menores)
  )

# verificar variables
hotel_preparado |>
  select(
    hotel,
    children,
    babies,
    duracion_total,
    incluye_menores
  ) |>
  head()


# ------------------------------------------------------------
# 4.4 dataset preparado
# ------------------------------------------------------------

# comparar dimensiones
dim(hotel_original)
dim(hotel_preparado)

# guardar dataset preparado
write.csv(
  hotel_preparado,
  "data/hotel_bookings_preparado.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 5.1.1 variables categoricas
# ------------------------------------------------------------

# reservas por hotel
tabla_hotel <- hotel_preparado |>
  count(hotel) |>
  mutate(
    porcentaje =
      round(
        n / sum(n) * 100,
        2
      )
  )

tabla_hotel


# cancelaciones
tabla_cancelacion <- hotel_preparado |>
  count(is_canceled) |>
  mutate(
    porcentaje =
      round(
        n / sum(n) * 100,
        2
      )
  )

tabla_cancelacion


# reservas con menores
tabla_menores <- hotel_preparado |>
  filter(
    !is.na(incluye_menores)
  ) |>
  count(incluye_menores) |>
  mutate(
    porcentaje =
      round(
        n / sum(n) * 100,
        2
      )
  )

tabla_menores


# grafico de reservas por hotel
grafico_hotel <- ggplot(
  tabla_hotel,
  aes(
    x = hotel,
    y = n
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  labs(
    title = "Reservas por tipo de hotel",
    x = "Tipo de hotel",
    y = "Numero de reservas"
  ) +
  theme_minimal()

grafico_hotel


# ------------------------------------------------------------
# 5.1.2 variables cuantitativas
# ------------------------------------------------------------

# resumen numerico
resumen_numerico <- tibble(
  variable = c(
    "lead_time",
    "adr",
    "duracion_total"
  ),
  
  media = c(
    mean(hotel_preparado$lead_time),
    mean(hotel_preparado$adr),
    mean(hotel_preparado$duracion_total)
  ),
  
  mediana = c(
    median(hotel_preparado$lead_time),
    median(hotel_preparado$adr),
    median(hotel_preparado$duracion_total)
  ),
  
  desviacion = c(
    sd(hotel_preparado$lead_time),
    sd(hotel_preparado$adr),
    sd(hotel_preparado$duracion_total)
  ),
  
  minimo = c(
    min(hotel_preparado$lead_time),
    min(hotel_preparado$adr),
    min(hotel_preparado$duracion_total)
  ),
  
  maximo = c(
    max(hotel_preparado$lead_time),
    max(hotel_preparado$adr),
    max(hotel_preparado$duracion_total)
  )
) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

resumen_numerico


# ------------------------------------------------------------
# 5.2 analisis de distribuciones
# ------------------------------------------------------------

# distribucion de duracion
grafico_duracion <- ggplot(
  hotel_preparado,
  aes(x = duracion_total)
) +
  geom_histogram(
    binwidth = 1,
    fill = "lightblue",
    color = "white"
  ) +
  labs(
    title = "Distribucion de la duracion total",
    x = "Noches",
    y = "Numero de reservas"
  ) +
  theme_minimal()

grafico_duracion


# distribucion de anticipacion
grafico_lead <- ggplot(
  hotel_preparado,
  aes(x = lead_time)
) +
  geom_histogram(
    binwidth = 30,
    fill = "lightblue",
    color = "white"
  ) +
  labs(
    title = "Distribucion de la anticipacion",
    x = "Dias de anticipacion",
    y = "Numero de reservas"
  ) +
  theme_minimal()

grafico_lead


# ------------------------------------------------------------
# 5.3 duracion total
# ------------------------------------------------------------

# calcular cuartiles
Q1_duracion <- quantile(
  hotel_preparado$duracion_total,
  0.25
)

Q3_duracion <- quantile(
  hotel_preparado$duracion_total,
  0.75
)

# calcular ric
RIC_duracion <-
  Q3_duracion - Q1_duracion

# calcular limites
limite_inf_duracion <-
  Q1_duracion - 1.5 * RIC_duracion

limite_sup_duracion <-
  Q3_duracion + 1.5 * RIC_duracion

# contar outliers
cantidad_outliers_duracion <- sum(
  hotel_preparado$duracion_total <
    limite_inf_duracion |
    hotel_preparado$duracion_total >
    limite_sup_duracion
)

# mostrar resultados
Q1_duracion
Q3_duracion
RIC_duracion
limite_inf_duracion
limite_sup_duracion
cantidad_outliers_duracion


# boxplot de duracion
grafico_box_duracion <- ggplot(
  hotel_preparado,
  aes(y = duracion_total)
) +
  geom_boxplot(
    fill = "lightblue"
  ) +
  labs(
    title = "Boxplot de la duracion total",
    x = NULL,
    y = "Noches"
  ) +
  theme_minimal()

grafico_box_duracion


# ------------------------------------------------------------
# 5.3 adr
# ------------------------------------------------------------

# calcular cuartiles
Q1_adr <- quantile(
  hotel_preparado$adr,
  0.25
)

Q3_adr <- quantile(
  hotel_preparado$adr,
  0.75
)

# calcular ric
RIC_adr <-
  Q3_adr - Q1_adr

# calcular limites
limite_inf_adr <-
  Q1_adr - 1.5 * RIC_adr

limite_sup_adr <-
  Q3_adr + 1.5 * RIC_adr

# contar outliers
cantidad_outliers_adr <- sum(
  hotel_preparado$adr <
    limite_inf_adr |
    hotel_preparado$adr >
    limite_sup_adr
)

# mostrar resultados
Q1_adr
Q3_adr
RIC_adr
limite_inf_adr
limite_sup_adr
cantidad_outliers_adr


# boxplot de adr
grafico_box_adr <- ggplot(
  hotel_preparado,
  aes(y = adr)
) +
  geom_boxplot(
    fill = "lightblue"
  ) +
  labs(
    title = "Boxplot de la tarifa diaria",
    x = NULL,
    y = "ADR"
  ) +
  theme_minimal()

grafico_box_adr


# ------------------------------------------------------------
# 5.4 analisis bivariado
# ------------------------------------------------------------

# reservas no canceladas
estancias_validas <- hotel_preparado |>
  filter(
    is_canceled == 0
  )

# resumen por hotel
resumen_estancia <- estancias_validas |>
  group_by(hotel) |>
  summarise(
    n = n(),
    
    media = round(
      mean(duracion_total),
      2
    ),
    
    mediana =
      median(duracion_total),
    
    desviacion = round(
      sd(duracion_total),
      2
    ),
    
    q1 = quantile(
      duracion_total,
      0.25
    ),
    
    q3 = quantile(
      duracion_total,
      0.75
    ),
    
    maximo =
      max(duracion_total),
    
    .groups = "drop"
  )

resumen_estancia


# comparar duracion
grafico_estancia <- ggplot(
  estancias_validas,
  aes(
    x = hotel,
    y = duracion_total,
    fill = hotel
  )
) +
  geom_boxplot(
    show.legend = FALSE
  ) +
  labs(
    title = "Duracion de estancia por hotel",
    x = "Tipo de hotel",
    y = "Noches"
  ) +
  theme_minimal()

grafico_estancia


# ------------------------------------------------------------
# 5.5 analisis multivariado basico
# ------------------------------------------------------------

# reservas clasificables
hotel_menores <- hotel_preparado |>
  filter(
    !is.na(incluye_menores)
  )

# resumen multivariado
resumen_menores_hotel <- hotel_menores |>
  group_by(
    hotel,
    incluye_menores
  ) |>
  summarise(
    reservas = n(),
    
    duracion_media = round(
      mean(duracion_total),
      2
    ),
    
    lead_mediana =
      median(lead_time),
    
    adr_mediana = round(
      median(adr),
      2
    ),
    
    cancelacion_pct = round(
      mean(is_canceled) * 100,
      2
    ),
    
    .groups = "drop"
  )

resumen_menores_hotel


# cancelacion por grupo
grafico_cancelacion <- ggplot(
  resumen_menores_hotel,
  aes(
    x = hotel,
    y = cancelacion_pct,
    fill = incluye_menores
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Cancelacion segun hotel y presencia de menores",
    x = "Tipo de hotel",
    y = "Cancelaciones (%)",
    fill = "Reserva"
  ) +
  theme_minimal()

grafico_cancelacion


# ------------------------------------------------------------
# 5.6 analisis temporal
# ------------------------------------------------------------

# reservas por mes
reservas_mes <- hotel_preparado |>
  mutate(
    mes_llegada = as.Date(
      format(
        fecha_llegada,
        "%Y-%m-01"
      )
    )
  ) |>
  count(
    mes_llegada,
    hotel
  )

reservas_mes


# evolucion mensual
grafico_temporal <- ggplot(
  reservas_mes,
  aes(
    x = mes_llegada,
    y = n,
    color = hotel
  )
) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "Evolucion mensual de las reservas",
    x = "Mes",
    y = "Numero de reservas",
    color = "Hotel"
  ) +
  theme_minimal()

grafico_temporal


# reservas totales por mes
reservas_totales_mes <- hotel_preparado |>
  mutate(
    mes_llegada = as.Date(
      format(
        fecha_llegada,
        "%Y-%m-01"
      )
    )
  ) |>
  count(
    mes_llegada,
    sort = TRUE
  )

reservas_totales_mes


# mes con mas reservas
reservas_totales_mes |>
  slice_max(
    n,
    n = 1
  )


# mes con menos reservas
reservas_totales_mes |>
  slice_min(
    n,
    n = 1
  )


# ------------------------------------------------------------
# 5.7 respuesta a la pregunta analitica 1
# ------------------------------------------------------------

resumen_estancia
grafico_estancia


# ------------------------------------------------------------
# 5.8 respuesta a la pregunta analitica 2
# ------------------------------------------------------------

# comparar reservas
resumen_menores <- hotel_menores |>
  group_by(
    incluye_menores
  ) |>
  summarise(
    reservas = n(),
    
    duracion_media = round(
      mean(duracion_total),
      2
    ),
    
    duracion_mediana =
      median(duracion_total),
    
    lead_media = round(
      mean(lead_time),
      2
    ),
    
    lead_mediana =
      median(lead_time),
    
    adr_media = round(
      mean(adr),
      2
    ),
    
    adr_mediana = round(
      median(adr),
      2
    ),
    
    cancelacion_pct = round(
      mean(is_canceled) * 100,
      2
    ),
    
    .groups = "drop"
  )

resumen_menores


# porcentaje con menores por hotel
porcentaje_menores_hotel <- hotel_menores |>
  count(
    hotel,
    incluye_menores
  ) |>
  group_by(hotel) |>
  mutate(
    porcentaje = round(
      n / sum(n) * 100,
      2
    )
  ) |>
  ungroup()

porcentaje_menores_hotel


# grafico de menores por hotel
grafico_menores <- ggplot(
  porcentaje_menores_hotel,
  aes(
    x = hotel,
    y = porcentaje,
    fill = incluye_menores
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Reservas con y sin menores por hotel",
    x = "Tipo de hotel",
    y = "Porcentaje",
    fill = "Reserva"
  ) +
  theme_minimal()

grafico_menores


# ------------------------------------------------------------
# 6 comunicacion de hallazgos
# ------------------------------------------------------------

# resultados para los hallazgos
resumen_estancia
resumen_menores
resumen_menores_hotel
porcentaje_menores_hotel

# resultados temporales
reservas_totales_mes |>
  slice_max(
    n,
    n = 1
  )

reservas_totales_mes |>
  slice_min(
    n,
    n = 1
  )


# ------------------------------------------------------------
# guardar graficos
# ------------------------------------------------------------

# crear carpeta si no existe
dir.create(
  "output/graficos",
  recursive = TRUE,
  showWarnings = FALSE
)


# guardar reservas por hotel
ggsave(
  "output/graficos/reservas_por_hotel.png",
  plot = grafico_hotel,
  width = 8,
  height = 5,
  dpi = 300
)


# guardar distribucion de duracion
ggsave(
  "output/graficos/distribucion_duracion.png",
  plot = grafico_duracion,
  width = 8,
  height = 5,
  dpi = 300
)


# guardar distribucion de anticipacion
ggsave(
  "output/graficos/distribucion_anticipacion.png",
  plot = grafico_lead,
  width = 8,
  height = 5,
  dpi = 300
)


# guardar boxplot de duracion
ggsave(
  "output/graficos/boxplot_duracion.png",
  plot = grafico_box_duracion,
  width = 8,
  height = 5,
  dpi = 300
)


# guardar boxplot de adr
ggsave(
  "output/graficos/boxplot_adr.png",
  plot = grafico_box_adr,
  width = 8,
  height = 5,
  dpi = 300
)


# guardar duracion por hotel
ggsave(
  "output/graficos/duracion_por_hotel.png",
  plot = grafico_estancia,
  width = 8,
  height = 5,
  dpi = 300
)


# guardar cancelacion por menores
ggsave(
  "output/graficos/cancelacion_por_menores.png",
  plot = grafico_cancelacion,
  width = 8,
  height = 5,
  dpi = 300
)


# guardar evolucion mensual
ggsave(
  "output/graficos/evolucion_mensual.png",
  plot = grafico_temporal,
  width = 9,
  height = 5,
  dpi = 300
)


# guardar menores por hotel
ggsave(
  "output/graficos/menores_por_hotel.png",
  plot = grafico_menores,
  width = 8,
  height = 5,
  dpi = 300
)
# CNDDHH
# ======

## Paquetes
## --------
library(ggplot2) #graficos
library(dplyr) #manipulación de data
library(zoo) #series de tiempo
library(readr)
library(tidyr)
library(RcppRoll)
library("viridis") # paleta de colores

source("funciones.R") #carga funciones de R

## Importación data
## ----------------
positivos <- read.csv("data/positivos_covid.csv", sep = ";")

fallecidos <- read.csv("data/fallecidos_covid.csv", sep = ";")

fallecidos_sinadef <-
  read.csv(
    "data/fallecidos_sinadef.csv",
    sep = ";",
    fileEncoding = "latin1",
    skip = 2
  ) #cambio a read.csv para aplicar separador

reportes_minsa <-
  read.csv("data/reportes_minsa.csv",
           sep = ";",
           fileEncoding = "UTF-8")

contagios_indigenas <-
  read.csv("data/2020-10-01.dge_indígenas_distritos.csv")

contagios_indigenas_pueblos <-
  read.csv("data/2020-10-01.confirmados_por_pueblo_indigena.csv", sep = "\t")

## Limpieza de Data
## ----------------
positivos <- mutate(
  positivos,
  y = substr(FECHA_RESULTADO, 1, 4),
  m = substr(FECHA_RESULTADO, 5, 6),
  d = substr(FECHA_RESULTADO, 7, 8),
  fecha = as.Date(paste0(y, "-", m, "-", d)),
  EDAD_n = as.numeric(EDAD),
)

fallecidos <- mutate(
  fallecidos,
  y = substr(FECHA_FALLECIMIENTO, 1, 4),
  m = substr(FECHA_FALLECIMIENTO, 5, 6),
  d = substr(FECHA_FALLECIMIENTO, 7, 8),
  fecha = as.Date(paste0(y, "-", m, "-", d)),
  edad_declarada = as.numeric(EDAD_DECLARADA),
  yn = substr(FECHA_NAC, 1, 4),
  mn = substr(FECHA_NAC, 5, 6),
  dn = substr(FECHA_NAC, 7, 8)
)

fallecidos_sinadef <- mutate(
  fallecidos_sinadef,
  fecha = as.Date(fallecidos_sinadef$FECHA),
  año = substr(fecha, 1, 4)
)


## Filtrado de data
## ----------------

### Filtro hasta 30 de septiembre, corte del reporte.

positivos <- filter(positivos, fecha <= "2020-09-30")
fallecidos <- filter(fallecidos, fecha <= "2020-09-30")

### Filtro data SINADEF, excluye extranjeros y muertes violentas
fallecidos_sinadef_2020 <- filter(
  fallecidos_sinadef,
  `DEPARTAMENTO.DOMICILIO` != "EXTRANJERO",
  `MUERTE.VIOLENTA` %in% c("SIN REGISTRO", "NO SE CONOCE"),
  fecha >= "2020-01-01" & fecha <= "2020-09-30"
)

fallecidos_sinadef_2019 <- filter(
  fallecidos_sinadef,
  `DEPARTAMENTO.DOMICILIO` != "EXTRANJERO",
  `MUERTE.VIOLENTA` %in% c("SIN REGISTRO", "NO SE CONOCE"),
  fecha >= "2019-01-01" &
    fecha <= "2019-12-31"
)

fallecidos_sinadef_2018 <- filter(
  fallecidos_sinadef,
  `DEPARTAMENTO.DOMICILIO` != "EXTRANJERO",
  `MUERTE.VIOLENTA` %in% c("SIN REGISTRO", "NO SE CONOCE"),
  fecha >= "2018-01-01" &
    fecha <= "2018-12-31"
)

fallecidos_sinadef_2017 <- filter(
  fallecidos_sinadef,
  `DEPARTAMENTO.DOMICILIO` != "EXTRANJERO",
  `MUERTE.VIOLENTA` %in% c("SIN REGISTRO", "NO SE CONOCE"),
  fecha >= "2017-01-01" &
    fecha <= "2017-12-31"
)

## Generando acumulados:
## ---------------------
positivos_cum <- positivos %>%
  select(fecha) %>%
  group_by(fecha) %>%
  summarise(count = n()) %>%
  mutate(positivos_cum = cumsum(replace_na(count, 0)))

fallecidos_cum <- fallecidos %>%
  select(fecha) %>%
  group_by(fecha) %>%
  summarise(count = n()) %>%
  mutate(fallecidos_cum = cumsum(replace_na(count, 0)))

## Tablas:
## -------

### Tabla general

tabla_general <- positivos_cum %>%
  tail(n = 1) %>%
  full_join(tail(fallecidos_cum, n = 1), by = "fecha") %>%
  select(fecha, positivos_cum, fallecidos_cum) %>%
  rename(Contagios = positivos_cum) %>%
  rename(Fallecidos = fallecidos_cum)

### Tabla por regiones

tabla_regiones <- positivos %>%
  select(DEPARTAMENTO) %>%
  group_by(DEPARTAMENTO) %>%
  summarise(count = n()) %>%
  mutate(positivos_pct = prop.table(count) * 100) %>%
  rename(Contagios = count)

fallecidos_regiones <- fallecidos %>%
  select(DEPARTAMENTO) %>%
  group_by(DEPARTAMENTO) %>%
  summarise(count = n()) %>%
  mutate(fallecidos_pct = prop.table(count) * 100) %>%
  rename(Fallecidos = count)

tabla_regiones <-
  full_join(tabla_regiones, fallecidos_regiones, by = "DEPARTAMENTO")

### Tabla contagios indígenas por departamento
tabla_indígenas <- contagios_indigenas %>%
  group_by(Departamento) %>%
  summarise(Contagios = sum(Confirmados)) %>%
  mutate(contagios_pct = prop.table(Contagios) * 100)

### Tabla contagios indígenas por pueblos indígenas (agrupando otros)

tabla_indigenas_pueblos <- contagios_indigenas_pueblos %>%
  group_by(Pueblo) %>%
  summarise(Contagios = sum(Confirmados)) %>%
  add_row(
    Pueblo = "Otros",
    Contagios = filter(. , Contagios <= 200) %>% pull(Contagios) %>% sum()
  ) %>%
  mutate(contagios_pct = prop.table(Contagios) * 100) %>%
  filter (contagios_pct > 1)


## Gráficos:
## ---------

### SINADEF

data.temp <- fallecidos_sinadef %>%
  filter (fecha <= "2020-09-30") %>%
  group_by(fecha) %>%
  summarise(count = n()) %>%
  mutate(
    año = substr(fecha, 1, 4),
    mes = substr(fecha, 6, 7),
    date = as.Date(format(fecha, "1990-%m-%d")),
  )

g.sinadef <-
  ggplot(data.temp, aes(x = date, y = count, colour = año)) +
  geom_line(size = 0.8) +
  scale_x_date(date_labels = "%b",
               breaks = "1 month",
               minor_breaks = "1 week") +
  labs (x = "Fecha", y = "Casos") +
  theme (legend.position = "bottom", legend.title = element_blank())

### Contagios por region

g.region <- tabla_regiones %>%
  select(DEPARTAMENTO, Contagios, Fallecidos) %>%
  gather(key = "Tipo", value = "casos",-DEPARTAMENTO) %>%
  ggplot(aes(
    x = casos,
    y = reorder(DEPARTAMENTO, casos),
    fill = Tipo
  )) +
  geom_col(position = position_dodge()) +
  scale_x_log10() +
  scale_fill_manual(values = c ("#00BFC4", "#F8766D"))

g.contagios.region <- tabla_regiones %>%
  ggplot(aes(
    y = reorder(DEPARTAMENTO, Contagios),
    x = Contagios,
    label = Contagios,
    fill = DEPARTAMENTO
  )) +
  geom_col() +
  scale_x_log10(
    labels = function(DEPARTAMENTO)
      format(DEPARTAMENTO, scientific = FALSE)
  ) +
  geom_label(size = 3, fill = "white") +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE) +
  labs (x = element_blank(), y = element_blank(), title = element_blank())

g.fallecidos.region <- tabla_regiones %>%
  filter(DEPARTAMENTO != "LIMA REGION") %>%
  ggplot(aes(
    y = reorder(DEPARTAMENTO, Fallecidos),
    x = Fallecidos,
    label = Fallecidos,
    fill = DEPARTAMENTO
  )) +
  geom_col() +
  scale_x_log10(
    labels = function(DEPARTAMENTO)
      format(DEPARTAMENTO, scientific = FALSE)
  ) +
  geom_label(size = 3, fill = "white") +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE, option = "B") +
  labs (x = element_blank(), y = element_blank(), title = element_blank())

g.contagios.indigenas <- tabla_indígenas %>%
  ggplot(aes(
    y = reorder(Departamento, Contagios),
    x = Contagios,
    fill = Departamento
  )) +
  geom_col() +
  geom_label(aes(label = round(contagios_pct, 2)), fill = "white") +
  labs(x = element_blank(), y = element_blank(), title = element_blank()) +
  theme(legend.position = "none")

g.contagios.pueblos <- tabla_indigenas_pueblos %>%
  ggplot(aes(
    y = reorder(Pueblo, Contagios),
    x = Contagios,
    fill = Pueblo
  )) +
  geom_col() +
  geom_label(aes(label = Contagios), fill = "white") +
  labs (x = element_blank(), y = element_blank(), title = element_blank()) +
  theme(legend.position = "none")
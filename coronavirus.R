###===Paquetes===###
library(ggplot2) #graficos
library(dplyr) #manipulación de data 
library(zoo) #series de tiempo
library(readr)
library(tidyr)
library(RcppRoll)

source("funciones.R")

###===Importación data===###
positivos <- read_csv("data/positivos_covid.csv")
fallecidos <- read_csv("data/fallecidos_covid.csv")
fallecidos_sinadef <- read.csv("data/fallecidos_sinadef.csv", sep =";", fileEncoding = "latin1") #cambio a read.csv para aplicar separador
reportes_minsa <- read.csv("data/reportes_minsa.csv", sep =";", fileEncoding = "UTF-8") 

###===Limpieza de Data===####
positivos <- mutate(positivos, 
                    y = substr(FECHA_RESULTADO, 1, 4),
                    m = substr(FECHA_RESULTADO,5,6),
                    d = substr(FECHA_RESULTADO,7,8),
                    fecha=as.Date(paste0(y,"-",m,"-",d)),
                    EDAD_n = as.numeric(EDAD),
                    )

fallecidos <- mutate(fallecidos, 
                    y = substr(FECHA_FALLECIMIENTO, 1, 4),
                    m = substr(FECHA_FALLECIMIENTO,5,6),
                    d = substr(FECHA_FALLECIMIENTO,7,8),
                    fecha=as.Date(paste0(y,"-",m,"-",d)),
                    edad_declarada = as.numeric(EDAD_DECLARADA),
                    yn = substr(FECHA_NAC, 1, 4),
                    mn = substr(FECHA_NAC, 5, 6),
                    dn = substr(FECHA_NAC, 7, 8)
                    )

#Calculando recuperados diarios
recuperados_diarios <- reportes_minsa %>% select(Dia, Recuperados) %>%
                        mutate(Dia = as.Date( Dia, format="%Y-%m-%d")) %>%
                        mutate(rec_mm7 = rollmean(Recuperados, k = 7, fill=NA))
  
colnames(recuperados_diarios)[colnames(recuperados_diarios) == "Dia"] <- "fecha"

data <-full_join(data, recuperados_diarios, by="fecha")

rm (recuperados_diarios)

#Generando acumulados
positivos_cum <- positivos %>%
  select(fecha) %>%
  group_by(fecha) %>%
  summarise(count=n())

fallecidos_cum <- fallecidos %>%
  select(fecha) %>%
  group_by(fecha) %>%
  summarise(count=n())

positivos_cum[,"positivos_cum"] <- cumsum(positivos_cum$count)
fallecidos_cum[,"fallecidos_cum"] <- cumsum(replace_na(fallecidos_cum$count, 0))

###===Gráficos===###

#Gráfico general
data.temp <- data %>%
            select(fecha, positivos_cum, fallecidos_cum, Recuperados) %>% 
            gather(key="variable", value="value", -fecha)

g_diarios <- ggplot(data.temp, aes (x=fecha, y=value)) +
            geom_line(aes(color = variable)) +
            scale_color_manual(values = c ("darkred", "darkblue", "darkgreen"), labels = c("Fallecidos", "Positivos", "Recuperados")) +
            theme(legend.position = "bottom", legend.title = element_blank()) +
            scale_y_log10() +
            scale_x_date(date_labels = "%b", breaks = "1 month", minor_breaks = "1 week") +
            labs(x = "Fecha", y = "Número de casos", title = "Coronavirus en Perú")

rm(data.temp) #limpiando tabla temporal



###===Paquetes===###
library(ggplot2) #graficos
library(dplyr) #manipulación de data 
library(zoo) #series de tiempo
library(readr)
library(tidyr)
library(RcppRoll)

###===Descarga y actualiza data===###
download.file("https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download", "data/positivos_covid.csv")
download.file("https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download", "data/fallecidos_covid.csv")
download.file("https://cloud.minsa.gob.pe/s/nqF2irNbFomCLaa/download", "data/fallecidos_sinadef.csv")
download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", "data/owid-covid-data.csv")

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
###===Funciones===###
diarios <- function (x, k = 7) {
  data.temp <- x %>%
              select(fecha) %>%
              group_by(fecha) %>%
              summarise(count=n()) %>%
              mutate(mm7 = rollmean(count, k, fill=NA)) #Añade media movil 7 día
  
  g.temp <- ggplot(data.temp, aes(x = fecha, y = count)) +
                  geom_bar(stat="identity") +
                  geom_line(aes(y = mm7, color = "red"), size = 1.5) +
                  scale_color_manual(values = "red", labels = "Media móvil 7 días") +
                  scale_x_date(date_labels = "%b", breaks = "1 month", minor_breaks = "1 week") +
                  labs (x = "Fecha", y = "Casos", title = "Número de contagios diarios") +
                  theme (legend.position = "bottom", legend.title = element_blank())
  
return(list(data.temp, g.temp))
}

covid.trend <- function () { #esta funcion crea un gráfico inspirado en esta web https://aatishb.com/covidtrends/
  data.temp <- positivos %>%
              select(fecha) %>%
              group_by(fecha) %>%
              summarise(count=n()) %>%
              mutate(sum_sem = roll_sum (count, 7, fill = NA, align = "right")) #Suma los casos de la semana previa, requiere RcppRoll
  
  g.temp <- ggplot(data.temp, aes( x = count, y = sum_sem)) +
            geom_line() +
            scale_x_log10() +
            scale_y_log10() +
            labs (x = "Número de casos", y = "Casos de la semana anterior", title = "Tendencia de Coronavirus")
  
  return(list(data.temp, g.temp))
}

#Calculando recuperados diarios
recuperados_diarios <- reportes_minsa %>% select(Dia, Recuperados) %>%
                        mutate(Dia = as.Date( Dia, format="%Y-%m-%d")) %>%
                        mutate(rec_mm7 = rollmean(Recuperados, k = 7, fill=NA))
  
colnames(recuperados_diarios)[colnames(recuperados_diarios) == "Dia"] <- "fecha"

data <-full_join(data, recuperados_diarios, by="fecha")

rm (recuperados_diarios)

#Generando acumulados
data[,"positivos_cum"] <- cumsum(data$positivos)
data[,"fallecidos_cum"] <- cumsum(replace_na(data$fallecidos, 0))

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



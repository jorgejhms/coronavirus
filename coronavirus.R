#Paquetes
library(ggplot2) #graficos
library(dplyr) #manipulación de data 
library(zoo) #series de tiempo
library(readr)
library(tidyr)

#Descarga y actualiza data
download.file("https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download", "data/positivos_covid.csv")
download.file("https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download", "data/fallecidos_covid.csv")
download.file("https://cloud.minsa.gob.pe/s/nqF2irNbFomCLaa/download", "data/fallecidos_sinadef.csv")

#Importación data
positivos <- read_csv("data/positivos_covid.csv")
fallecidos <- read_csv("data/fallecidos_covid.csv")
fallecidos_sinadef <- read.csv("data/fallecidos_sinadef.csv", sep =";", fileEncoding = "latin1") #cambio a read.csv para aplicar separador
reportes_minsa <- read.csv("data/reportes_minsa.csv", sep =";", fileEncoding = "UTF-8") 

#Limpieza de Data#
##################

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

#Calcular casos diarios
data <- positivos %>%
    select(fecha) %>%
    group_by(fecha) %>%
    summarise(positivos=n())

data <-mutate(data, #Añadiendo media movil 7 días
                        pos_mm7 = rollmean(positivos, k = 7, fill=NA)
                        )

#Calculo fallecidos diarios
fallecidos_diarios <- fallecidos %>%
  select(fecha) %>%
  group_by(fecha) %>%
  summarise(fallecidos=n())

fallecidos_diarios <-mutate(fallecidos_diarios, #Añadiendo media movil 7 días
                        fal_mm7 = rollmean(fallecidos, k = 7, fill=NA)
                        )

data <- full_join(data, fallecidos_diarios, by = "fecha")

rm(fallecidos_diarios) #limpiando entorno

#Calculando recuperados diarios
recuperados_diarios <- reportes_minsa %>% select(Dia, Recuperados)
recuperados_diarios <- mutate(recuperados_diarios, Dia = as.Date( Dia, format="%Y-%m-%d"))
colnames(recuperados_diarios)[colnames(recuperados_diarios) == "Dia"] <- "fecha"
recuperados_diarios <- mutate(recuperados_diarios, rec_mm7 = rollmean(Recuperados, k = 7, fill=NA))

data <-full_join(data, recuperados_diarios, by="fecha")

rm (recuperados_diarios)

#Generando acumulados
data[,"positivos_cum"] <- cumsum(data$positivos)
data[,"fallecidos_cum"] <- cumsum(replace_na(data$fallecidos, 0))

#Gráficos
df_g <- data %>% select(fecha, positivos_cum, fallecidos_cum, Recuperados) %>% gather(key="variable", value="value", -fecha)

g_diarios <- ggplot(df_g, aes (x=fecha, y=value)) +
    geom_line(aes(color = variable)) +
    scale_color_manual(values = c ("darkred", "darkblue", "darkgreen")) +
    theme(legend.position = "bottom") +
    scale_y_log10() +
    ggtitle("Coronavirus en Perú") + ylab ("Número de casos") + xlab("Fecha")

rm(df_g) #limpiando tabla temporal

df_g <- data %>% select (fecha, positivos, pos_mm7) 
g_pos_diarios <- ggplot(df_g, aes(x = fecha, y = positivos)) +
    geom_bar(stat="identity") +
    geom_line(aes(y = pos_mm7, color = "red"), size = 1.5)
dev.copy(g_pos_diarios, file ="img/g_pos_diarios.png")
rm(df_g)
png("g_pos_diarios.png", width = 600, height = 600)
print(g_pos_diarios)
dev.off()

df_g <- data %>% select (fecha, fallecidos, fal_mm7) 
g_fal_diarios <- ggplot(df_g, aes(x = fecha, y = fallecidos)) +
  geom_bar(stat="identity") +
  geom_line(aes(y = fal_mm7, color = "red"), size = 1.5)
rm(df_g)
png("g_fal_diarios.png", width = 600, height = 600)
print(g_fal_diarios)
dev.off()
###===Funciones===###
descargar.datos <- function () {
  download.file("https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download", "data/positivos_covid.csv")
  download.file("https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download", "data/fallecidos_covid.csv")
  download.file("https://cloud.minsa.gob.pe/s/nqF2irNbFomCLaa/download", "data/fallecidos_sinadef.csv")
  download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", "data/owid-covid-data.csv")
}

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
    labs (x = "Fecha", y = "Casos") +
    theme (legend.position = "bottom", legend.title = element_blank())
  
  return(list(tail(data.temp), g.temp))
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
  
  return(list(tail(data.temp), g.temp))
}
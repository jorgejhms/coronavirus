# Funciones


descargar.datos <- 
  function () {
  # Descarga las últimas bases de datos desde sus fuentes.
  message("Descargando bases de datos")
  download.file(
    "https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download",
    "data/positivos_covid.csv"
  )
  download.file(
    "https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download",
    "data/fallecidos_covid.csv"
  )
  download.file(
    "https://cloud.minsa.gob.pe/s/nqF2irNbFomCLaa/download",
    "data/fallecidos_sinadef.csv"
  )
  download.file(
    "https://covid.ourworldindata.org/data/owid-covid-data.csv",
    "data/owid-covid-data.csv"
  )
  message("Bases de datos descargadas")
  }

diarios <-
  function (x, k = 7) {
    # Crea un gráfico con el número de casos de cada día en forma de barras y la media móvil como una línea roja.
    x %>%
      select(fecha) %>%
      group_by(fecha) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = fecha, y = count)) +
      geom_bar(stat = "identity") +
      geom_line(aes(y = rollmean(count, k, fill = NA), color = "red"), size = 1.5) +
      scale_color_manual(values = "red", labels = "Media móvil 7 días") +
      scale_x_date(date_labels = "%b",
                   breaks = "1 month",
                   minor_breaks = "1 week") +
      labs (x = "Fecha", y = "Casos") +
      theme (legend.position = "bottom"
             , legend.title = element_blank()) +
      labs(x = element_blank(),
           y = element_blank(),
           title = element_blank())
    return()
  }

covid.trend <-
  function () {
    # esta funcion crea un gráfico inspirado en esta web https://aatishb.com/covidtrends/
    test <-    positivos %>%
      select(fecha) %>%
      group_by(fecha) %>%
      summarise(count = n()) %>%
      mutate(
        sum_sem = roll_sum (count, 7, fill = NA, align = "right"),
        #Suma los casos de la semana previa, requiere RcppRoll
        positivos_cum = cumsum(replace_na(count, 0))
      ) %>%
      ggplot(aes(x = positivos_cum, y = sum_sem)) +
      geom_line() +
      scale_x_log10(limits = c(50,NA)) +
      scale_y_log10(limits = c(40,NA)) +
      labs (x = "Número de casos", y = "Casos de la semana anterior", title = "Tendencia de Coronavirus")
    return(test)
  }

importar.datos <-
  function () {
    # Importa las bases de datos descargadas (no funciona)
    message("Importando bases de datos...")
    .GlobalEnv$positivos <- read.csv("data/positivos_covid.csv")
    .GlobalEnv$fallecidos <- read.csv("data/fallecidos_covid.csv")
    .GlobalEnv$fallecidos_sinadef <-
      read.csv(
        "data/fallecidos_sinadef.csv",
        sep = ";",
        fileEncoding = "latin1",
        skip = 2
      ) #cambio a read.csv para aplicar separador
    .GlobalEnv$reportes_minsa <-
      read.csv("data/reportes_minsa.csv",
               sep = ";",
               fileEncoding = "UTF-8")
    message("¡Bases de datos importadas!")
  }
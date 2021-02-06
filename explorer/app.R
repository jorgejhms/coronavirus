Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8") # setea el sistema en español

# Librerias
library(shiny)
library(tidyverse)
library(lubridate)


# Data

da_p <- read.csv("../data/positivos_covid.csv", sep = ";") %>%
    mutate(
        FECHA_RESULTADO = as.Date(as.character(FECHA_RESULTADO), format = "%Y%m%d"),
        EDAD_n = as.numeric(EDAD),
        DEPARTAMENTO = recode(DEPARTAMENTO, "LIMA REGION" = "LIMA")
        #Junta Lima y Lima pronvincias
    ) %>%
    filter(!is.na(FECHA_RESULTADO))

da_f <- read.csv("../data/fallecidos_covid.csv", sep = ";") %>%
    mutate(
        FECHA_CORTE = as.Date(as.character(FECHA_CORTE), format = "%Y%m%d"),
        FECHA_FALLECIMIENTO = as.Date(as.character(FECHA_FALLECIMIENTO),
                                      format = "%Y%m%d"),
        FECHA_NAC = as.Date(as.character(FECHA_NAC), format = "%Y%m%d"),
    ) %>%
    filter(!is.na(FECHA_FALLECIMIENTO))

sinadef <-
    read.csv(
        "../data/fallecidos_sinadef.csv",
        sep = ";",
        fileEncoding = "latin1",
        skip = 2
    ) %>%
    mutate(FECHA = as.Date(FECHA),
           año = year(FECHA)) %>%
    filter(
        `DEPARTAMENTO.DOMICILIO` != "EXTRANJERO",
        `MUERTE.VIOLENTA` %in% c("SIN REGISTRO", "NO SE CONOCE")
    )


## Datos importantes

fecha_actualizacion <-
    da_p$FECHA_CORTE %>% last() %>% as.character() %>% as.Date("%Y%m%d")

# Frontend
ui <- htmlTemplate(
    "template.html",
    
    #Inputs
    date = dateRangeInput(
        inputId = "date",
        label = "Fechas",
        start = min(da_p$FECHA_RESULTADO),
        end = fecha_actualizacion,
        min = min(da_p$FECHA_RESULTADO),
        max = fecha_actualizacion
    ),
    
    dep = selectInput(
        inputId = "dep",
        label = "Escoge tu región:",
        choices = c("Todos", as.character(unique(da_p$DEPARTAMENTO))),
        selected = "Todos"
    ),
    
    # Values
    fecha_actualizacion = textOutput("fecha_actualizacion"),
    
    # Plots
    p_pos = plotOutput("p_pos"),
    p_fal = plotOutput("p_fal"),
    p_sinadef = plotOutput("p_sinadef")
)

# backend
server <- function(input, output) {
    
    # Filtros de regiones
    temp_pos <- reactive({
        if (input$dep != "Todos"){ 
            da_p <- da_p %>%
                filter(DEPARTAMENTO == input$dep)
        } else { 
            da_p <- da_p 
        }
    })
    
    temp_fal <- reactive({
        if (input$dep != "Todos"){ 
            da_f <- da_f %>%
                filter(DEPARTAMENTO == input$dep)
        } else { 
            da_f <- da_f 
        }
    })
    
    temp_sinadef <- reactive({
        if (input$dep != "Todos"){ 
            sinadef <- sinadef %>%
                filter(DEPARTAMENTO.DOMICILIO == input$dep)
        } else { 
            sinadef <- sinadef
        }
    })
    
    # Values
    
    output$fecha_actualizacion <- renderText({as.character(fecha_actualizacion)})
    # Gráficos
    
    output$p_pos <- renderPlot({
        temp_pos() %>%
            select(FECHA_RESULTADO) %>%
            group_by(FECHA_RESULTADO) %>%
            summarise(positivos = n()) %>%
            mutate(positivos_cum = cumsum(replace_na(positivos, 0))) %>%
            filter(FECHA_RESULTADO >= input$date[1] &
                       FECHA_RESULTADO <= input$date[2]) %>%
            ggplot(aes(x = FECHA_RESULTADO, y = positivos)) +
            geom_bar(stat = "identity") +
            geom_line(aes(y = zoo::rollmean(positivos, 7, fill = NA)),
                      size = 1.2,
                      colour = "red1") +
            scale_x_date(
                date_labels = "%b",
                breaks = "1 month",
                minor_breaks = NULL
            ) +
            theme(legend.position = "bottom"
                  ,
                  legend.title = element_blank()) +
            labs(x = element_blank(),
                 y = element_blank(),
                 title = element_blank())
    })
    
    
    output$p_fal <- renderPlot({
        temp_fal() %>%
            select(FECHA_FALLECIMIENTO) %>%
            group_by(FECHA_FALLECIMIENTO) %>%
            summarise(fallecidos = n()) %>%
            mutate(fallecidos_cum = cumsum(replace_na(fallecidos, 0))) %>%
            filter(FECHA_FALLECIMIENTO >= input$date[1] &
                       FECHA_FALLECIMIENTO <= input$date[2]) %>%
            ggplot(aes(x = FECHA_FALLECIMIENTO, y = fallecidos)) +
            geom_bar(stat = "identity") +
            geom_line(aes(y = zoo::rollmean(fallecidos, 7, fill = NA)),
                      size = 1.2,
                      colour = "red1") +
            scale_x_date(
                date_labels = "%b",
                breaks = "1 month",
                minor_breaks = NULL
            ) +
            theme(legend.position = "bottom"
                  ,
                  legend.title = element_blank()) +
            labs(x = element_blank(),
                 y = element_blank(),
                 title = element_blank())
    })
    
    output$p_sinadef <- renderPlot({
        temp_sinadef() %>%
            group_by(FECHA) %>%
            summarise(count = n()) %>%
            filter(FECHA >= input$date[1] &
                       FECHA <= input$date[2]) %>%
            ggplot(aes(x = FECHA, y = count)) +
            geom_line(size = 0.8) +
            scale_x_date(date_labels = "%b",
                         breaks = "1 month",
                         minor_breaks = "1 week") +
            scale_color_manual(values = c (rep("darkgray", 3), "orange", "red")) +
            labs (x = element_blank(),
                  y = element_blank()) +
            theme (legend.position = "bottom",
                   legend.title = element_blank(), )
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

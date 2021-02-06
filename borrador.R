

### Tendencia
```{r, cache=TRUE}
minsa_p_cum %>%
  mutate(
    sum_sem = roll_sum (positivos, 7, fill = NA, align = "right"),
    #Suma los casos de la semana previa, requiere RcppRoll
  ) %>%
  ggplot(aes(x = positivos_cum, y = sum_sem)) +
  geom_line() +
  scale_x_log10(limits = c(50, NA)) +
  scale_y_log10(limits = c(40, NA)) +
  labs (x = "Número de casos", y = "Casos de la semana anterior", title = "Tendencia de Coronavirus")
```


Fila {}
-----------------------
  
  ### Data por regiones
  
  ```{r}


datatable(
  tabla_regiones,
  class = 'cell-border stripe',
  colnames = c(
    'Departamento',
    'Contagios',
    '%',
    'Fallecidos',
    '%',
    "Contagios x 100 mil hab. ultimos 14 días"
  ),
  options = list(pageLength = 13)
)
```

Créditos
--------
  
  Fuente: [Datos abiertos del MINSA](https://www.minsa.gob.pe/datosabiertos/).







### Tendencia

```{r, cache=TRUE}
#minsa_p %>%
# select(FECHA_RESULTADO) %>%
#group_by(FECHA_RESULTADO) %>%
#summarise(count = n()) 

minsa_p_cum %>%
  mutate(
    sum_sem = roll_sum (positivos, 7, fill = NA, align = "right"),
    #Suma los casos de la semana previa, requiere RcppRoll
  ) %>%
  ggplot(aes(x = positivos_cum, y = sum_sem)) +
  geom_line() +
  scale_x_log10(limits = c(50, NA)) +
  scale_y_log10(limits = c(40, NA)) +
  labs (x = "Número de casos", y = "Casos de la semana anterior", title = "Tendencia de Coronavirus")
```
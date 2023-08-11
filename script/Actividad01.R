# Actividad 01 - Inmobiliaria Cali

## Fase 01: Proceso de Preparacion de los Datos
### 1. IntegraciC3n de los datos
### 2. Eliminar variables irrelevantes y redundantes
### 3. DescripciC3n estadC-stica de los datos
### 4. Limpieza de datos
## Fase 02: Modelamiento de Datos
## Fase 03: Visualizacion de resultados


# ::::::::: Desarrollo del Proceso :::::::::::

# Fase01: PreparaciC3n de los Datos
## 1. IntegraciC3n de los datos
### 1.0 Cargue de librerias a utilizar
#library(gdata)
#library(dplyr)
#library(ggplot2)
#library(leaflet)

### 1.1 Adecuacion del Espacio de Trabajo
rm(list = ls())

### 1.2 Carga de los Datos
library(gdata)
Datos <- read.csv(file = file.choose(), header = TRUE, sep = ",")

### 1.3 Informacion de las variables
str(Datos)

### 1.4 Descripcion Estadistica Inicial de las Variables
summary(Datos)


## 2. Eliminar variables irrelevantes y redundantes
### 2.1 Variables Irrelevamtes
library(dplyr)
Datos=select(Datos, -id)
summary(Datos)


## 3. DescripciC3n estadC-stica de los datos
### 3.1 Variables Cuantitativas
#### 3.1.1 Variable NC:mero de pisos
library(ggplot2)

ggplot(Datos, aes(x = factor(piso))) + geom_bar(fill = "#148F77") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = "NC:mero de pisos", y = "Frecuencia", title = "GrC!fico de nC:mero de pisos") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))


#### 3.1.2 Variable Estrato socioeconomico
ggplot(Datos, aes(x = factor(estrato))) + geom_bar(fill = "#148F77") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "Estratos", y = "Frecuencia", title = "GrC!fico de estratos") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))

#### 3.1.3 Variable Precio en Millones
Datos$rango_precio <- cut(Datos$preciom,
                          breaks = c(-Inf, 100, 300, 600, 900, 1200, 1500, 1800,
                                     Inf),
                          labels = c("<100", "100-300", "300-600", "600-900",
                                     "900-1200", "1200-1500", "1500-1800", ">1800"))

ggplot(Datos, aes(x = rango_precio)) +
  geom_bar(fill = "#148F77") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "Rango de precio", y = "Frecuencia", title = "GrC!fico de precios por rangos") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))

#### 3.1.4 Variable Area construida
Datos$rango_area <- cut(Datos$areaconst,
                          breaks = c(-Inf, 100, 300, 600, 900, 1200, 1500, 1800,
                                     Inf),
                          labels = c("<100", "100-300", "300-600", "600-900",
                                     "900-1200", "1200-1500", "1500-1800", ">1800"))

ggplot(Datos, aes(x = rango_area)) +
  geom_bar(fill = "#148F77") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "Area construida", y = "Frecuencia", title = "GrC!fico de area construida") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))

#### 3.1.5 Variable Parqueadero
ggplot(Datos, aes(x = factor(parquea))) + geom_bar(fill = "#148F77") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "Parqueadero", y = "Frecuencia", title = "GrC!fico de parqueaderos") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))

#### 3.1.6 Variable Numero de baC1os
ggplot(Datos, aes(x = factor(banios))) + geom_bar(fill = "#148F77") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "Num baC1os", y = "Frecuencia", title = "GrC!fico de numero de baC1os") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))

#### 3.1.7 Variable Numero de habitaciones
ggplot(Datos, aes(x = factor(habitac))) + geom_bar(fill = "#148F77") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "Num habitaciones", y = "Frecuencia", title = "GrC!fico de numero de habitaciones") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))

### 3.2 Variables Cualitativas (zona, tipo, barrio)
#### 3.2.1 Variable zona
ggplot(Datos, aes(x = zona, fill = zona)) + geom_bar(fill = "#148F77") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(x = "Zonas", y = "Frecuencia", title = "GrC!fico de zonas") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))

#### 3.2.2 Variable Tipo de Vivienda
ggplot(Datos, aes(x = tipo, fill = tipo)) + geom_bar(fill = "#148F77") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(x = "Tipo", y = "Frecuencia", title = "GrC!fico de Tipos de Viviendas") +
  theme(panel.background = element_rect(fill = "#A3E4D7"))

#### 3.2.3 Variable Barrios - Latitudes - Longitudes
# Crear el dataframe con los datos
data <- data.frame(
  zona = Datos$zona,
  barrio = Datos$barrio,
  longitud = Datos$longitud,
  latitud = Datos$latitud
)
# Crear el mapa y centrarlo en Cali, Valle del Cauca
library(leaflet)
map <- leaflet(data = data) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitud, lat = ~latitud, 
                   popup = ~barrio, radius = 5,
                   color = ~case_when(
                     zona == "Zona Norte" ~ "red",
                     zona == "Zona Oriente" ~ "blue",
                     zona == "Zona Centro" ~ "green",
                     zona == "Zona Oeste" ~ "yellow",
                     zona == "Zona Sur" ~ "pink",
                     TRUE ~ "gray"
                   )) %>%
  setView(lng = -76.5225, lat = 3.4372, zoom = 12)  # Coordenadas de Cali, Valle del Cauca
# Mostrar el mapa
map



## 4. Limpieza de datos
### 4.1 Limpieza de registros completamente vacios (NA)
Datos <- Datos[complete.cases(Datos$zona), ]
summary

### 4.2 Variables Cuantitativas (piso, parquea, banios, habitac)
#### 4.2.1 Reemplazamos valores 0 o "NA" por la media en la variable piso
media_piso <- mean(Datos$piso[Datos$piso != 0 & !is.na(Datos$piso)], na.rm = TRUE)
media_piso <- round(media_piso)
Datos <- Datos %>% 
  mutate(piso = ifelse(piso == 0 | is.na(piso), media_piso, piso))

#### 4.2.2 Reemplazamos valores 0 o "NA" por la media en la variable parquea
media_parquea <- mean(Datos$parquea[Datos$parquea != 0 & !is.na(Datos$parquea)], na.rm = TRUE)
media_parquea <- round(media_parquea)
Datos <- Datos %>% 
  mutate(parquea = ifelse(parquea == 0 | is.na(parquea), media_parquea, parquea))

#### 4.2.3 Reemplazamos valores 0 o "NA" por la media en la variable banios
media_banios <- mean(Datos$banios[Datos$banios != 0 & !is.na(Datos$banios)], na.rm = TRUE)
media_banios <- round(media_banios)
Datos <- Datos %>% 
  mutate(banios = ifelse(banios == 0 | is.na(banios), media_banios, banios))

#### 4.2.4 Reemplazamos valores 0 o "NA" por la media en la variable habitac
media_habitac <- mean(Datos$habitac[Datos$habitac != 0 & !is.na(Datos$habitac)], na.rm = TRUE)
media_habitac <- round(media_habitac)
Datos <- Datos %>% 
  mutate(habitac = ifelse(habitac == 0 | is.na(habitac), media_habitac, habitac))

### 4.3 Variables Cualitativas (tipo)
# Reemplazomos el valor "apto" por "Apartamento" en la variable "tipo"
Datos <- Datos %>% 
  mutate(tipo = ifelse(tipo == "apto" | tipo == "APARTAMENTO", "Apartamento", tipo))

Datos <- Datos %>% 
  mutate(tipo = ifelse(tipo == "CASA" | tipo == "casa", "Casa", tipo))



## 5 Exportamos los datos procesados
nombre_archivo <- "DatosProcesados.csv"
ruta_archivo_csv <- file.path(getwd(), nombre_archivo)
write.csv(Datos, file = ruta_archivo_csv, row.names = FALSE)
cat("El archivo CSV se ha guardado en:", ruta_archivo_csv, "\n")


# Fase02: Modelamiento de los Datos y Fase03: Visualizacion del Modelamiento
## 1. Modelamiento de datos por el Factor Economico
### 1.1 Precio vs Tipo de Vivienda
#### Tabla
library(knitr)
tabla_tipo <- Datos %>%
  group_by(tipo) %>%
  summarise(
    suma_precio = sum(preciom, na.rm = TRUE),
    promedio_precio = mean(preciom, na.rm = TRUE),
    Count = n()
  ) %>%
  mutate(
    suma_precio = format(suma_precio, big.mark = ",", scientific = FALSE),
    promedio_precio = format(promedio_precio, big.mark = ",", scientific = FALSE),
    Count = format(Count, big.mark = ",", scientific = FALSE)
  )
print(tabla_tipo)

#### Grafica
ggplot(tabla_tipo, aes(x = tipo, y = promedio_precio, fill = tipo)) +
  geom_bar(stat = "identity", color = "black", fill = "#148F77") +
  geom_text(aes(label = promedio_precio), vjust = -0.5, color = "black") +
  labs(x = "Tipo de Vivienda", y = "Promedio de Precios",
       title = "Promedio de Precios por Tipo de Vivienda") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#A3E4D7"))


### 1.2 Precio vs Ubicacion
#### Tabla
tabla_zona <- Datos %>%
  group_by(zona) %>%
  summarise(
    suma_precio = sum(preciom, na.rm = TRUE),
    promedio_precio = mean(preciom, na.rm = TRUE),
    Count = n()
  ) %>%
  mutate(
    suma_precio = format(suma_precio, big.mark = ",", scientific = FALSE),
    promedio_precio = format(promedio_precio, big.mark = ",", scientific = FALSE),
    Count = format(Count, big.mark = ",", scientific = FALSE)
  )
print(tabla_zona)

#### Grafica
ggplot(tabla_zona, aes(x = zona, y = promedio_precio, fill = zona)) +
  geom_bar(stat = "identity", color = "black", fill = "#148F77") +
  geom_text(aes(label = promedio_precio), vjust = -0.5, color = "black") +
  labs(x = "Zona de Vivienda", y = "Promedio de Precios",
       title = "Promedio de Precios por Zona de la Vivienda") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#A3E4D7"))


### 1.3 Precio vs Estrato
#### Tabla
tabla_estrato <- Datos %>%
  group_by(estrato) %>%
  summarise(
    suma_precio = sum(preciom, na.rm = TRUE),
    promedio_precio = mean(preciom, na.rm = TRUE),
    Count = n()
  ) %>%
  mutate(
    suma_precio = format(suma_precio, big.mark = ",", scientific = FALSE),
    promedio_precio = format(promedio_precio, big.mark = ",", scientific = FALSE),
    Count = format(Count, big.mark = ",", scientific = FALSE)
  )
print(tabla_estrato)

#### Grafica
ggplot(tabla_estrato, aes(x = estrato, y = promedio_precio, fill = estrato)) +
  geom_bar(stat = "identity", color = "black", fill = "#148F77") +
  geom_text(aes(label = promedio_precio), vjust = -0.5, color = "black") +
  labs(x = "Estrato", y = "Promedio de Precios",
       title = "Promedio de Precios por Estratos") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#A3E4D7"))


### 1.4 Precio vs TamaC1o
#### Tabla
rangos <- c(0, 100, 300, 600, Inf)
Datos$grupo_area <- cut(Datos$areaconst, breaks = rangos, labels =
                          c("0-100", "100-300", "300-600", ">600"), right = FALSE)
tabla_tamanio <- Datos %>%
  group_by(grupo_area) %>%
  summarise(
    suma_precio = sum(preciom, na.rm = TRUE),
    promedio_precio = mean(preciom, na.rm = TRUE),
    Count = n()
  ) %>%
  mutate(
    suma_precio = format(suma_precio, big.mark = ",", scientific = FALSE),
    promedio_precio = format(promedio_precio, big.mark = ",", scientific = FALSE),
    Count = format(Count, big.mark = ",", scientific = FALSE)
  )
print(tabla_tamanio)

#### Grafica
ggplot(tabla_tamanio, aes(x = grupo_area, y = promedio_precio, fill = grupo_area)) +
  geom_bar(stat = "identity", color = "black", fill = "#148F77") +
  geom_text(aes(label = promedio_precio), vjust = -0.5, color = "black") +
  labs(x = "Area construida", y = "Promedio de Precios",
       title = "Promedio de Precios por Area construida") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#A3E4D7"))


## 2. Modelamiento de datos por el Factor Caracteristicas
### 2.1 Tipo vs Zona
#### Tabla
tabla_tipozona <- Datos %>%
  group_by(zona) %>%
  summarise(
    apartamento = sum(tipo == "Apartamento", na.rm = TRUE),
    casa = sum(tipo == "Casa", na.rm = TRUE),
    sum = n()
  )
total_apartamento <- sum(tabla_tipozona$apartamento, na.rm = TRUE)
total_casa <- sum(tabla_tipozona$casa, na.rm = TRUE)
tabla_tipozona <- tabla_tipozona %>%
  add_row(zona = "Total", apartamento = total_apartamento, casa = total_casa)

print(tabla_tipozona)

#### Grafica
ggplot(tabla_tipozona, aes(x = zona)) +
  geom_bar(aes(y = apartamento, fill = "Apartamento"), stat = "identity") +
  geom_bar(aes(y = casa, fill = "Casa"), stat = "identity") +
  geom_text(aes(y = sum, label = sum), vjust = -0.5, color = "black") +
  scale_fill_manual(values = c(Apartamento = "#148F77", Casa = "#FF5733")) +
  labs(x = "Zona", y = "Cantidad",
       title = "Cantidad de Apartamentos y Casas por Zona") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "#A3E4D7"))

ggplot(tabla_tipozona, aes(x = zona)) +
  geom_bar(aes(y = apartamento, fill = "Apartamento"), position = "dodge", stat = "identity") +
  geom_bar(aes(y = casa, fill = "Casa"), position = "dodge", stat = "identity") +
  geom_text(aes(y = apartamento, label = apartamento), position = position_dodge(width = 0.8), vjust = -0.5) +
  geom_text(aes(y = casa, label = casa), position = position_dodge(width = 0.8), vjust = -0.5) +
  scale_fill_manual(values = c(Apartamento = "#1E6BB8", Casa = "#FF5733")) +
  labs(x = "Zona", y = "Cantidad",
       title = "Distribuci??n de Apartamentos y Casas por Zona") +
  theme_minimal() +
  theme(legend.position = "top")

### 2.2 Tipo vs Estrato
#### Tabla
tabla_tipoestrato <- Datos %>%
  group_by(estrato) %>%
  summarise(
    apartamento = sum(tipo == "Apartamento", na.rm = TRUE),
    casa = sum(tipo == "Casa", na.rm = TRUE),
    sum = n()
  )
total_apartamento <- sum(tabla_tipoestrato$apartamento, na.rm = TRUE)
total_casa <- sum(tabla_tipoestrato$casa, na.rm = TRUE)
tabla_tipoestrato <- tabla_tipoestrato %>%
  add_row(estrato = -1, apartamento = total_apartamento, casa = total_casa)
tabla_tipoestrato$estrato[tabla_tipoestrato$estrato == -1] <- "Total"

print(tabla_tipoestrato)

#### Grafica
ggplot(tabla_tipoestrato, aes(x = estrato)) +
  geom_bar(aes(y = apartamento, fill = "Apartamento"), stat = "identity") +
  geom_bar(aes(y = casa, fill = "Casa"), stat = "identity") +
  geom_text(aes(y = sum, label = sum), vjust = -0.5, color = "black") +
  scale_fill_manual(values = c(Apartamento = "#148F77", Casa = "#FF5733")) +
  labs(x = "Estrato", y = "Cantidad",
       title = "Cantidad de Apartamentos y Casas por Estrato") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "#A3E4D7"))

# Promedio de Precios de Tipos de vivienda segC:n Zona y Estrato
ggplot(Datos, aes(x = preciom, y = zona, color = factor(estrato))) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  facet_grid(. ~ tipo) +
  labs(x = "Promedio de Precios", y = "Zona",
       title = "Promedio de Precios de Tipos de vivienda segC:n Zona y Estrato",
       color = "Estrato") +
  scale_color_discrete(name = "Estrato") +
  theme_minimal()

# Tipo de Vivienda segC:n Zona, Estrato y Dimensiones
ggplot(Datos, aes(x = zona, y = areaconst, color = factor(estrato))) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  facet_grid(. ~ tipo) +
  labs(x = "Zona", y = "Dimensiones",
       title = "Tipo de Vivienda segC:n Zona, Estrato y Dimensiones") +
  scale_color_discrete(name = "Estrato") +
  theme_minimal()

# Correlaciones
# AsegC:rate de tener las bibliotecas necesarias
library(reshape2)
valUni <- unique(Datos$zona)
Datos$zona_num <- as.numeric(factor(Datos$zona, levels = valUni))

# Filtrar los datos para el tipo de vivienda "casa"
df_casa <- Datos[Datos$tipo == "Casa",]

# Calcula la matriz de correlaciC3n
correlation_matrix <- cor(df_casa[,c("estrato", "zona_num", "preciom", "areaconst", "piso", 
                                     "habitac", "banios", "parquea")])

# Convierte la matriz de correlaciC3n en un dataframe para ggplot
correlation_df <- reshape2::melt(correlation_matrix)

# Crea el mapa de calor
ggplot(data = correlation_df, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(label=sprintf("%.2f", value)), color="black", size=3) +
  scale_fill_gradient2(low = "#148F77", high = "#1E6BB8", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.title = element_blank()) +
  coord_fixed()

# Resultados
resultados <- data.frame(
  Variable = c("Cant var", "Dat buenos", "Var buenas", "Var Piso", "Var Parqueadero"),
  Inicial = rep(c(13, 4812, 11, 2641, 1606)),
  Final = rep(c(12, 8327, 12, 8327, 8327))
)

# Reorganizar los datos para el grC!fico de barras agrupadas
library(reshape2)
resultados_melt <- melt(resultados, id.vars = "Variable")

# Crear el grC!fico utilizando ggplot
ggplot(resultados_melt, aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = value), position = position_dodge(width = 0.8), vjust = -0.5) + # Agregar etiquetas de valores
  scale_fill_manual(values = c(Inicial = "#1E6BB8", Final = "#FF5733")) +
  labs(x = "Variables", y = "Valores", title = "Comparacion de Valores Iniciales y Finales") +
  theme_minimal() +
  theme(legend.position = "top")

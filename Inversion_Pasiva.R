#Remover todos los objetos del "Enviorment"
rm(list = ls())

#los 0s aceptados antes de expresar una cifra en notacion cientifica
options("scipen"=100, "digits"=4)

### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teoria Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(kableExtra)) # Tablas en HTML para interactuar

# Cargar el token de QUANDL
Quandl.api_key("Akx24vw1x3ziugMZeuY3")

# Funcion para descagar precios (aquí pones dentro de las llaves la función)
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable(code = "WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}

#Aqui creamos los argumentos que se necesitan para la funcion que creamos
tk <- c("TSLA", "BBY","HD") #tickers #con la 'c' le dices que es un vector
cs <- c("date","adj_close") #columnas que necesitas
fs <- c("2015-08-01","2016-08-01") #fechas a obtener

#Descargamos precios
Datos <- list() #primero creamos el vector 'Datos' vacio

#Creamos un for que va desde el 1 hasta la longitud de tk, o sea 3.
for(i in 1:length(tk)) {
  Datos[[i]] <- Bajar_Precios(cs,tk[i],fs[1],fs[2])
  }

#Recordamos que para los for y funciones, aqui van entre llaves, como en python es
#gracias a la identacion


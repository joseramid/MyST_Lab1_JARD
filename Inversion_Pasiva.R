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
cs <- c("date","adj_close")
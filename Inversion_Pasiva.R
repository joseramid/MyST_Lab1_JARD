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
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}

#El error era poner code = "wiki/prices", entonces borre lo que decia code.
#Tambien hay que checar bien la API key.

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

names(Datos) <- tk
#con 'names' cambiamos los nombres de las columnas por lo que quieras, en este caso
#por los tickers.

for(i in 1:length(tk)){
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))
  }

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1] #el -1 quita el ultimo valor, el rendimiento 0.

#aqui solo esta creando una lista, una lista con los rendimientos de los tres activos
#y los esta ordenando por el indice de la fecha. es por eso que es objeto 'xts'

names(Rends) <- tk
#aqui otra vez le cambiamos los nombres de las columnas por los tickers.

#el tipo de objeto que arroja Rends o Datos es un tipo de objeto 'xts'.
#un xts es como una DataFrame pero sus indices son las fechas siempre.

Port1 <- portfolio.spec(assets = tk) #te da los pesos igual

#Restricciones para el portafolio

Port1 <- add.constraint(portfolio = Port1, type = "full_investment")
#Nos aseguramos de que la suma de los pesos sea 1

Port1 <- add.constraint(portfolio = Port1, type = "box",
                        min = c(.01,.01,.01), max = c(.7,.7,.7))
#creo que si le pones solo un numero, toma ese minimo o maximo para todos los activos

Port1 <- add.objective(portfolio = Port1, type = "return", name = "mean")

Port1 <- optimize.portfolio(R = Rends, portfolio = Port1,
                            optimize_method = "random",
                            trace = TRUE, search_size = 5000)

Portafolios <- vector("list", length = length(Port1$random_portfolio_objective_results))

for(i in 1:length(Port1$random_portfolio_objective_results)) {
  
  Portafolios[[i]]$Pesos <- Port1$random_portfolio_objective_results[[i]]$weights
  #estamos en el vector vacio que creamos, llamado Portafolio, llenandolo con los 
  #valores aleatoreos que creo de la nube de portafolios de las funciones de la libreria
  #que hicimos con Port1. Entonces en este caso, dentro del objeto nuevo Portafolio
  #vamos a crear una nueva variable llamada pesos, y cada iteracion va a guardar
  #los pesos aleatoreos que encontro de la funcion.
  Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
  #lo mismo que la variable anterior, pero ahora estamos guardando los valores esperados
  Portafolios[[i]]$Vars <- var.portfolio(R = Port1$R, weights = Portafolios[[i]]$Pesos)
  #R son los rendimientos
  names(Portafolios[[i]]$Medias) <- NULL
  #esto se hizo porque se estaban guardando el vector medias, tambien el nombre de cada
  #activo, es por esto que con esta linea se borra el nombre y se quedan lo puros numeros
}

df_Portafolios <- data.frame(matrix(nrow = length(Port1$random_portfolio_objective_results),
                                    ncol = 3,
                                    data = 0))
#primero se crea la matriz (que pueden tener un solo tipo de dato), con la cantidad
#de filas que obtivumos de portafolios aleatorios,por eso depende de la longitud,
#luego 3 columnas (peso, media, var) y con el numero 0.
#Ya que se crea la matriz, se hace dataFrame, que es como una matriz pero soporta
#todo tipo de datos.

colnames(df_Portafolios) <- c("Rend","Var","Clase")
#Edita el nombre de las columnas.

Capital_Inicial <- 10000

for(i in i:length(Port1$random_portfolio_objective_results)) {
  
  #Aqui estamos haciendo las medias anuales, porque estaban diarias.
  #Y rendondeamos el numero a 4 decimales.
  df_Portafolios$Rend[i] <- round(Portafolios[[i]]$Medias*252,4)
  #Siempre que quieres entrar al elemento de una lista es doble corchete
  #Si quieres indexar, es un solo corchete.
  df_Portafolios$Var[i] <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)
  #Aqui estamos anualizando tambien, pero como hablamos de varianza,
  #tenemos que sacarle raiz cuadrada... Primero se obtiene la desv. est.
  #y luego se analiza con la raiz del plazo (sqrt(T))
  df_Portafolios$Clase[i] <- "No-Frontera"
  #En clase se acomularan las que estan en la frontera eficiente
  
  for(k in 1:length(tk)) {
  
    df_Portafolios[i, paste("Peso_", tk[k], sep = "")] <- Portafolios[[i]]$Pesos[k]
    #el comando paste es para pegar cadenas de texto.
    
    df_Portafolios[i, paste("Titulos_ini_", tk[k], sep = "")] <- 
      (Capital_Inicial*Portafolios[[i]]$Pesos[k])%/%Datos[[k]]$adj_close[1]
    #el operador %/% es para modulo, esto es porque no podemos comprar
    #fracciones de titulos
  }
}


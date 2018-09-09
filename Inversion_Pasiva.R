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



Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                            name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                            text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                          '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
  layout(title = "Portafolios (Markowitz)",
         xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                      showgrid = F),
         yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
         legend = list(orientation = 'h', y = -0.25))
Plot_portafolios

Port_1 <- df_Portafolios[which.max(df_Portafolios$Rend),]

# Portafolio con m?nima varianza
Port_2 <- df_Portafolios[which.min(df_Portafolios$Var),]

# Tasa libre de riesgo
rf <- 0.0025          
# Rendimiento de portafolio
rp <- df_Portafolios$Rend
# Varianza de portafolio
sp <- df_Portafolios$Var
# Indice de sharpe
sharpe <- (rp-rf)/sp

# Portafolio con m?ximo Sharpe ratio 
Port_3 <- df_Portafolios[which.max(sharpe),]

Ports <- cbind(rbind(Port_1, Port_2, Port_3),
               "Portafolio" = c("M?ximo Rendimiento","M?nima Varianza","M?ximo Sharpe Ratio"))

Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                            name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                            text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                          '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
  layout(title = "Portafolios (Markowitz)",
         xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                      showgrid = F),
         yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
         legend = list(orientation = 'h', y = -0.25)) %>%
  add_trace(x = ~Ports$Var[1], y = ~Ports$Rend[1], name = Ports$Portafolio[1],
            mode = 'marker', marker = list(color="red", size=10)) %>%
  add_trace(x = ~Ports$Var[2], y = ~Ports$Rend[2], name = Ports$Portafolio[2],
            mode = 'marker', marker = list(color="blue", size=10)) %>%
  add_trace(x = ~Ports$Var[3], y = ~Ports$Rend[3], name = Ports$Portafolio[3],
            mode = 'marker', marker = list(color="orange", size=10))
Plot_portafolios

# Pesos y titulos iniciales, de todos los activos, para los 3 portafolios
Pesos_Titulos <- Ports[,-c(1,2,3)]

# Encontrar las columnas cuyo nombre contenga "Titulos_ini", con esas encontraremos m?s f?cil los t?tulos
# por portafolio por activo
Ind <- grep(pattern = "Titulos_ini",x = colnames(Pesos_Titulos))
Historicos_Ports <- data.frame("Date" = Datos[[1]]$date)

# Crear data frame que contendr? los datos finales de cada estrategia
for(i in 1:length(Ports[,1])) {
  Historicos_Ports[[paste("Portafolio_",i,sep="")]] <- 
    (Datos[[1]]$adj_close*Pesos_Titulos[i,Ind[1]]  + 
       Datos[[2]]$adj_close*Pesos_Titulos[i,Ind[2]] +
       Datos[[3]]$adj_close*Pesos_Titulos[i,Ind[3]])
}


plot_ly(Historicos_Ports) %>%
  add_trace(x = ~Date, y = ~round(Portafolio_1,2), type = 'scatter', mode = 'lines', name = 'M?ximo Rendimiento',
            line = list(color = 'red'), hoverinfo = "text", text = ~paste('Port_1',round(Portafolio_1,2))) %>%
  add_trace(x = ~Date, y = ~round(Portafolio_2,2), type = 'scatter', mode = 'lines', name = 'M?nima Varianza',
            line = list(color = 'blue'), hoverinfo = "text", text = ~paste('Port_2',round(Portafolio_2,2)))  %>%
  add_trace(x = ~Date, y = ~round(Portafolio_3,2), type = 'scatter', mode = 'lines', name = 'M?ximo Sharpe Ratio',
            line = list(color = 'orange'), hoverinfo = "text", text = ~paste('Port_3',round(Portafolio_3,2)))%>% 
  layout(title = "3 Portafolios distintos objetivos",
         xaxis = list(title = "Fechas", showgrid = T),
         yaxis = list(title = "Balance"), 
         legend = list(orientation = 'h', y = -0.25, x = 0.5))
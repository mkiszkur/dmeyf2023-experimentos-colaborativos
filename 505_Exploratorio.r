# Este script genera graficos que muestra el impacto de la cuarentena en algunas variables

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_03.csv.gz"
PARAM$experimento <- "EXP5000"
# FIN Parametros del script

#------------------------------------------------------------------------------
#Funciones auxiliares
#------------------------------------------------------------------------------

plotear_lineas_verdes <- function(){
  abline(
    v = c(1, 13, 25),
    col = c("green", "green", "green"),
    lty = c(1, 1, 1),
    lwd = c(1, 1, 1)
  )
  
  abline(
    v = c(7, 19, 31),
    col = c("green", "green", "green"),
    lty = c(3, 3, 3),
    lwd = c(1, 1, 1)
  )
  
  
}

plotear_grafico <- function (data, calcular_min_max, ratio, campo, main_title, x_label, y_label){

  if (calcular_min_max) {
    
    ymin <- min(get (ratio, data))
    ymax <- max(get (ratio, data))
  
    if (ymin == 0) ymin <- -0.1
    if (ymax == 0) ymax <- 0.1
  }
  
  plot(
    x = 1:nrow(data),
    y = get (ratio, data),
    type = "o",
    main = paste0(main_title, campo),
    xlab = x_label,
    ylab = y_label,
    ylim = if (calcular_min_max) c(ymin, ymax) else NULL, 
    xaxt = "n"
  )
  
  axis(1, at = 1:nrow(data), labels = data$foto_mes)
  
  plotear_lineas_verdes()  
}



zeros_ratio <- function(campos, ds){
  
  for (campo in campos) {
    
    tbl <- ds[
      ,
      list("zero_ratio" = sum(get(campo) == 0, na.rm = TRUE) / .N),
      foto_mes
    ]
    
    plotear_grafico(tbl, TRUE, "zero_ratio", campo,  "Zeroes ratio  -  ", "Periodo", "Zeroes  ratio")
  }
  

}


nas_ratio <- function (campos, ds){
  
  for (campo in campos) {
    tbl <- ds[
      ,
      list("na_ratio" = sum(is.na(get(campo)), na.rm = TRUE) / .N),
      foto_mes
    ]
  
    plotear_grafico(tbl, TRUE, "na_ratio", campo,  "NAs ratio  -  ", "Periodo", "NAs  ratio")

  }
}

promedios <- function (campos, ds){
  
  for (campo in campos) {
    tbl <- ds[
      ,
      list("promedio" = mean(get(campo), na.rm = TRUE)),
      foto_mes
    ]
    
    ceros <- ds[
      ,
      list("zero_ratio" = sum(get(campo) == 0, na.rm = TRUE) / .N),
      foto_mes
    ]
    
    plotear_grafico(tbl, FALSE, "promedio", campo,  "Promedios  -  ", "Periodo", "Promedio")
    
    for (i in 1:nrow(tbl)) {
      if (ceros[i, zero_ratio] > 0.99 & median(ceros[, zero_ratio]) < 0.99) {
        abline(
          v = c(i),
          col = c("red"), lty = c(1), lwd = c(1)
        )
      }
    }
  }
  
}


promedios_no_cero <- function (campos, ds) {
  for (campo in campos) {
    tbl <- ds[
      get(campo) != 0,
      list("promedio" = mean(get(campo), na.rm = TRUE)),
      foto_mes
    ]
    
    ceros <- ds[
      ,
      list("zero_ratio" = sum(get(campo) == 0, na.rm = TRUE) / .N),
      foto_mes
    ]
    
    plotear_grafico(tbl, FALSE, "promedio", campo,  "Promedios NO cero  -  ", "Periodo", "Promedio valores no cero")
    
  
    for (i in 1:nrow(tbl)) {
      if (ceros[i, zero_ratio] > 0.99 & median(ceros[, zero_ratio]) < 0.99) {
        abline(v = c(i), col = c("red"), lty = c(1), lwd = c(1))
      }
    }
  }

}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# copio si hace falta el dataset

setwd("/Users/miguelkiszkurno/Documents/dmeyf") 

# cargo el dataset
dataset <- fread(PARAM$dataset) # donde entreno


# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# ordeno el dataset
setorder(dataset, foto_mes, numero_de_cliente)

campos_cantidad <- c('cprestamos_personales',
                   'cextraccion_autoservicio',
                   'ccheques_depositados',
                   'ccallcenter_transacciones',
                   'chomebanking_transacciones',
                   'ccajas_transacciones',
                   'ccajas_consultas',
                   'ccajas_depositos',
                   'ccajas_extracciones',
                   'ccajas_otras',
                   'catm_trx',
                   'catm_trx_other',
                   'cmobile_app_trx')


campos_count <-  c('internet',
                    'tcallcenter',
                    'thomebanking',
                    'tmobile_app')


#------------------------------------------------------------------------------
# Para cada variable ,
# grafico para cada mes el ratio de ceros que tiene esa variable
# el zeroes_ratio de una variable para un mes dado
# es el cociente entre
#   la cantidad de veces que la variable toma el valor cero ese mes
#   y la cantidad total de registros para ese mes

pdf("campos-count-zeroes_ratio.pdf")

zeros_ratio(campos_count, dataset)

dev.off()


#------------------------------------------------------------------------------
# Para cada variable ,
# grafico para cada mes el ratio de ceros que tiene esa variable
# el zeroes_ratio de una variable para un mes dado
# es el cociente entre
#   la cantidad de veces que la variable toma el valor cero ese mes
#   y la cantidad total de registros para ese mes

pdf("campos-cant-zeroes_ratio.pdf")

zeros_ratio(campos_cantidad, dataset)

dev.off()

#------------------------------------------------------------------------------
# Para cada variable ,
# grafico para cada mes el ratio de NAs que tiene esa variable
# el nas_ratio de una variable para un mes dado
# es el cociente entre
#   la cantidad de veces que la variable toma el valor nulo (NA) ese mes
#   y la cantidad total de registros para ese mes

pdf("campos-cant-nas_ratio.pdf")

nas_ratio (campos_cantidad, dataset)

dev.off()

#------------------------------------------------------------------------------
# Para cada variable , grafico para cada mes el promedio de esa variable
# el promedio de una variable para un mes dado es
# la definicion tradicional de promedio


pdf("campos-cant-promedios.pdf")

promedios (campos_cantidad, dataset)

dev.off()

#------------------------------------------------------------------------------
# Para cada variable ,
#  grafico para cada mes el promedio de esa variable
#   cuando la variable es DISTINTA de cero
# el promedio_nocero de una variable para un mes dado
# es el promedios del conjunto de valores de esa variable para ese mes
#   tales que no no son ni nulos ni tampoco valen cero

pdf("campos-cant-promedios_nocero.pdf")

promedios_no_cero(campos_cantidad, dataset) 

dev.off()

#------------------------------------------------------------------------------

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "zRend.txt",
  append = TRUE
)

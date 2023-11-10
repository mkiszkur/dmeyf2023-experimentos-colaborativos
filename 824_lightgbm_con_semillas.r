# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

#Experimento <- Experimento Colabortivo Prueba 001
PARAM$experimento <- "ECS001"

#PARAM$input$dataset <- "./datasets/Competencia_03_EC.csv.gz"
#PARAM$input$dataset <- "./datasets/competencia_03_100.csv.gz"
PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"


# meses donde se entrena el modelo
#PARAM$input$training <- c(202012, 202101, 202102, 202103, 202104, 202105)
PARAM$input$training <- c(202105)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 102191

# hiperparametros intencionalmente NO optimos
PARAM$finalmodel$optim$num_iterations <- 730
PARAM$finalmodel$optim$learning_rate <- 0.0323601846272594
PARAM$finalmodel$optim$feature_fraction <- 0.909773795582897
PARAM$finalmodel$optim$min_data_in_leaf <- 4637
PARAM$finalmodel$optim$num_leaves <- 667


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$finalmodel$semilla
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

PARAM$semillerio <- c(100193,100207)
#PARAM$tipo_ejecucion = 'Google Cloud'
PARAM$tipo_ejecucion = 'local'

if (PARAM$tipo_ejecucion == 'local') {
  setwd("/Users/miguelkiszkurno/Documents/dmeyf") 
}else {
  setwd("~/buckets/b1/") # Establezco el Working Directory
}



# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
realidad <- dataset[foto_mes == PARAM$input$future, list(numero_de_cliente, foto_mes, clase_ternaria) ]
realidad[, real := ifelse(clase_ternaria %in% c("CONTINUA", "BAJA+1"), 0L, 1L) ]

campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]


#Genero dataset de ceros para acumular las probabilidades
sumarizacion <- dapply[, list(numero_de_cliente, foto_mes)]
sumarizacion[, prob := 0]

for (s in PARAM$semillerio){
  
  cat(format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Inicio modelo de la semilla: ", s, "\n")
  
  
  # reemplazar la semilla con la siguiente semilla
  PARAM$finalmodel$lgb_basicos$seed = s
  
  # genero el modelo
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
                      PARAM$finalmodel$optim)

  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )

  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  archivo_importancia <- paste0 (s, "_impo.txt")
  
  fwrite(tb_importancia,
         file = archivo_importancia,
         sep = "\t"
  )
  
  #--------------------------------------



  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  

  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]
  
  sumarizacion[, prob := prob + prediccion]

  # grabo las probabilidad del modelo
  archivo_prediccion <- paste0 (s, "_prediccion.txt")
  
  fwrite(tb_entrega,
    file = archivo_prediccion,
    sep = "\t"
  )

}

# grabo las sumas de las probabilidades
fwrite(sumarizacion,
       file = "prediccion_total.txt",
       sep = "\t"
)


# ordeno por probabilidad descendente
sumarizacion$real <- realidad$real
print(sumarizacion)

setorder(sumarizacion, -prob)
# genero archivos con los "envios" mejores
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  
  #Calculo los que tengo que enviar en funcion del corte actual
  sumarizacion[, Predicted := 0L]
  sumarizacion[1:envios, Predicted := 1L]

  #Calculo la ganancia
  cuenta  <- sumarizacion [Predicted == 1]
  cuenta [, gan := ifelse(Predicted == 1 & real == 1, 273000, -7000)]
  cuenta [, gan_acum := cumsum(gan)]
  
  #Escribo el archivo con la ganancia
  fwrite(cuenta,
         file = paste0(PARAM$experimento, "_", envios, "-cuenta.csv"),
         sep = ",")
  #Escribo el archivo para kaggle
  fwrite(sumarizacion[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
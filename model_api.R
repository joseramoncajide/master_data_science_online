##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Supervised Learning for classification (Cont.)
# Model deployment
##########################################################################


library(tidymodels)
library(plumber)
titanic_model <- readRDS("tmp/titanic_model.rds")

#' @apiTitle Modelo predictivo (Titanic))
#' @apiDescription Esta API toma los datos de con la clase y edad y estima la propabilidad de que supervivencia
#' 
#' 
#' @param Pclass:numeric Clase: 1st, 2nd, 3rd
#' @param Age:numeric Edad: 1-80

#* @get /predict
default <- function(Pclass, Age){
  Pclass <- as.numeric(1)
  Age <- as.numeric(1)
  new.df <- tibble(Pclass = Pclass, Age = Age)
  if (any(is.na(new.df))) {
    res$status <- 400
    res$body <- "Faltan parámetros"
  }
  predict(titanic_model, new.df, type = "prob")
  
  
}
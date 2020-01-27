
#==============================================================================================================
# Funcion para hacer tablas de frecuencias
#  df              = data.frame
#  ...             = variables sin más, no hace falta lista ni " ni nada
#  col_names       = vector con los nombres para las columnas por defecto -> c("Variable","Values","n","rel.freq")
#  total           = hacer total de n y de %
#  sort_by_values  = ordena por nombre de categorias
#  sort_by_freq    = ordena por n de categorias
#  sort_by_percent = ordena por % de categorias
#  sort_descending = ordena de forma descendente (de mayor a menor)
#  
#
#==============================================================================================================
# EJEMPLO DE USO
# -------------------
# Generamos datos aleatorios
# edad_ <- sample(x = 65:100, size=30, replace = TRUE )
# estatura_ <- as.integer(sample(x = 120:205, size=30, replace = TRUE ))
# sexo_ <- sample(x = c("Hombre", "Mujer"), prob = c(.5,.5), size = 30, replace = TRUE)
# rubio_ <- sample(x = c("Si", "No"), prob = c(.2,.8), size = 30, replace = TRUE)
# data_ <- data.frame(EDAD=edad_,ESTATURA=estatura_,SEXO=sexo_,RUBIO=rubio_)
#
# FRECUENCIAS
# freq(data_,SEXO,RUBIO)
# freq(data_,SEXO,RUBIO, decimales = 3)
# freq(data_,SEXO,RUBIO, decimales = 3, sort_by_percent = TRUE)
#
#==================================================================================
require("dplyr")

#' Title
#'
#' @param df data
#' @param ... variables (no quotes needed)
#' @param col_names vector with names for the 4 columns
#' @param decimales number of decimals to show
#' @param show_warnings 
#' @param total 
#' @param sort_by_values 
#' @param sort_by_freq 
#' @param sort_by_percent 
#' @param sort_decreasing 
#' @param debug
#'
#' @return list of dataframes (1 per variable)
#' @export
#'
#' @examples
freq <- function(df,..., col_names=c("Variable","Values","n","rel.freq"), group_by = c(""),
                 decimales=2, show_warnings = TRUE, total=TRUE,
                 sort_by_values=FALSE, sort_by_freq=FALSE, sort_by_percent=FALSE, sort_decreasing = TRUE,
                 debug = FALSE) {
  
  if (debug) {
    
    print(paste("Column names: ",col_names))
    # if (exists(group_by)) cat(paste("\n",group_by))
  }
  
  if (length(col_names) != 4 ) stop("Numero de columnas en 'col_names' debería ser 4")
  
  vars <- enquos(...)
  
  result_df <- list()
  
  
  if (length(group_by)>0)
  {
    group <- group_by
    if (debug) cat(paste("\n Group by: ",group,"\n"))
    df <- df %>% group_by_at(vars(one_of(group)))
  }
  
  
  for (v in vars)
  {
    res <- udaic_freq(df,!! v,total=total, decimales=decimales, 
                      sort_by_values=sort_by_values,
                      sort_by_freq=sort_by_freq,
                      sort_by_percent=sort_by_percent)
    names(res) <- col_names
    
    result_df[[length(result_df)+1]] <- as.data.frame(res)
  }
  
  if (exists("result_df"))
  {
    # class(result_df) <- "udaic_freq"
    return(result_df)
  }
}


udaic_freq <- function(df, var, total=TRUE, decimales=2, sort_by_values=TRUE, sort_by_freq=FALSE, sort_by_percent=FALSE, sort_decreasing = TRUE)
{
  v <- enquo(var)
  
  result_temp <- df %>% group_by(!! v) %>%
    summarise(n = n()) %>%
    mutate(rel.freq = round((n/sum(n))*100,decimales))
  # mutate(rel.freq = n/sum(n)*100)
  #-------- add the column for Values within the variable
  colnames(result_temp)[1] <- "Values"
  
  #-------- add the column with the variable NAME
  var_column = rep(quo_name(v),nrow(result_temp))
  result_temp <- mutate(result_temp,Variable=var_column)
  
  
  #-- take Variable_column to the front
  result_temp <- result_temp %>% select(Variable, everything())
  
  result_temp <- as.data.frame(result_temp)
  
  # -- SORT
  if (sort_by_values) result_temp <- result_temp[order(result_temp[,2], decreasing = sort_decreasing),]
  if (sort_by_freq) result_temp <- result_temp[order(result_temp[,3], decreasing = sort_decreasing),]
  if (sort_by_percent) result_temp <- result_temp[order(result_temp[,4], decreasing = sort_decreasing),]
  rownames(result_temp) <- NULL
  
  # ------------- REMOVE var name except top 
  result_temp$Variable[-1]  <- ""
  
  #-------- add totals row
  if (total == TRUE)
  {
    var_total <- paste(quo_name(v),"TOTAL")
    levels(result_temp$Values) <- c(levels(result_temp$Values),"-")
    result_temp <- result_temp %>% rbind(c(var_total,"-",sum(.$n),sum(.$n)/sum(.$n)*100))
  }
  
  
  # class(result_temp) <- "udaic.freq"
  return(result_temp)
}




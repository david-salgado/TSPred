#' Seleccion de la mejor prediccion de un conjunto de modelos de series temporales.
#'
#' \code{BestTSPred} Selecciona la mejor prediccion para una serie de tiempo de entre las
#' proporcionadas por un conjunto de modelos.
#'
#' Este metodo toma como argumento de entrada el objeto \code{x} que contiene las variables que
#' queremos predecir y selecciona la mejor prediccion de entre un conjunto de predicciones
#' especificadas como input en el objeto de clase \linkS4class{BestTSPredParam}.
#'
#'
#' @param x \code{vector} u objeto de clase \code{StQList}, con las variables y los valores para los
#' que obtendremos las predicciones.
#'
#' @param BestTSPredParam Objeto de clase \linkS4class{BestTSPredParam} con los parametros de los
#' distintos modelos de prediccion de series temporales y las variables para las que se quieren
#' obtener las predicciones.
#'
#' @return \code{data.table} con componentes \code{Pred} y \code{STD} que contienen la mejor
#' prediccion, para cada variable, de entre las especificadas en el objeto de entrada
#' \code{BestTSPredParam}, y sus correspondientes desviaciones tipicas estimadas.
#'
#' @examples
#'
#' # Predicting one and two months ahead in time
#' data(Example1.TS)
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L))
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list)
#' BestTSPred(Example1.TS, BestTSPredParam)
#'
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' StQList <- readRDS('../E30183.FF.StQList.rds')
#' Units <- StQ::getUnits(StQList[['MM102016']])
#' Units <- Units[sample(1:(dim(Units)[1]), 1000)]
#' data.table::setkeyv(Units, 'NOrden')
#' StQ::setUnits(StQList) <- Units
#' VarNames <- c('CifraNeg_13.___', 'Personal_07.__1._1._')
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L),
#'                 Arima = list('AutoArimaTSPred', forward = 2L))
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)
#' BestTSPred(StQListExample, BestTSPredParam)
#' }
#'
#' @import data.table RepoTime  StQ TSPred parallel
#'
#' @include BestTSPredParam-class.R
#'
#' @export
setGeneric("BestTSPred", function(x, BestTSPredParam){standardGeneric("BestTSPred")})

#' @rdname BestTSPred
#'
#' @export
setGeneric("BestTSPred", function(x, BestTSPredParam){standardGeneric("BestTSPred")})

#' @rdname BestTSPred
#'
#' @export
setMethod(
  f = "BestTSPred",
  signature = c("vector"),
  function(x, BestTSPredParam){


    Results <- lapply(seq(along = BestTSPredParam@TSPred.list), function(TSPred){

      Function <- BestTSPredParam@TSPred.list[[TSPred]][[1L]]
      Param.List <- list()
      Param.List[['x']] <- x
      if (length(BestTSPredParam@TSPred.list[[TSPred]]) >= 2) Param.List <- c(Param.List, BestTSPredParam@TSPred.list[[TSPred]][-1])

      out <- do.call(Function, Param.List)
      out[, TSPred := TSPred]
      return(out)

    })

    DT <- rbindlist(Results)
    IDQuals <- setdiff(names(DT), c('Pred', 'STD', 'TSPred'))
    STD.na <- DT[, all(is.na(STD)), by = IDQuals]
    STD.na <- STD.na[V1 == TRUE]
    if (dim(STD.na)[1] > 0){

      STD.na <- STD.na[, Pred := as.numeric(NA)]
      STD.na <- STD.na[, STD := as.numeric(NA)]
      STD.na[, V1 := NULL]
      units.na <- STD.na[[IDQuals]]
      DT <- DT[!(DT[[IDQuals]] %in% units.na)]
    }
    MinSTD.index <- DT[, which.min(STD), by = IDQuals]
    setnames(MinSTD.index, 'V1', 'TSPred')
    output <- merge(DT, MinSTD.index, by = c(IDQuals, 'TSPred'))
    output[, TSPred := NULL]
    if (dim(STD.na)[1] > 0) output <- rbind(STD.na, output)


    return(output)
  }
)
#' @rdname BestTSPred
#'
#' @export
setMethod(
  f = "BestTSPred",
  signature = c("StQList"),
  function(x, BestTSPredParam){


      VarNames <- BestTSPredParam@VarNames
      if (length(VarNames) == 0) stop('[BestTSPred StQList] Slot VarNames in the parameter BestTSPredParam must be specified.')

      x_IDDDnoNA <- x[IDDD != '']

      Results <- lapply(seq(along = BestTSPredParam@TSPred.list), function(TSPred){

        Function <- BestTSPredParam@TSPred.list[[TSPred]][[1L]]
        Param.List <- list()
        Param.List[['x']] <- x_IDDDnoNA
        Param.List[['VarNames']] <- VarNames

        if (length(BestTSPredParam@TSPred.list[[TSPred]]) >= 2) Param.List <- c(Param.List, BestTSPredParam@TSPred.list[[TSPred]][-1])

        out <- do.call(Function, Param.List)
        out[, TSPred := TSPred]
        return(out)

      })


      output <- rbindlist(Results)
      IDQuals <- setdiff(names(output), c(paste0('Pred', VarNames), paste0('STD', VarNames), 'TSPred'))

      Results <- lapply(VarNames, function(var){

        cols <- names(output)[grep(paste0(var,'$'), names(output))]
        DT <- copy(output)[, c(IDQuals, cols, 'TSPred'), with = FALSE]
        setnames(DT, cols[2], 'STD')
        STD.na <- DT[, all(is.na(STD)), by = IDQuals]
        STD.na <- STD.na[V1 == TRUE]
        if (dim(STD.na)[1] > 0) {

          STD.na <- STD.na[, (cols[1]) := as.numeric(NA)]
          STD.na <- STD.na[, (cols[2]) := as.numeric(NA)]
          STD.na[, V1 := NULL]
          units.na <- STD.na[[IDQuals]]
          DT <- DT[!(DT[[IDQuals]] %in% units.na)]
        }
        MinSTD.index <- DT[, which.min(STD), by = IDQuals]
        setnames(MinSTD.index, 'V1', 'TSPred')
        out <- merge(DT, MinSTD.index, by = c(IDQuals, 'TSPred'))
        out[, TSPred := NULL]
        setnames(out, 'STD', cols[2])
        if (dim(STD.na)[1] > 0) out <- rbind(STD.na, out)

        return(out)
      })

      output <- Reduce(merge, Results)


    return(output)

  })

#' @title Method to predict according to the regular difference time series model.
#'
#' @description This method implements the predicted value and their standard deviation according to
#' the regular difference time series model \eqn{(1-B)y_{t}=a_{t}}{(1-B)y<sub>t</sub>=a<sub>t</sub>}.
#'
#' @param x \code{Vector} or object of class \linkS4class{StQList} upon which the prediction will be
#' made.
#'
#' @param VarNames character vector with the variable names for which the prediction will be made;
#' by default it is NULL.
#'
#' @param frequency integer indicating the frequency of the time periods in the time series; by
#' default it is 12L.
#'
#' @param forward integer indicating the number of periods ahead when the prediction will be made;
#' by default it is 2L.
#'
#' @return It returns a \code{data.table} with components Pred and STD, containing the point
#' prediction and the estimated standard deviations, respectively, for each variable.
#'
#' @examples
#'
#' # Predicting one and two months ahead in time
#' data(Example1.TS)
#' RegDiffTSPred(Example1.TS, forward = 1L)
#' RegDiffTSPred(Example1.TS, forward = 2L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' RegDiffTSPred(Example2.TS, forward = 1L)
#'
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' RegDiffTSPred(StQListExample, VarNames)
#' }
#'
#' @import forecast imputeTS data.table StQ RepoTime parallel
#'
#' @export
setGeneric("RegDiffTSPred", function(x, VarNames, frequency = 12L, forward = 2L){
    standardGeneric("RegDiffTSPred")})
#'
#' @rdname RegDiffTSPred
#'
#' @export
setMethod(
    f = "RegDiffTSPred",
    signature = c("vector"),
    function(x, VarNames, frequency = 12L, forward = 2L){

        x <- as.numeric(x)
        x[is.infinite(x)] <- NA_real_

        if (all(is.na(x))) {

            output <- data.table(Pred = NA_real_, STD = NA_real_)
            return(output)

        }

        ini <- which.min(is.na(x))
        last <- length(x)
        x <- x[ini:last]


        # vectors with not enough observations returns NA
        x.aux <- x[!is.na(x)]

        if (length(x) == 0 | length(x.aux) == 1) return(data.table(Pred = NA_real_, STD = NA_real_))

        if (length(x.aux) <= 3) return(data.table(Pred = x.aux[length(x.aux)], STD = sd(x.aux)))

        if (length(rle(x.aux)$values) == 1) {

            #x <- imputeTS::na_kalman(x, model = 'auto.arima') # Needs at least 3 non-NA data point
            x[is.na(x)] <- rle(x.aux)$values

        } else {

            ejecuc <- try(
                x <- imputeTS::na_kalman(x) # , type = 'level' Needs at least 3 non-NA data point
            , silent = TRUE)
            if (inherits(ejecuc, "try-error")) {
                x <- imputeTS::na_kalman(x, type = 'trend') # type by default "BSM", other option: "trend"
            }

        }


        x <- ts(x, frequency = frequency)

        fit <- forecast::Arima(x, order = c(0, 1, 0), seasonal = c(0, 0, 0))

        out <- forecast::forecast(fit, h = forward, level = 0.95)
        std <- (out$upper[forward] - out$lower[forward]) / (2 * 1.96)
        output <- list(Pred = out$mean[forward], STD = std)
        output <- data.table(Pred = output$Pred, STD = output$STD)
        return(output)

    }
)
#'
#' @rdname RegDiffTSPred
#'
#' @export
setMethod(
    f = "RegDiffTSPred",
    signature = c("StQList"),
    function(x, VarNames, frequency = 12L, forward = 2L){

        if (length(VarNames) == 0) stop('[RegDiffTSPred StQList] The input parameter VarNames must be specified.\n')

        x_StQ <- StQListToStQ(x)
        VNC <- getVNC(getDD(x_StQ))$MicroData
        IDQuals <- unique(VNC[['IDQual']])
        IDQuals <- IDQuals[IDQuals != '' & IDQuals != 'Period']
        DT <- dcast_StQ(x_StQ, ExtractNames(VarNames))
        DT[, orderPeriod := orderRepoTime(Period), by = IDQuals]
        setkeyv(DT, c(IDQuals, 'orderPeriod'))
        VarNames <- intersect(VarNames, names(DT))
        if (length(VarNames) == 0) {

            stop('[TSPred::RegDiffTSPred] No VarNames present in StQList object.\n')

        }

        if (length(VarNames) == 1) {

            output <- DT[ ,RegDiffTSPred(get(VarNames), frequency = frequency, forward = forward),
                          by = IDQuals]
            setnames(output, c('Pred', 'STD'), paste0(c('Pred', 'STD'), VarNames))

        } else {

            n_cores <- floor(detectCores() / 2 - 1)
            clust <- makeCluster(n_cores)

            clusterExport(clust, c("VarNames", 'frequency', 'forward', 'DT', 'IDQuals'), envir = environment())
            clusterEvalQ(clust, library(data.table))
            clusterEvalQ(clust, library(TSPred))

            output <- parLapply(clust, VarNames, function(var){

                out <- DT[ ,RegDiffTSPred(get(var), frequency = frequency, forward = forward), by = IDQuals]

                return(out)

            })

            stopCluster(clust)

            names(output) <- VarNames
            output <- lapply(seq_along(output), function(n){
                setnames(output[[n]], c('Pred', 'STD'), paste0(c('Pred', 'STD'), names(output[n])))})
            output <- Reduce(merge, output)

        }

        return(output)
    }
)

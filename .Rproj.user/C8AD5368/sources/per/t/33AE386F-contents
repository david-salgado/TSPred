#' @title Convert an \link{StQ} object into a dcasted \linkS4class{data.table}
#'
#' @description \code{dcast_StQ} returns a \linkS4class{data.table} in dcasted form (observations by
#' row and variables by columns) with data from the input \link{StQ} object.
#'
#' This method converts the slot \code{Data} from the input \code{StQ} object into a
#' \linkS4class{data.table} with statistical units by row and variables specified in the input
#' parameter \code{VarNames} by columns.
#'
#' To distinguish between variables and qualifiers this function makes use of the slot \code{DD} of
#' input \link{StQ} variable.
#'
#' This method is indeed a wrapper for the function \code{\link[data.table]{dcast.data.table}} of
#' the package \linkS4class{data.table}, adapted to the structure of object \link{StQ}.
#'
#' @param object Object of class \link{StQ} whose slot \code{Data} will be converted.
#'
#' @param VarNames \code{Character} vector with names of the output variables (default \code{NULL}).
#'
#' @param UnitNames \code{TRUE} or \code{FALSE} (default) to return output with UnitNames in dcasted
#'  form.
#'
#' @return Returns a \linkS4class{data.table} with data from slot \code{Data} of the input
#' \link{StQ} object with statistical units by rows and variables by columns. Only variables
#' in \code{VarNames} will be output. If no variable name is specified, all variables in the input
#' object will be output.
#'
#' @examples
#' data(ExampleStQ)
#' Mat <- dcast_StQ(ExampleStQ, VarNames = 'Turnover')
#' Mat
#' str(Mat)
#'
#' dcast_StQ(ExampleStQ, VarNames = 'Employees')
#'
#' dcast_StQ(ExampleStQ[ID != ''])
#'
#' @seealso \code{\link{melt_StQ}}, \code{\link[data.table]{dcast.data.table}},
#' \code{\link[data.table]{melt.data.table}}, \code{\link[reshape2]{melt}},
#' \code{\link[reshape2]{dcast}}
#'
#' @include StQ.R DDslotWith.R getNonIDQual.R getDD.R getData.R getVNC.R getIDQual.R VarNamesToFormula.R sub.StQ.R getIDDD.R ExtractNames.R getUnits.R ParseUnitName.R getClass.R getVariables.R
#' 
#' @importFrom formula.tools lhs.vars
#'
#' @importFrom stats as.formula
#'
#' @import data.table
#'
#' @export
setGeneric("dcast_StQ",
           function(object, VarNames = NULL, UnitNames = FALSE){standardGeneric("dcast_StQ")})

#' @rdname dcast_StQ
#'
#' @export
setMethod(
    f = "dcast_StQ",
    signature = c("StQ"),
    function(object, VarNames = NULL, UnitNames = FALSE){
        
        IDQual_R <- getIDQual(object)
        dotQual_R <- getDotQual(object)
        allQual <- union(IDQual_R, dotQual_R)
        if (UnitNames) {
            
            allQual <- IDDDToUnitNames(allQual_R, DD)
            
        }
        
        IDDDs <- getVariables(object)
        invalidIDDD <- VarNames[!VarNames %in% IDDDs]
        if (length(invalidIDDD) != 0) {
            
            stop(paste0('[StQ::dcast_StQ] The following IDDDs in the input parameter VarNames are not valid: ', 
                        paste0(invalidIDDD, collapse = ', '),
                        '\n Only IDDDs (without qualifiers) are allowed in the input parameter VarNames.\n',
                        'If you are interested in a particular variable, subset the output dcasted data.table.\n'))
        }
        
        if (!is.null(VarNames)) IDDDs <- VarNames
        
        # For each reshape formula we create a data.table to dcast
        DD <- getDD(object)
        IDQuals_UnitName <- IDDDToUnitNames(getIDQual(DD), DD)
        varClasses <- getClass(object)
return(list(getData(object), IDDDs))
        Data <- getData(object)[IDDD %chin% IDDDs]
        IDDDs_in_Data <- unique(Data[['IDDD']])
        formulas.dt <- VarNamesToFormula(IDDDs_in_Data, DD)
        allVars_inForm <- unique(Reduce(c, lapply(formulas.dt$Form, function(x){all.vars(as.formula(x))})))
        allQual <- intersect(allQual, allVars_inForm)
        IDDDs_by_form <- split(formulas.dt[['Variable']], formulas.dt[['Form']])
        Data_byform_dcasted <- lapply(names(IDDDs_by_form), function(formla){
            
            tempData <- Data[IDDD %chin% IDDDs_by_form[[formla]]]
            tempDataCols <- names(tempData)
            setkeyv(tempData, setdiff(tempDataCols, 'Value'))
            dupTempData <- tempData[duplicated(tempData, by = key(tempData))]
            if (dim(dupTempData)[[1]] > 0) {
                
                warning(paste0('[StQ::dcast_StQ] There exist duplicated rows in the component ',
                               formla,
                               '.\n The table will be reformatted with the default agg.fun function (length).\n'))
            }
            
            vars_in_formla <- all.vars(as.formula(formla))
            quals_in_formla_notin_Data <- setdiff(vars_in_formla, tempDataCols)
            if (length(quals_in_formla_notin_Data) > 0) {
                
                tempData[, (quals_in_formla_notin_Data) := '']
            }
            
            tempData <- tempData[, c(vars_in_formla, 'Value'), with = F]
            tempData_dcasted <- data.table::dcast.data.table(
                data = tempData,
                formula = as.formula(formla),
                drop = TRUE,
                value.var = 'Value')
#return(tempData_dcasted)
            tempDataCols <- sort(names(tempData_dcasted))
            for (col in tempDataCols){
                
                if (all(is.na(tempData_dcasted[[col]]))) tempData_dcasted[, (col) := NULL]
                if (col == '.') tempData_dcasted[, (col) := NULL]
                
            }
#return(list(tempData_dcasted, varClasses))            
            for (col in names(tempData_dcasted)){
                
                colClass <- varClasses[[ExtractNames(col)]]
                tempData_dcasted[, (col) := as(get(col), colClass)]
            }
#return(tempData_dcasted)            
            if (UnitNames) {
                
                unitNames <- IDDDToUnitNames(names(tempData_dcasted), DD)
                invalidIDDDnames <- names(tempData_dcasted)[is.na(unitNames)]
                if (length(invalidIDDDnames) > 0) {
                    
                    stop(paste0('[StQ::dcast_StQ] The following IDDDnames are not contained in the data dictionary: ',
                                paste0(invalidIDDDnames, collapse = ', ')))
                }
                setnames(tempData_dcasted, unitNames)
#return(tempData_dcasted)
                colNames_UnitName <- names(tempData_dcasted)
                localIDQuals <- intersect(IDQuals_UnitName, colNames_UnitName)
                otherCols <- setdiff(colNames_UnitName, localIDQuals)
                metaStrings <- stringr::str_extract_all(otherCols, '(?<=\\[).+?(?=\\])')
                otherCols_nonMeta <- otherCols[sapply(metaStrings, function(str) length(str) == 0)]
                otherCols_Meta <- otherCols[sapply(metaStrings, function(str) length(str) != 0)]
                metaStrings <- unique(unlist(metaStrings))
                metaStrings <- metaStrings[metaStrings != '']
                otherCols_Meta <- c(otherCols_Meta, metaStrings)
                otherCols_nonMeta <- setdiff(otherCols_nonMeta, metaStrings)
                tempData_dcasted_parsed <- tempData_dcasted[
                    , c(localIDQuals, otherCols_nonMeta), with = FALSE]
                tempData_dcasted_Meta <- tempData_dcasted[
                    , c(localIDQuals, otherCols_Meta), with = FALSE]
#return(list(tempData_dcasted_parsed, tempData_dcasted_Meta, metaStrings))
                for (mStr in metaStrings){
                    
                    pattrn <- paste0('\\[', mStr, '\\]')
                    localMetaCols <- otherCols_Meta[grep(pattrn, otherCols_Meta)]
                    allLocalCols <- unique(c(localIDQuals, mStr, localMetaCols))
                    tempDT <- tempData_dcasted_Meta[, ..allLocalCols]
                    
                    valueVar <- gsub(paste0('_\\[', mStr, '\\]'), '', localMetaCols)
                    
                    formla <- paste(
                        paste0(localIDQuals, collapse = ' + '),
                        mStr,
                        sep = ' ~ '
                    )
                    
                    
                    setnames(tempDT, localMetaCols, valueVar)
                    
#return(list(tempDT, formla))                    
                    flagDel <- FALSE
                    
                    # Add an auxiliary variable to be sure dcast keep the names
                    # even when valueVar has length 1.
                    if(length(valueVar) == 1){
                        tempDT[, '.aux' := NA]
                        valueVar <- c(valueVar, '.aux')
                        flagDel <- TRUE
                    }
                    
                    tempDT_parsed <- data.table::dcast.data.table(
                        data = tempDT,
                        formula = as.formula(formla),
                        drop = TRUE,
                        value.var = valueVar)
#return(tempDT_parsed)                    
                    if(flagDel){
                        
                        set(tempDT_parsed, j = grep('.aux',  names(tempDT_parsed)), value = NULL)
                        
                    }
#return(list(tempData_dcasted_parsed, tempDT_parsed))                    
                    tempData_dcasted <- merge(tempData_dcasted_parsed, tempDT_parsed, 
                                                     by = intersect(names(tempData_dcasted_parsed),
                                                                    names(tempDT_parsed)),
                                                     all = TRUE)
                }
                
            }
            
            newIDQual <- setdiff(allQual, names(tempData_dcasted))
            for (col in newIDQual){

                tempData_dcasted[, (col) := '']
            }
            
            return(tempData_dcasted)
        })
        
#names(Data_byform_dcasted) <- names(IDDDs_by_form)
#return(Data_byform_dcasted)        
        
        Data_dcasted <- Reduce(
            
            function(x, y) {
                
                if (length(intersect(names(x), names(y))) > 0){
                    
                    combinedDT <- merge(x, y, all = TRUE, by = intersect(names(x), names(y)))
                    
                } else {
                    
                    combinedDT <- rbindlist(list(x, y), fill = TRUE)
                    
                }
                
                return(combinedDT)
            }, Data_byform_dcasted)
        
        return(Data_dcasted[])
        
    }
)

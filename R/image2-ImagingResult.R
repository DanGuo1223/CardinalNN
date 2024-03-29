
setMethod("image", c(x = "SparseImagingResult"),
          function(x, formula,
                   model = modelData(x),
                   superpose = is_matrix,
                   ...,
                   column,
                   colorscale = cividis,
                   colorkey = !superpose,
                   alpha.power = 2)
          {
            .checkForIncompleteProcessing(x)
            args <- .parseFormula2(formula)
            if ( length(args$lhs) != 1L )
              .stop("lhs of formula must include exactly 1 variable")
            if ( !is.null(args$g) )
              .stop("conditioning variables via | not allowed")
            is3d <- length(args$rhs) == 3L
            is_matrix <- !is.numeric_vector(resultData(x, 1, names(args$lhs)))
            newx <- .reshape_pixel_results(x, names(args$lhs))
            formula2 <- paste0(c(deparse(formula), "model"), collapse="|")
            formula2 <- as.formula(formula2)
            if ( is.null(names(model)) ) {
              feature1 <- subset_rows(fData(newx), list(model_id=model))
            } else {
              model <- model[names(model) %in% names(fData(newx))]
              feature1 <- subset_rows(fData(newx), as.list(model))
            }
            ncols <- sort(unique(fData(newx)[["column"]]))
            nc <- length(ncols)
            if ( missing(column) )
              column <- ncols
            if ( is.numeric(column) ) {
              feature2 <- subset_rows(fData(newx), list(column_id=column))
            } else {
              feature2 <- subset_rows(fData(newx), list(column=column))
            }
            feature <- intersect(feature1, feature2)
            if ( is_matrix ) {
              feature.groups <- featureData(newx)[["column"]][feature]
            } else {
              feature.groups <- NULL
            }
            image(newx, formula=formula2, feature=feature,
                  feature.groups=feature.groups, superpose=superpose,
                  colorscale=colorscale, colorkey=colorkey,
                  alpha.power=alpha.power, ...)
          })

# format pixel data

.reshape_pixel_results <- function(object, name) {
  data <- lapply(resultData(object), function(res) {
    res <- res[[name, exact=FALSE]]
    if ( is.factor(res) || is.character(res) ) {
      .factor_matrix(res)
    } else if ( is.vector(res) ) {
      as.matrix(res)
    } else {
      res
    }
  })
  cols <- lapply(data, function(x) {
    if ( is.null(colnames(x))) {
      paste0(seq_len(ncol(x)))
    } else {
      colnames(x)
    }
  })
  pnm <- names(modelData(object))
  fdata <- lapply(seq_along(cols), function(i) {
    par <- as.list(modelData(object)[i,pnm,drop=FALSE])
    par[["model"]] <- .format.data.labels(par)
    par[["model"]] <- factor(par$model, levels=unique(par$model))
    par[["column"]] <- cols[[i]]
    len <- length(cols[[i]])
    par1 <- DataFrame(model_id=i, column_id=seq_len(len))
    par2 <- DataFrame(par)		
    cbind(par1, par2)
  })
  fdata <- do.call("rbind", fdata)
  data <- SimpleList(t(do.call("cbind", data)))
  names(data) <- name
  SparseImagingExperiment(imageData=data,
                          featureData=fdata,
                          pixelData=pixelData(object))
}


# format formula

.formula_pixel_results <- function(x, lhs, is3d = FALSE) {
	if ( is3d ) {
		fm <- paste0(lhs, "~", paste0(coordnames(x)[1:3], collapse="*"))
	} else {
		fm <- paste0(lhs, "~", paste0(coordnames(x)[c(1,2)], collapse="*"))
	}
	formula <- as.formula(fm, env=parent.frame(3))
}




## miCNN
setMethod("image",
          signature = c(x = "MiCNN_classification"),
          function(x, formula, values = c("class"), ...)
          {
            if ( missing(formula) )
              formula <- .formula_pixel_results(x, match.arg(values))
            callNextMethod(x, formula=formula, ...)
            #Cardinal::image(x, formula=formula, ...)
          })

setMethod("image3D",
          signature = c(x = "MiCNN_classification"),
          function(x, formula, values = c ("class"), ...)
          {
            if ( missing(formula) )
              formula <- .formula_pixel_results(x, match.arg(values), is3d=TRUE)
            callNextMethod(x, formula=formula, ...)
          })







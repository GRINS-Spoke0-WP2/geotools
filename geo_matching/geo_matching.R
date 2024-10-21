geo_matching <- function(data,
                         format = NULL) {
  if (is.null(format)) {
    format <- list("xyt","xyt")
  }
  #data[[1]]
  if (format[[1]] == "xyt") {
    grid1.df <- data[[1]]
    names(grid1.df) <- c("longitude", "latitude", "time", "var1")
  } else if (format1 == "grid") { #da matrice a xyt
    grid1.df <- data.frame(
      longitude = rep(rep(dimnames(data[[1]])[[2]], each = dim(data[[1]])[1]), dim(data[[1]])[3]),
      latitude = rep(dimnames(data[[1]])[[1]], dim(data[[1]])[2] * dim(data[[1]])[3]),
      time = rep(as.Date(as.numeric(dimnames(
        data[[1]]
      )[[3]])), each = dim(data[[1]])[2] * dim(data[[1]])[1]),
      var1 = c(data[[1]])
    )
  } else{
    stop("format 1 unknwon")
  }
  #data[[2]]
  if (format[[2]] == "xyt") {
    grid2.df <- data[[2]]
    names(grid2.df) <- c("longitude", "latitude", "time", "var2")
  } else if (format2 == "grid") {
    grid2.df <- data.frame(
      longitude = rep(rep(dimnames(data[[2]])[[2]], each = dim(data[[2]])[1]), dim(data[[2]])[3]),
      latitude = rep(dimnames(data[[2]])[[1]], dim(data[[2]])[2] * dim(data[[2]])[3]),
      time = rep(as.Date(as.numeric(dimnames(
        data[[2]]
      )[[3]])), each = dim(data[[2]])[2] * dim(data[[2]])[1]),
      var2 = c(data[[2]])
    )
  } else{
    stop("format 2 unknwon")
  }
  
}
library(sp)
library(spacetime)
library(RColorBrewer)
# library(sf)

geo_matching <- function(data, settings = NULL) {
  ndata <- length(data)
  if (is.null(settings)) {
    settings <- .empty_settings()
  }
  settings <- .input_check(settings, ndata)
  grid.df <- .create_df(data, settings)
  STs <- .create_STs(ndata, grid.df, settings)
  .check_sp(STs, ndata) #spatial check
  over_ST <- over(STs[[1]], STs[[2]])
  STs[[1]]@data <- cbind(STs[[1]]@data, over_ST[, ncol(over_ST)])
  if (settings$format[2] == "matrix") {
    names(STs[[1]]@data)[ncol(STs[[1]]@data)] <- paste0("matrix_", 2)
  }
  
  if (ndata > 2) {
    for (i in 3:ndata) {
      over_ST <- over(STs[[1]], STs[[i]])
      STs[[1]]@data <- cbind(STs[[1]]@data, over_ST[, ncol(over_ST)])
      if (settings$format[i] == "matrix") {
        names(STs[[1]]@data)[ncol(STs[[1]]@data)] <- paste0("matrix_", i)
      }
    }
  }
  return(STs[[1]]@data)
}

#initial conditions
.empty_settings <- function() {
  settings <- list(
    format = NULL,
    type = NULL,
    crs = NULL,
    varnames = NULL
  )
  return(settings)
}
.input_check <- function(settings, ndata) {
  settings <- .check_format(settings, ndata)
  settings <- .check_type(settings, ndata)
  settings <- .check_crs(settings, ndata)
  return(settings)
}
.check_format <- function(settings, ndata) {
  #accepted: xyt matrix
  if (is.null(settings$format)) {
    settings$format <- rep("xyt", ndata)
  }
  return(settings)
}
.check_type <- function(settings, ndata) {
  #accepted: points grid
  if (is.null(settings$type)) {
    settings$type <- rep("points", ndata)
  }
  return(settings)
}
.check_crs <- function(settings, ndata) {
  if (is.null(settings$crs)) {
    settings$crs <- rep("EPSG:4326", ndata)
  } else{
    settings$crs <- paste0("EPSG:", settings$crs)
  }
  return(settings)
}

.create_df <- function(data, settings) {
  ndata <- length(data)
  grid.df <- list()
  for (i in 1:ndata) {
    #aggiungere poligoni (ora solo punti e griglia)
    print(paste("reading data", i))
    if (settings$format[i] == "xyt") {
      if (nrow(unique(data[[i]][, 1:3])) != nrow(data[[i]])) {
        stop("multiple records for same space-time locations")
      }
      grid.df[[i]] <- data[[i]]
      names(grid.df[[i]])[1:3] <-
        c("longitude", "latitude", "time")
    } else if (format[i] == "matrix") {
      #da matrice a xyt
      grid.df[[i]] <- data.frame(
        longitude = rep(rep(dimnames(data[[i]])[[2]], each = dim(data[[i]])[1]), dim(data[[i]])[3]),
        latitude = rep(dimnames(data[[i]])[[1]], dim(data[[i]])[2] * dim(data[[i]])[3]),
        time = rep(as.Date(as.numeric(
          dimnames(data[[i]])[[3]]
        )), each = dim(data[[i]])[2] * dim(data[[i]])[1]),
        var = c(data[[i]])
      )
      names(grid.df[[i]])[4] <- paste0("var", i)
    } else if (format[i] == "shp") {
      grid.df[[i]] <- st_drop_geometry(data[[i]])
      next()
    } else{
      stop(paste("format", i, "unknwon"))
    }
    grid.df[[i]]$longitude <- as.numeric(grid.df[[i]]$longitude)
    grid.df[[i]]$latitude <- as.numeric(grid.df[[i]]$latitude)
    grid.df[[i]]$time <- as.Date(grid.df[[i]]$time)
    grid.df[[i]] <- grid.df[[i]][order(grid.df[[i]]$time,
                                       grid.df[[i]]$longitude,
                                       grid.df[[i]]$latitude), ]
    print("done")
  }
  return(grid.df)
}

.create_STs <- function(ndata, grid.df, settings) {
  STs <- list()
  for (i in 1:ndata) {
    print(paste("converting data", i, "to ST"))
    # sp <- unique(grid.df[[i]][,1:2]) #too slow
    sp <-
      grid.df[[i]][grid.df[[i]]$time == grid.df[[i]]$time[1], 1:2]
    if (settings$type[i] == "points") {
      coordinates(sp) <- c("longitude", "latitude")
      slot(sp, "proj4string") <- CRS(SRS_string = settings$crs[i])
      t <- unique(grid.df[[i]][, 3])
    } else if (settings$type[i] == "grid") {
      coordinates(sp) <- c("longitude", "latitude")
      slot(sp, "proj4string") <- CRS(SRS_string = settings$crs[i])
      gridded(sp) <- TRUE
      t <- unique(grid.df[[i]][, 3])
    } else if (settings$type[i] == "polygons") {
      sp <- data[[i]]
      if (any(class$sp == "sf")) {
        sp <- st_make_valid(sp)
        sp <- as_Spatial(sp)
        t <- unique(grid.df[[1]][, 3])
        grid.df[[i]] <-
          grid.df[[i]][rep(1:nrow(grid.df[[i]]), length(t)), ]
      } else{
        stop("polygons should be in sf format")
      }
    } else {
      stop(paste("format of data", i, "unknown"))
    }
    if ((length(sp) * length(t)) == nrow(grid.df[[i]])) {
      STs[[i]] <- STFDF(sp, t, grid.df[[i]])
    }
    print("done")
  }
  return(STs)
}

.check_sp <- function(STs, ndata) {
  cols <- brewer.pal(9, "Set1")
  for (i in 2:ndata) {
    if (settings$format[i] == "shp") {
      next()
    }
    plot(STs[[1]]@sp, main = paste("data 1 + data", i))
    plot(STs[[i]]@sp, col = sample(cols, 1), add = T)
  }
}
############# TRASH #############
# .extract_varnames <- function(settings, ndata, grid.df) {
#   if (is.null(settings$varnames)) {
#     for (i in 1:ndata) {
#       if (settings$format[i] == "xyt") {
#         settings$varnames[i] <- names(grid.df[[i]])[4]
#       }
#       if (settings$format[i] == "matrix") {
#         settings$varnames[i] <- paste0("y_", i)
#       }
#     }
#   }
#   return(settings)
# }

# settings <- .extract_varnames()

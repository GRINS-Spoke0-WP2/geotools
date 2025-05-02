#' @title Align heterogeneous space-time data onto a common grid
#' @name geomatching
#'
#' @description
#' Aligns input space-time data from different spatial grids onto a specific
#' common grid, even when their geographic reference systems may differ.
#' This procedure is known as \strong{spatial overlay}.
#'
#' @usage geomatching(data, settings = NULL, check_sp = FALSE)
#'
#' @param data List of space-time datasets, each either a \code{data.frame} or a
#' 3D \code{matrix}. The spatial grid of the first element is used as the
#' reference grid for aligning all others.
#' @param settings Named list with the fields \strong{format}, \strong{type}
#' and \strong{crs}. \code{"format"} defines the data format for each input:
#' use \code{"xyt"} if the i-th input is a data.frame with three columns
#' representing x-coordinate, y-coordinate and time; use \code{"matrix"} if the
#' i-th input is a 3D array where the first, second, and third dimensions
#' correspond to x, y, and time respectively. \code{"type"} specifies the
#' geometry type used for matching, e.g., \code{"points"} or \code{"grid"}.
#' \code{"crs"} sets the Coordinate Reference System as an EPSG code.
#' @param check_sp Logical flag to enable spatial consistency checks
#' (currently not implemented).
#'
#' @return A \strong{data.frame} containing the matched data aligned on the
#' reference grid.
#'
#' @examples
#' # SEE "demo.Rmd" FOR MORE DETAILS
#'
#' \dontrun{
#' res_geomatch <- geomatching(
#'   data=list(AQ_EEA_NO2, AQ_CAMS_NO2),
#'   settings = list(
#'     "format"=list("xyt", "matrix"),
#'     "type"=list("points", "grid"),
#'     "crs"=list(4979, 4326)
#'   )
#' )}
#'
#' @seealso \url{https://github.com/GRINS-Spoke0-WP2/geotools/blob/develop/demo/demo.Rmd}
#'
#' @export
#'
#' @importFrom sp coordinates CRS gridded
#' @importFrom spacetime STFDF over
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sf st_drop_geometry st_make_valid st_transform st_geometry as_Spatial

# points and GRIDs ONLY IN WGS84 - EPSG 4326
geomatching <- function(data,
                        settings = NULL,
                        check_sp = FALSE) {

  ndata <- length(data)
  if (is.null(settings)) {
    settings <- .empty_settings()
  }
  settings <- .input_check(settings, ndata)
  grid.df <- .create_df(data, settings)
  STs <- .create_STs(data, grid.df, settings)

  # if (check_sp) {
  #   .check_sp(STs, ndata)
  # }#spatial check
  # over_ST <- spacetime::over(STs[[1]], STs[[2]])
  # if (any(settings$format == "shp")) {
  #   nshp <- which(settings$format == "shp")
  # }

  for (i in 2:ndata) {
    over_ST <- spacetime::over(STs[[1]], STs[[i]])

     # if (i == nshp) {
     #  STs[[1]]@data <- cbind(STs[[1]]@data, over_ST)
     # } else{

    STs[[1]]@data <- cbind(STs[[1]]@data, over_ST[, 4:ncol(over_ST), drop=FALSE])
    if (settings$format[i] == "matrix") {
      names(STs[[1]]@data)[ncol(STs[[1]]@data)] <- paste0("matrix_", i)
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
  #accepted: xyt, matrix
  if (is.null(settings$format)) {
    settings$format <- rep("xyt", ndata)
  }
  return(settings)
}
.check_type <- function(settings, ndata) {
  #accepted: points, grid
  if (is.null(settings$type)) {
    settings$type <- rep("points", ndata)
  }
  return(settings)
}
.check_crs <- function(settings, ndata) {
  if (is.null(settings$crs)) {
    if (any(settings$format == "shp")) {
      stop("crs of shp data must be specified")
    }
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
    print(paste("reading data", i))
    if (settings$format[i] == "xyt") {
      if (nrow(unique(data[[i]][, 1:3])) != nrow(data[[i]])) {
        stop("multiple records for same space-time locations")
      }
      grid.df[[i]] <- data[[i]]
      names(grid.df[[i]])[1:3] <-
        c("longitude", "latitude", "time")
    } else if (settings$format[i] == "matrix") {
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
    } else if (settings$format[i] == "shp") {
      grid.df[[i]] <- df::st_drop_geometry(data[[i]])
      print("done")
      next()
    } else if (settings$format[i] == "stfdf") {
      next()
    }
    else{
      stop(paste("format of data", i, "unknwon"))
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

.create_STs <- function(data, grid.df, settings) {
  STs <- list()
  ndata <- length(data)
  for (i in 1:ndata) {
    print(paste("converting data", i, "to ST"))
    # sp <- unique(grid.df[[i]][,1:2]) #too slow
    sp <-
      grid.df[[i]][grid.df[[i]]$time == grid.df[[i]]$time[1], 1:2]
    if (settings$type[i] == "points") {
      sp::coordinates(sp) <- c("longitude", "latitude")
      slot(sp, "proj4string") <- sp::CRS(SRS_string = settings$crs[i])
      t <- unique(grid.df[[i]][, 3])
    } else if (settings$type[i] == "grid") {
      sp::coordinates(sp) <- c("longitude", "latitude")
      slot(sp, "proj4string") <- sp::CRS(SRS_string = settings$crs[i])
      sp::gridded(sp) <- TRUE
      t <- unique(grid.df[[i]][, 3])
    } else if (settings$type[i] == "polygons") {
      sp <- data[[i]]
      if (any(class(sp) == "sf")) {
        sp <- sf::st_make_valid(sp)
        if (settings$crs[i] != settings$crs[1]) {
          sp <- sf::st_transform(sp, crs = st_crs(settings$crs[1]))
        }
        sp <- sf::st_geometry(sp)
        sp <- sf::as_Spatial(sp)
        ndt <- which(settings$format %in% c("xyt", "matrix"))[1]
        t <- unique(grid.df[[ndt]][, 3])
        grid.df[[i]] <-
          grid.df[[i]][rep(1:nrow(grid.df[[i]]), length(t)), ]
      } else{
        stop("polygons should be in sf format")
      }
    } else if (settings$type[i] == "stgrid") {
      next()
    } else {
      stop(paste("format of data", i, "unknown"))
    }
    if ((length(sp) * length(t)) == nrow(grid.df[[i]])) {
      STs[[i]] <- spacetime::STFDF(sp, t, grid.df[[i]])
    } else {
      stop("space x time different from number of rows of dataset")
    }
    print("done")
  }
  return(STs)
}

.check_sp <- function(STs, ndata) {
  cols <- RColorBrewer::brewer.pal(9, "Set1")
  for (i in 2:ndata) {
    if (settings$format[i] == "shp") {
      next()
    }
    plot(STs[[1]]@sp, main = paste("data 1 + data", i))
    plot(STs[[i]]@sp, col = sample(cols, 1), add = T)
  }
}

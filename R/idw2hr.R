#' @title Interpolate data onto a high-resolution grid using IDW
#' @name idw2hr
#'
#' @description
#' Interpolates input space-time data onto a high-resolution spatial grid using
#' \strong{IDW} (\strong{Inverse Distance Weighting}). The goal is to assign
#' (interpolated) observations within the boundaries of smaller municipalities
#' (which are polygons) as well. This function supports parallel computing.
#'
#' @usage idw2hr(data, crs = 4326, outgrid_params = NULL, col_names = NULL,
#' interest_vars = NULL, idp = 1, nmax = 4, ncores = 1, restore_NA = FALSE)
#'
#' @param data Space-time dataset in \code{data.frame} or \code{array} format.
#' @param crs Coordinate Reference System (CRS) for the input data, given as an
#' EPSG code. If it differs from \code{4326} (\strong{WGS 84}), then data will
#' be reprojected accordingly.
#' @param outgrid_params Named list with the fields \strong{resolution},
#' \strong{min_lon}, \strong{max_lon}, \strong{min_lat} and \strong{max_lat}.
#' These numeric parameters specify the resolution and geographic bounds of the
#' output high-resolution grid. All values must refer to the CRS \strong{4326}.
#' If not specified, then the resolution defaults to \code{0.01} and the
#' geographic bounds are automatically inferred from the input data.
#' @param col_names Named list with the field \strong{coords_x},
#' \strong{coords_y} and \strong{t}. These parameters specify the names of the
#' columns representing the x-coordinate, the y-coordinate and time respectively.
#' If not specified, then default column names \code{"longitude"},
#' \code{"latitude"} and \code{"time"} are assumed.
#' @param interest_vars List specifying the names of the variables to be
#' interpolated using IDW. If not specified, then all the available variables
#' will be interpolated.
#' @param idp Inverse distance power parameter used in IDW interpolation.
#' Controls how strongly the weighting decreases with distance. Higher values
#' give more influence to nearby points. Default value is \code{1}.
#' @param nmax Number of points to take into account to perform IDW. Default
#' value is \code{4}.
#' @param ncores Number of CPU cores to use for parallel processing.
#' Default value is \code{1}, then computation is performed sequentially. Higher
#' values enable parallel execution to improve performance on large datasets. If
#' \code{ncores} exceeds the number of available cores, the number of available
#' cores will be used instead.
#' @param restore_NA If \code{TRUE}, the function will propagate NA values from
#' the low-resolution dataset to the high-resolution dataset by setting
#' corresponding points and their neighbors to NA. If \code{FALSE}, the function
#' will leave the high-resolution dataset unchanged regarding NA values. Default
#' is \code{FALSE}.
#'
#' @return A \code{data.frame} resulting from interpolation onto a
#' high-resolution grid using IDW.
#'
#' @examples
#' # SEE "demo.Rmd" FOR MORE DETAILS
#'
#' \dontrun{
#' res_idw2hr <- idw2hr(
#'   data = input_data,
#'   crs = 4979,
#'   outgrid_params = list(
#'     "resolution" = 0.02,
#'     "min_lon" = 6,
#'     "max_lon" = 7,
#'     "min_lat" = 42,
#'     "max_lat" = 43
#'   ),
#'   col_names = list(
#'     "coords_x" = "x",
#'     "coords_y" = "y",
#'     "t" = "time"
#'   ),
#'   interest_vars = list(
#'     "altitude",
#'     "AQ_EEA_NO2",
#'     "AQ_CAMS_NO2"
#'   ),
#'   idp = 1,
#'   ncores = 4
#' )}
#'
#' @seealso \url{https://github.com/GRINS-Spoke0-WP2/geotools/blob/develop/demo/demo.Rmd}
#'
#' @export
#'
#' @importFrom sp coordinates proj4string gridded CRS
#' @importFrom sf st_as_sf st_transform
#' @importFrom gstat idw
#' @importFrom dplyr rename %>%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel

idw2hr <- function(data, crs = 4326, outgrid_params = NULL, col_names = NULL,
                   interest_vars = NULL, idp = 1, nmax = 4, ncores = 1,
                   restore_NA = FALSE){

  # PN: outgrid_params MUST reference CRS 4326

  # from array to data.frame
  if (is.array(data) && is.numeric(data)) {
    data <- .array2df(data)
  }

  # check section
  data <- .check_colnames_idw2hr(data, col_names)
  interest_vars <- .check_interest_vars(data, interest_vars)

  # cast to sp and reshape
  sp_data <- .reshape(data, crs)

  # build output grid
  outgrid <- .check_outgrid(sp_data, outgrid_params)

  # register cluster
  max_cores <- parallel::detectCores()
  if (ncores > max_cores) {
    warning(
      sprintf("Requested %d cores, but only %d available. Using %d cores.",
              ncores, max_cores, max_cores)
    )
    ncores <- max_cores
  }
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)

  # run parallel for
  hr_data <- foreach::foreach(time_i = unique(sp_data@data$time),
                     .combine = 'rbind',
                     .packages = c("sp", "gstat", "dplyr")) %dopar% {

    # subset
    df <- subset(sp_data, time == time_i)
    hr_data_i <- data.frame(
      longitude = coordinates(outgrid)[,1],
      latitude = coordinates(outgrid)[,2],
      time = rep(time_i, nrow(coordinates(outgrid)))
    )

    for (var_i in interest_vars) {

      # run IDW
      obj = gstat::idw(
        formula = as.formula(sprintf("%s ~ 1", var_i)),
        locations = df[!is.na(df@data[[var_i]]), ],
        newdata = outgrid,
        debug.level = 0,
        idp = idp,
        nmax = nmax
      )

      # add new column
      hr_data_i[[var_i]] <- obj@data$var1.pred
    }
    return(hr_data_i)
  }

  # stop cluster
  parallel::stopCluster(cl)

  # restore NA
  if (restore_NA) {
    hr_data <- .restore_NA(data, hr_data, outgrid_params$resolution)
  }
  return(hr_data)
}

.array2df <- function(data){

  # from array to data.frame
  df <- data.frame(
    expand.grid(
      latitude = dimnames(data)[[1]],
      longitude = dimnames(data)[[2]],
      time = as.Date(as.numeric(dimnames(data)[[3]]))
    ),
    var = as.vector(data)
  )

  # cast longitude and latitude
  df$longitude <- as.numeric(as.character(df$longitude))
  df$latitude <- as.numeric(as.character(df$latitude))

  return(df)
}

.check_colnames_idw2hr <- function(data, col_names){

  # empty dictionary
  if(is.null(col_names)){
    col_names <- list(
      "coords_x" = "longitude",
      "coords_y" = "latitude",
      "t" = "time"
    )
  }
  else{

    # check keys presence
    if(!("coords_x" %in% names(col_names))){
      stop(sprintf("key 'coords_x' not present in the 'col_names' param"))
    }
    if(!("coords_y" %in% names(col_names))){
      stop(sprintf("key 'coords_y' not present in the 'col_names' param"))
    }
    if(!("t" %in% names(col_names))){
      stop(sprintf("key 't' not present in the 'col_names' param"))
    }

    # check columns presence
    if(!(any(colnames(data) == col_names$coords_x))){
      stop(sprintf("column '%s' not present in the data.frame", col_names$coords_x))
    }
    if(!(any(colnames(data) == col_names$coords_y))){
      stop(sprintf("column '%s' not present in the data.frame", col_names$coords_y))
    }
    if(!(any(colnames(data) == col_names$t))){
      stop(sprintf("column '%s' not present in the data.frame", col_names$t))
    }
  }

  # rename columns
  data <- data %>%
    dplyr::rename(
      longitude = col_names$coords_x,
      latitude = col_names$coords_y,
      time = col_names$t
    )

  return(data)
}

.check_interest_vars <- function(data, interest_vars){

  if(is.null(interest_vars)){
    interest_vars <- colnames(data)[!(colnames(data) %in% c("longitude", "latitude", "time"))]
  }
  else{
    for(var_i in interest_vars){
      if(!(any(colnames(data) == var_i))){
        stop(sprintf("column '%s' not present in the data.frame", var_i))
      } else if(class(data[[var_i]]) != "numeric"){
        stop(sprintf("column '%s' is not numeric", var_i))
      }
    }
  }

  return(interest_vars)
}

.check_outgrid <- function(data, outgrid_params){

  # empty dictionary
  if(is.null(outgrid_params)){
    outgrid_params <- list(
      "resolution" = .01,
      "min_lon" = min(data@coords[, 1]),
      "max_lon" = max(data@coords[, 1]),
      "min_lat" = min(data@coords[, 2]),
      "max_lat" = max(data@coords[, 2])
    )
  }
  else{

    # check keys presence
    if(!("resolution" %in% names(outgrid_params))){
      stop(sprintf("key 'resolution' not present in the 'outgrid_params'"))
    }
    if(!("min_lon" %in% names(outgrid_params))){
      stop(sprintf("key 'min_lon' not present in the 'outgrid_params'"))
    }
    if(!("max_lon" %in% names(outgrid_params))){
      stop(sprintf("key 'max_lon' not present in the 'outgrid_params'"))
    }
    if(!("min_lat" %in% names(outgrid_params))){
      stop(sprintf("key 'min_lat' not present in the 'outgrid_params'"))
    }
    if(!("max_lat" %in% names(outgrid_params))){
      stop(sprintf("key 'max_lat' not present in the 'outgrid_params'"))
    }
  }

  # build grid
  outgrid <- expand.grid(
    x = seq(
      outgrid_params$min_lon,
      outgrid_params$max_lon,
      by=outgrid_params$resolution
    ),
    y = seq(
      outgrid_params$min_lat,
      outgrid_params$max_lat,
      by=outgrid_params$resolution
    )
  )
  sp::coordinates(outgrid) <- ~x + y
  sp::proj4string(outgrid) <- sp::CRS(SRS_string = "EPSG:4326")
  sp::gridded(outgrid) <- TRUE

  return(outgrid)
}

.reshape <- function(data, crs){

  # cast to sp
  data <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = crs)

  # reshape
  data <- sf::st_transform(data, crs = 4326)

  return(as(data, "Spatial"))
}

.restore_NA <- function(lr_df, hr_df, resolution){

  # select only NA points
  NA_points <- lr_df[is.na(lr_df$var), ]

  output <- hr_df
  for (i in seq_len(nrow(NA_points))) {

    lat <- NA_points$latitude[i]
    lon <- NA_points$longitude[i]
    t <- NA_points$time[i]

    # define target latitudes: current point and one step north
    lat_vals <- c(lat, lat + resolution)

    # define target longitudes: current point and one step east
    lon_vals <- c(lon, lon + resolution)

    # loop over all combinations of latitude and longitude
    for (lat_i in lat_vals) {
      for (lon_i in lon_vals) {

        # find matching rows in high-resolution data
        idx <- which(
          output$latitude == lat_i &
            output$longitude == lon_i &
            output$time == t
        )

        # if matching rows are found, set their 'var' to NA
        if (length(idx) > 0) {
          output$var[idx] <- NA
        }
      }
    }
  }

  return(output)
}

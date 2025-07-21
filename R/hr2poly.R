#' @title Aggregate high resolution grid data onto polygons
#' @name hr2poly
#'
#' @description
#' Aggregates input high-resolution space-time data onto \strong{polygons} and
#' computes the user-specified statistics (e.g., mean, median, and standard
#' deviation) for each space-time variable. This function supports parallel
#' computing.
#'
#' @usage hr2poly(data, polygon_type = "mun", col_names = NULL, stats = NULL,
#' crs = 4326, ncores = 1, keep_geometry = FALSE)
#' @param data Space-time dataset in \code{data.frame} format.
#' @param polygon_type Level of detail for the administrative boundaries.
#' Accepted values are \code{"mun"} (\strong{municipalities}),
#' \code{"prov"} (\strong{provinces}), or \code{"reg"} (\strong{regions}).
#' Default values is \code{"mun"}.
#' @param col_names Named list with the field \strong{coords_x},
#' \strong{coords_y} and \strong{t}. These parameters specify the names of the
#' columns representing the x-coordinate, the y-coordinate and time respectively.
#' If not specified, then default column names \code{"longitude"},
#' \code{"latitude"} and \code{"time"} are assumed.
#' @param stats Named list specifying which statistics to compute for each
#' variable of interest. Accepted statistics are \code{"min"} (\strong{minimum}),
#' \code{"mean"} (\strong{mean value}), \code{"median"} (\strong{median value}),
#' \code{"max"} (\strong{maximum}), and \code{"std"} (\strong{standard deviation}).
#' If not specified, then all these statistics are computed for all numeric
#' variables.
#' @param crs Coordinate Reference System (CRS) for the input data, given as an
#' EPSG code. If it differs from \code{4326} (\strong{WGS 84}), then data will
#' be reprojected accordingly.
#' @param ncores Number of CPU cores to use for parallel processing.
#' Default value is \code{1}, then computation is performed sequentially. Higher
#' values enable parallel execution to improve performance on large datasets.
#' @param keep_geometry Logical. Whether to retain the geometry in the output
#' dataset. Default is \code{FALSE}.
#'
#' @return A \code{data.frame} containing daily statistics of the variables of
#' interest, computed for each administrative boundary (e.g., municipality).
#'
#' @examples
#' # SEE "demo.Rmd" FOR MORE DETAILS
#'
#' \dontrun{
#' res_hr2poly <- hr2poly(
#'   data = input_data,
#'   polygon_type = "mun",
#'   col_names = list(
#'     "coords_x" = "x",
#'     "coords_y" = "y",
#'     "t" = "time"
#'   ),
#'   stats = list(
#'     "AQ_EEA_NO2" = c("min", "mean", "max", "std"),
#'     "AQ_CAMS_NO2" = c("min", "median", "max", "std"),
#'     "altitude" = c("mean")
#'   ),
#'   crs = 4979,
#'   ncores = 2,
#'   keep_geometry = TRUE
#' )}
#'
#' @seealso \url{https://github.com/GRINS-Spoke0-WP2/geotools/blob/develop/demo/demo.Rmd}
#'
#' @export
#'
#' @importFrom sf st_as_sf st_transform st_crs st_join st_drop_geometry
#' @importFrom dplyr rename group_by across summarise select all_of
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster

hr2poly <- function(data, polygon_type = "mun", col_names = NULL, stats = NULL,
                    crs = 4326, ncores = 1, keep_geometry = FALSE){

  # check section
  data <- .check_colnames_hr2poly(data, col_names)
  temp <- .check_polygon(polygon_type)
  bounds <- temp$bounds
  code <- temp$code
  stats <- .check_stats(data, stats)

  # from st to sf
  sf_data <- sf::st_as_sf(
    data,
    coords = c("coords_x", "coords_y"),
    crs = crs
  )

  # change crs
  sf_data <- sf::st_transform(
    sf_data,
    crs = sf::st_crs(bounds)
  )

  # join
  join_data <- sf::st_join(
    sf_data,
    bounds,
    left = FALSE
  )
  join_data <- sf::st_drop_geometry(join_data)

  # com_exc <- !unique(bounds$name_comuni) %in% unique(join_data$nome_colonna_comuni)
  # if(length(com_exc)>0) warning con lista degli esclusi = print(com_exc)

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

  # stop cluster (on exit)
  on.exit(
    {
      parallel::stopCluster(cl)
    },
    add = TRUE
  )

  # group
  stats_data <- foreach::foreach(var_i = names(stats),
                        .combine = "cbind",
                        .packages = c("dplyr")) %dopar% {

    temp <- join_data %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(c(code, "time"))
        )
      ) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(c(var_i)),
          lst(
            "min" = min,
            "mean" = mean,
            "median" = median,
            "max" = max,
            "std" = sd
          ),
          na.rm = T
        ),
        .groups = "drop"
      )
    return(temp)
  }
  stats_data <- stats_data %>%
    dplyr::select(-which(duplicated(names(stats_data))))

  # filt not required stats
  output_vars <- c(code, "time")
  for(var_i in names(stats)){
    for(stat_i in stats[[var_i]]){
      output_vars[length(output_vars) + 1] = sprintf("%s_%s", var_i, stat_i)
    }
  }
  stats_data <- stats_data %>% dplyr::select(dplyr::all_of(output_vars))

  # get polygons data
  stats_data <- merge(bounds, stats_data)

  if(keep_geometry == FALSE){
    stats_data <- sf::st_drop_geometry(stats_data)
  }
  return(stats_data)
}

.check_colnames_hr2poly <- function(data, col_names){

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
      stop(sprintf("column '%s' not present in the data.frame", col_names$lon))
    }
    if(!(any(colnames(data) == col_names$coords_y))){
      stop(sprintf("column '%s' not present in the data.frame", col_names$la))
    }
    if(!(any(colnames(data) == col_names$t))){
      stop(sprintf("column '%s' not present in the data.frame", col_names$t))
    }
  }

  # rename columns
  data <- data %>%
    dplyr::rename(
      coords_x = col_names$coords_x,
      coords_y = col_names$coords_y,
      time = col_names$t
    )

  return(data)
}

.check_polygon <- function(polygon_type){

  # load data
  # load("~/Desktop/geotools/data/IT_adm_bounds_2025.RData")

  if((polygon_type != "mun") & (polygon_type != "prov") & (polygon_type != "reg")){
    stop("'polygon' must be 'mun', 'prov' or 'reg'")
  }
  else{
    if(polygon_type == "mun"){
      return(list(bounds = mun_bounds, code = "PRO_COM"))
    }
    if(polygon_type == "prov"){
      return(list(bounds = prov_bounds, code = "COD_PROV"))
    }
    if(polygon_type == "reg"){
      return(list(bounds = reg_bounds, code = "COD_REG"))
    }
  }
}

.check_stats <- function(data, stats){

  # empty dictionary
  if(is.null(stats)){
    stats <- list()
    for(col_i in colnames(data)){
      if((col_i != "coords_x") & (col_i != "coords_y") & (col_i != "time")){
        if(class(data[[col_i]]) == "numeric"){
          stats[[col_i]] <- c("min", "mean", "median", "max", "std")
        }
      }
    }
  }

  else {
    for(var_i in names(stats)){
      if(!(any(colnames(data) == var_i))){
        stop(sprintf("column '%s' not present in the data.frame", var_i))
      } else if (class(data[[var_i]]) != "numeric") {
        stop(sprintf("column '%s' is not numeric", var_i))
      } else {
        for(stat_i in stats[[var_i]]){
          if(!(stat_i %in% c("min", "mean", "median", "max", "std"))) {
            stop("statistic must be 'min', 'mean', 'median', 'max' or 'std'")
          }
        }
      }
    }
  }

  return(stats)
}

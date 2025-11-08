#' @title Align heterogeneous space-time data onto a common grid
#' @name geomatching
#'
#' @description
#' Aligns input space-time data from different spatial grids onto a specific
#' common grid, even when their geographic reference systems may differ.
#' This procedure is known as \strong{spatial overlay}.
#'
#' @usage geomatching(data, settings = NULL, aggregate = FALSE, group_by = "mun")
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
#' @param aggregate Logical flag. If \code{TRUE}, the function aggregates the
#' matched data over the specified administrative boundaries (see group_by)
#' and computes summary statistics (e.g., mean, median, quartiles, standard
#' deviation) for each space-time variable. If \code{FALSE}, the function returns
#' the matched data without aggregation.
#' @param group_by Character string. Specifies the administrative boundary level
#' for aggregation when \code{aggregate = TRUE}. Accepted values are \strong{"mun"}
#' (municipality), \strong{"prov"} (province) or \strong{"reg"} (region).
#' The data will be grouped and summarized according to the selected boundary level.
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
#' @importFrom dplyr rename group_by across summarise select all_of

# points and GRIDs ONLY IN WGS84 - EPSG 4326
geomatching <- function(data,
                        settings = NULL,
                        aggregate = FALSE,
                        group_by = "mun"){

  ndata <- length(data)
  if (is.null(settings)) {
    settings <- .empty_settings()
  }

  if(aggregate){

    # configuration
    code <- .check_group_by(group_by)

    # extend high-resolution gridded LAUs
    hr_grid_df <- .extend_hr_grid(data, settings)
    hr_grid_df <- hr_grid_df[, c("longitude", "latitude", "time", "COD_REG", "COD_PROV", "PRO_COM")]

    # extend settings
    settings[["format"]] <- c(list("xyt"), settings[["format"]])
    settings[["type"]] <- c(list("points"), settings[["type"]])
    settings[["crs"]] <- c(list(4326), settings[["crs"]])

    # append
    data <- c(list(hr_grid_df), data)
    ndata <- length(data)
  }

  settings <- .input_check(settings, ndata)
  grid.df <- .create_df(data, settings)
  STs <- .create_STs(data, grid.df, settings)

  for (i in 2:ndata) {
    over_ST <- spacetime::over(STs[[1]], STs[[i]])

    STs[[1]]@data <- cbind(STs[[1]]@data, over_ST[, 4:ncol(over_ST), drop=FALSE])
    if (settings$format[i] == "matrix") {
      names(STs[[1]]@data)[ncol(STs[[1]]@data)] <- paste0("matrix_", i)
    }
  }

  # aggregate
  if (aggregate) {

    # configuration
    raw_df <- STs[[1]]@data
    num_vars <- .get_numeric_var_names(raw_df)

    # execute
    aggr_df <- .aggregate(raw_df, code, num_vars)

    # restore missing mun.
    if (group_by == "mun") {
      aggr_df <- .restore_missing_mun(aggr_df)
    }

    # sort
    aggr_df <- aggr_df[do.call(order, aggr_df[c(code, "time")]), ]

    return(aggr_df)

  } else {

    return(STs[[1]]@data)
  }
}

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

  # accepted: xyt, matrix
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
    sp <-
      grid.df[[i]][grid.df[[i]]$time == grid.df[[i]]$time[1], 1:2]
    if (settings$type[i] == "points") {
      sp::coordinates(sp) <- c("longitude", "latitude")
      slot(sp, "proj4string") <- sp::CRS(SRS_string = settings$crs[i])
      t <- unique(grid.df[[i]][, 3])
    } else if (settings$type[i] == "grid") {
      sp::coordinates(sp) <- c("longitude", "latitude")
      slot(sp, "proj4string") <- sp::CRS(SRS_string = settings$crs[i])
      t <- unique(grid.df[[i]][, 3])
      sp::gridded(sp) <- TRUE
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
    if (is.data.frame(t)) {
      t <- t$time
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

.extend_hr_grid <- function(data, settings) {

  # get first and last dates
  first_date <- NULL
  last_date <- NULL
  i <- 1
  for(data_i in data){
    if (settings[["format"]][[i]] == "xyt") {
      current_first_date <- min(data_i$time)
      current_last_date <- max(data_i$time)
    } else {
      current_first_date <- min(as.Date(as.numeric(dimnames(data_i)[[3]])))
      current_last_date <- max(as.Date(as.numeric(dimnames(data_i)[[3]])))
    }

    if(is.null(first_date) || current_first_date < first_date){
      first_date <- current_first_date
    }
    if(is.null(last_date) || current_last_date > last_date){
      last_date <- current_last_date
    }
    i <- i + 1
  }

  # extend grid
  grid_list <- list()
  i <- 1
  for(date_i in seq(first_date, last_date, by = "day")){
    temp <- hr_grid_LAUs
    temp$time <- date_i
    grid_list[[i]] <- temp
    i <- i + 1
  }
  grid_df <- do.call(rbind, grid_list)
  grid_df$time <- as.Date(grid_df$time)

  return(grid_df)
}

.check_group_by <- function(group_by){

  if((group_by != "mun") & (group_by != "prov") & (group_by != "reg")){
    stop("'group_by' must be 'mun', 'prov' or 'reg'")
  } else {
    if(group_by == "mun"){
      return("PRO_COM")
    }
    if(group_by == "prov"){
      return("COD_PROV")
    }
    if(group_by == "reg"){
      return("COD_REG")
    }
  }
}

.get_numeric_var_names <- function(df) {

  return(
      setdiff(
      names(df)[sapply(df, is.numeric)],
      c("longitude", "latitude", "time", "COD_REG", "COD_PROV", "PRO_COM")
    )
  )
}

.aggregate <- function(df, code, vars) {

  aggr_df <- suppressWarnings(
    df %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(c(code, "time"))
        )
      ) %>% 
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(vars),
          list(
            min = ~min(.x, na.rm = TRUE),
            `1st_quartile` = ~quantile(.x, 0.25, na.rm = TRUE),
            mean = ~mean(.x, na.rm = TRUE),
            median = ~median(.x, na.rm = TRUE),
            `3rd_quartile` = ~quantile(.x, 0.75, na.rm = TRUE),
            max = ~max(.x, na.rm = TRUE),
            sd = ~sd(.x, na.rm = TRUE)
          )
        ),
        .groups = "drop"
      )
  )

  return(aggr_df)
}

.restore_missing_mun <- function(df) {

  # set Amalfi
  temp <- df[df$PRO_COM == 65006, ]
  temp$PRO_COM <- 65011
  df <- rbind(df, temp)

  # set Sagliano Micca
  temp <- df[df$PRO_COM == 96056, ]
  temp$PRO_COM <- 96034
  df <- rbind(df, temp)

  return(df)
}

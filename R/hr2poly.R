# import libraries
library(sf)
library(dplyr)
library(foreach)
library(doParallel)

hr2poly <- function(data, polygon_type = "mun", col_names = NULL, stats = NULL,
                    crs = 4326, ncores = 2, keep_geometry = FALSE){

  # check section
  data <- .check_colnames_hr2poly(data, col_names)
  temp <- .check_polygon(polygon_type)
  bounds <- temp$bounds
  code <- temp$code
  stats <- .check_stats(data, stats)

  # from st to sf
  sf_data <- st_as_sf(
    data,
    coords = c("coords_x", "coords_y"),
    crs = crs
  )

  # change crs
  sf_data <- st_transform(
    sf_data,
    crs = st_crs(bounds)
  )

  # join
  join_data <- st_join(
    sf_data,
    bounds,
    left = FALSE
  )
  join_data <- st_drop_geometry(join_data)

  # register cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  # group
  stats_data <- foreach(var_i = names(stats),
                        .combine = "cbind",
                        .packages = c("dplyr")) %dopar% {

    temp <- join_data %>%
      group_by(
        across(
          all_of(c(code, "time"))
        )
      ) %>%
      summarise(
        across(
          all_of(c(var_i)),
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
    select(-which(duplicated(names(stats_data))))

  # filt not required stats
  output_vars <- c(code, "time")
  for(var_i in names(stats)){
    for(stat_i in stats[[var_i]]){
      output_vars[length(output_vars) + 1] = sprintf("%s_%s", var_i, stat_i)
    }
  }
  stats_data <- stats_data %>% select(all_of(output_vars))

  # stop cluster
  stopCluster(cl)

  # get polygons data
  stats_data <- merge(bounds, stats_data)

  if(keep_geometry == FALSE){
    stats_data <- st_drop_geometry(stats_data)
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
    rename(
      coords_x = col_names$coords_x,
      coords_y = col_names$coords_y,
      time = col_names$t
    )

  return(data)
}

.check_polygon <- function(polygon_type){

  # load data
  load("~/Desktop/geotools/data/IT_adm_bounds_2025.RData")

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

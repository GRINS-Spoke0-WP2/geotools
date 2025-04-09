# import libraries
library(sf)
library(dplyr)
library(foreach)
library(doParallel)

hr2poly <- function(data, polygon_type = "mun", col_names = NULL, stats = NULL,
                    crs = 4326, ncores = 2){

  # check section
  data <- .check_colnames(data, col_names)
  temp <- .check_polygon(polygon_type)
  bounds <- temp$bounds
  code <- temp$code
  stats <- .check_stats(data, stats)

  # from st to sf
  data_sf <- st_as_sf(
    data,
    coords = c("longitude", "latitude"),
    crs = crs
  )

  # change crs
  data_sf <- st_transform(
    data_sf,
    crs = st_crs(bounds)
  )

  # join
  data_join <- st_join(
    data_sf,
    bounds,
    left = FALSE
  )
  data_join <- st_drop_geometry(data_join)

  # register cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  # group
  data_stats <- foreach(var_i = names(stats),
                        .combine = "cbind",
                        .packages = c("dplyr")) %dopar% {

    temp <- data_join %>%
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
  data_stats <- data_stats %>%
    select(-which(duplicated(names(data_stats))))

  # stop cluster
  stopCluster(cl)

  # get polygons data
  data_stats <- merge(bounds, data_stats)

  return(data_stats)
}

.check_colnames <- function(data, col_names){

  # empty dictionary
  if(is.null(col_names)){
    col_names <- list(
      "lon" = "longitude",
      "lat" = "latitude",
      "t" = "time"
    )
  }
  else{

    # check keys presence
    if(!("lon" %in% names(col_names))){
      stop(sprintf("key 'lon' not present in the 'col_names' param"))
    }
    if(!("lat" %in% names(col_names))){
      stop(sprintf("key 'lat' not present in the 'col_names' param"))
    }
    if(!("t" %in% names(col_names))){
      stop(sprintf("key 't' not present in the 'col_names' param"))
    }

    # check columns presence
    if(!(any(colnames(data) == col_names$lon))){
      stop(sprintf("column '%s' not present in the data.frame", col_names$lon))
    }
    if(!(any(colnames(data) == col_names$lat))){
      stop(sprintf("column '%s' not present in the data.frame", col_names$la))
    }
    if(!(any(colnames(data) == col_names$t))){
      stop(sprintf("column '%s' not present in the data.frame", col_names$t))
    }
  }

  # rename columns
  data <- data %>%
    rename(
      longitude = col_names$lon,
      latitude = col_names$lat,
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
      if((col_i != "longitude") & (col_i != "latitude") & (col_i != "time")){
        if(class(data[[col_i]]) == "numeric"){
          stats[[col_i]] <- c("min", "mean", "median", "max", "std")
        }
      }
    }
  }

  return(stats)
}

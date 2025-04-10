# import libraries
library(sp)
library(sf)
library(gstat)
library(dplyr)
library(foreach)
library(doParallel)

idw2hr <- function(data, crs = 4326, outgrid_params = NULL, col_names = NULL,
                   interest_vars = NULL, idp = 1, ncores = 2){

  # PN: outgrid_params MUST reference CRS 4326

  # check section
  data <- .check_colnames(data, col_names)
  interest_vars <- .check_interest_vars(data, interest_vars)

  # reshape and cast to spazial data.frame
  sp_data <- .reshape(data, crs)

  # build output grid
  outgrid <- .check_outgrid(sp_data, outgrid_params, crs)

  # register cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  # run parallel for
  hr_data <- foreach(time_i = unique(sp_data@data$time),
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
      obj = idw(
        formula = as.formula(sprintf("%s ~ 1", var_i)),
        locations = df[!is.na(df@data[[var_i]]), ],
        newdata = outgrid,
        debug.level = 0,
        idp = idp
      )

      # add new column
      hr_data_i[[var_i]] <- obj@data$var1.pred
    }
    return(hr_data_i)
  }

  # stop cluster
  stopCluster(cl)

  return(hr_data)
}

.check_colnames <- function(data, col_names){

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
    rename(
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

.check_outgrid <- function(data, outgrid_params, crs){

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
  coordinates(outgrid) <- ~x + y
  proj4string(outgrid) <- CRS(SRS_string = sprintf("EPSG:%s", crs))
  gridded(outgrid) <- TRUE

  return(outgrid)
}

.reshape <- function(data, crs){

  # to sf
  data <- st_as_sf(data, coords = c("longitude", "latitude"), crs = crs)

  # reshape
  data <- st_transform(data, crs = 4326)

  return(as(data, "Spatial"))
}

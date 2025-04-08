# import libraries
library(sp)
library(gstat)
library(dplyr)

idw2hr <- function(data, outgrid_params = NULL, col_names = NULL,
                   interest_vars = NULL, idp = 1, crs=4326){

  # check section
  data <- .check_colnames(data, col_names)
  interest_vars <- .check_interest_vars(data, interest_vars)
  outgrid <- .check_outgrid(data, outgrid_params, crs)

  # remove missing values
  data <- na.omit(data)

  unique_times <- unique(data$time)
  ALL_hr_data <- list()
  for (time_i in unique_times) {

    i <- 0
    for (var_i in interest_vars) {

      # build spatial data.frame
      df <- data[data$time == time_i, ]
      coordinates(df) <- ~longitude+latitude
      proj4string(df) <- CRS(SRS_string = sprintf("EPSG:%s", crs))

      # run IDW
      obj = idw(
        formula = as.formula(sprintf("%s ~ 1", var_i)),
        locations = df,
        newdata = outgrid,
        debug.level = 0,
        idp = idp,
      )

      # add new column
      if (i == 0){
        temp <- data.frame(
          longitude = obj@coords[, "x"],
          latitude = obj@coords[, "y"],
          time = as.Date(time_i)
        )
      }
      temp[[var_i]] <- obj@data$var1.pred
      i <- i + 1
    }

    # add data regarding new date
    ALL_hr_data[[length(ALL_hr_data) + 1]] <- temp
  }

  # rows concatenation
  hr_data <- do.call(rbind, ALL_hr_data)
  return(hr_data)

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

.check_interest_vars <- function(data, interest_vars){

  if(is.null(interest_vars)){
    interest_vars <- colnames(data)[!(colnames(data) %in% c("Longitude", "Latitude", "Time"))]
  }
  else{
    for(var_i in interest_vars){
      if(!(any(colnames(data) == var_i))){
        stop(sprintf("column '%s' not present in the data.frame", var_i))
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
      "min_lon" = min(data$longitude),
      "max_lon" = max(data$longitude),
      "min_lat" = min(data$latitude),
      "max_lat" = max(data$latitude)
    )
  }
  else{

    # check keys presence
    if(!("resolution" %in% names(outgrid_params))){
      stop(sprintf("key 'resolution' not present in the 'outgrid_params'"))
    }
    if(!("min_lon" %in% names(outgrid_params))){
      stop(sprintf("key 'min_lom' not present in the 'outgrid_params'"))
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

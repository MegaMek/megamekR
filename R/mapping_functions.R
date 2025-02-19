
planetary_system_values <- c("id", "sucsId", "xcood", "ycood", "spectralType",
                             "primarySlot")
planetary_system_events <- c("date", "nadirCharge", "zenithCharge")
planet_events <- c("date", "faction", "population", "socioIndustrial", "hpg",
                   "hiringHall", "atmosphere", "pressure", "composition",
                   "water", "temperature", "lifeForm", "dayLength")
planet_values <- c("name", "type", "orbitalDist", "sysPos", "icon", "pressure",
                   "atmosphere", "composition", "gravity", "diameter",
                   "density", "dayLength", "yearLength", "temperature",
                   "water", "lifeForm", "desc", "ring", "smallMoons")
planet_values_sourceable <- c("spectralType", "primarySlot", "name", "type",
                              "sysPos", "pressure", "atmosphere", "composition",
                              "gravity", "diameter", "density", "dayLength",
                              "yearLength", "temperature", "water", "lifeForm",
                              "population", "socioIndustrial", "hpg", "faction",
                              "hiringHall", "capital", "ring", "smallMoons",
                              "size", "nadirCharge", "zenithCharge")

read_planetary_data <- function(yaml_path) {

  raw_data <- yaml::read_yaml(yaml_path)

  # get base system data
  system_data <-  get_values(raw_data, planetary_system_values)

  system_event_data <- NULL
  if(!is.null(raw_data$event)) {
    system_event_data <- purrr::map(raw_data$event, function(event) {
      get_values(event, planetary_system_events) |>
        dplyr::mutate(date = lubridate::as_date(date))
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(date = lubridate::as_date(date))
  }

  # get base planet data
  planet_data <- purrr::map(raw_data$planet, function(p) {
    get_values(p, planet_values)
  }) |>
    dplyr::bind_rows()

  planetary_events <- purrr::map(raw_data$planet, function(p) {
    if(!is.null(p$event)) {
      purrr::map(p$event, function(event) {
        get_values(event, planet_events)
      }) |>
        dplyr::bind_rows() |>
        dplyr::mutate(date = lubridate::as_date(date))
    }
  })

  landmasses <- purrr::map(raw_data$planet, function(p) {
    if(!is.null(p$landmass)) {
      purrr::map(p$landmass, function(landmass) {
        get_values(landmass, c("name", "capital"))
      }) |>
        dplyr::bind_rows()
    }
  })

  satellites <- purrr::map(raw_data$planet, function(p) {
    if(!is.null(p$satellite)) {
      purrr::map(p$satellite, function(satellite) {
        get_values(satellite, c("name", "size", "icon"))
      }) |>
        dplyr::bind_rows()
    }
  })

  return(list(system = system_data,
              planets = planet_data,
              satellites = satellites,
              landmasses = landmasses,
              system_events = system_event_data,
              planetary_events = planetary_events))

}

write_planetary_data <- function(planetary_system, path) {
  # everything goes in lists of lists of lists

  # start with overall system information
  planet_list <- list(id = planetary_system$system$id,
                      sucsId = planetary_system$system$sucsId,
                      xcood = planetary_system$system$xcood,
                      ycood = planetary_system$system$ycood,
                      spectralType = write_sourceable_value(
                        planetary_system$system$spectralType,
                        planetary_system$system$source_spectralType
                      ),
                      primarySlot = write_sourceable_value(
                        planetary_system$system$primarySlot,
                        planetary_system$system$source_primarySlot
                      ))

  if(!is.null(planetary_system$system_events)) {
    planet_list$event <- write_tibble_list(planetary_system$system_events,
                                           planetary_system_events)
  }

  # get base planet information
  planets <- write_tibble_list(planetary_system$planets, planet_values)

  # now cycle through to add in landmasses, satellites, and events
  for(i in 1:length(planets)) {
    planet <- planets[[i]]

    # add landmasses
    landmass <- planetary_system$landmasses[[i]]
    if(!is.null(landmass)) {
      planet$landmass <- write_tibble_list(landmass, c("name", "capital"))
    }

    # add satellites
    satellite <- planetary_system$satellites[[i]]
    if(!is.null(satellite)) {
      planet$satellite <- write_tibble_list(satellite, c("name", "size", "icon"))
    }

    # add planetary events
    events <- planetary_system$planetary_events[[i]]
    if(!is.null(events)) {
      planet$event <- write_tibble_list(events, planet_events)
    }

    planets[[i]] <- planet
  }

  planet_list$planet <- planets

  write_yaml(planet_list, file = path, indent.mapping.sequence = TRUE,
             precision = 12)
}

write_sourceable_value <- function(value, source) {
  if(is.na(source)) {
    return(value)
  } else {
    return(list(source = source, value = value))
  }
}

write_tibble_list <- function(df, variables) {
  df |>
    pmap(function(...) {
      row <- tibble(...)
      data_list <- list()
      for(variable in variables) {
        if(variable %in% colnames(row) && !is.na(pull(row, variable))) {
          if(variable %in% planet_values_sourceable) {
            # we need to write a sourceable value
            value <- pull(row, variable)
            source <- pull(row, paste("source_", variable, sep = ""))
            data_list[[variable]] <- write_sourceable_value(value, source)
          } else {
            # we can just write the value
            # special hack for dates, which need to be converted back
            # to characters
            value <- pull(row, variable)
            if(is.Date(value)) {
              value <- as.character(value)
            }
            data_list[[variable]] <- value
          }

        }
      }
      return(data_list)
    })
}


get_values <- function(data_source, value_names) {
  value_names |>
    purrr::map(function(x) {
      temp <- data_source[[x]]
      if(is.null(temp)) {
        return(NULL)
      }
      if(!is.list(temp)) {
        if(x == "faction") {
          paste(temp, collapse = ", ")
        }
        if(!(x %in% planet_values_sourceable)) {
          temp <- list(value = temp)
        } else {
          # create a source value
          temp <- list(source = NA, value = temp)
        }
      } else {
        if(x == "faction") {
          temp$value <- paste(temp$value, collapse = ", ")
        }
      }
      if(length(temp) > 1) {
        # reverse the ordering
        temp <- list(temp$value, temp$source)

        temp <- setNames(temp, c(x, paste("source", x, sep="_")))
      } else {
        temp <- setNames(temp, x)
      }
      return(tibble::as_tibble(temp))
    }) |>
    dplyr::bind_cols()
}



planetary_system_values <- c("id", "sucsId", "xcood", "ycood", "spectralType",
                             "primarySlot")
planetary_system_events <- c("date", "nadirCharge", "zenithCharge")
planet_events <- c("date", "population", "socioIndustrial", "hpg", "faction",
                   "hiringHall", "atmosphere", "pressure", "composition",
                   "water", "temperature", "lifeForm", "dayLength")
planet_values <- c("sysPos", "name", "type", "orbitalDist", "pressure",
                   "atmosphere", "composition", "gravity", "diameter",
                   "density", "dayLength", "yearLength", "temperature",
                   "water", "lifeForm", "smallMoons", "icon")
planet_values_sourceable <- c("spectralType", "primarySlot", "name", "type",
                              "sysPos", "pressure", "atmosphere", "composition",
                              "gravity", "diameter", "density", "dayLength",
                              "yearLength", "temperature", "water", "lifeForm",
                              "population", "socioIndustrial", "hpg", "faction",
                              "hiringHall", "capital", "smallMoons", "size")

read_planetary_data <- function(yaml_path) {

  raw_data <- yaml::read_yaml(yaml_path)

  # get base system data
  system_data <-  get_values(raw_data, planetary_system_values)

  system_event_data <- NULL
  if(!is.null(raw_data$event)) {
    system_event_data <- purrr::map(raw_data$event, function(event) {
      get_values(event, planetary_system_events)
    }) |>
      dplyr::bind_rows()
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



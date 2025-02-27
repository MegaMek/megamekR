# Constants ------------------------------------------------------------------

# these character vectors define the various variables we need to extract from
# the yaml data. The order of these will affect the order variables are written
# out in yaml.

# values we expect to find at the level of the planetary system
PLANETARY_SYSTEM_VARS <- c("id", "sucsId", "xcood", "ycood",
                                "spectralType", "primarySlot")
# values we expect to find in planetary system events
PLANETARY_SYSTEM_EVENT_VARS <- c("date", "nadirCharge", "zenithCharge")
# values we expect to find in planetary events
PLANETARY_EVENT_VARS <- c("date", "faction", "population", "socioIndustrial",
                          "hpg", "hiringHall", "atmosphere", "pressure",
                          "composition", "water", "temperature", "lifeForm",
                          "dayLength")
# values we expect to find in the base planet data (not events)
PLANET_VARS <- c("name", "type", "orbitalDist", "sysPos", "icon", "pressure",
                   "atmosphere", "composition", "gravity", "diameter",
                   "density", "dayLength", "yearLength", "temperature",
                   "water", "lifeForm", "desc", "ring", "smallMoons")
# values that are potentially sourceable in the data - can come at any level
SOURCEABLE_VARS <- c("spectralType", "primarySlot", "name", "type",
                     "sysPos", "pressure", "atmosphere", "composition",
                     "gravity", "diameter", "density", "dayLength",
                     "yearLength", "temperature", "water", "lifeForm",
                     "population", "socioIndustrial", "hpg", "faction",
                     "hiringHall", "capital", "ring", "smallMoons",
                     "size", "nadirCharge", "zenithCharge")

# these vectors define the factor levels for a variety of variables
PLANET_TYPE_LEVELS <- c("ASTEROID_BELT", "DWARF_TERRESTRIAL", "TERRESTRIAL",
                        "GIANT_TERRESTRIAL", "ICE_GIANT", "GAS_GIANT")
PRESSURE_LEVELS <- c("VACUUM", "TRACE", "THIN", "STANDARD", "HIGH", "VERY_HIGH")
ATMOSPHERE_LEVELS <- c("NONE", "TAINTEDPOISON", "TAINTEDCAUSTIC",
                       "TAINTEDFLAME", "TOXICPOISON", "TOXICCAUSTIC",
                       "TOXICFLAME", "BREATHABLE")
LIFEFORM_LEVELS <- c("NONE", "MICROBE", "PLANT", "INSECT", "FISH", "AMPHIBIAN",
                     "REPTILE", "BIRD", "MAMMAL")
HPG_LEVELS <- c("A","B","C","D","X")
HIRING_HALL_LEVELS <- c("NONE", "QUESTIONABLE", "MINOR", "STANDARD", "GREAT")

# IO functions --------------------------------------------------------------

#' Read in a single planetary system from yaml
#'
#' @description
#' This function reads in a single planetary system file using the syntax of
#' MekHQ.
#'
#' @param yaml_path character string providing the path to the yaml file or
#'     a url to the file.
#'
#' @returns The final returned object is a list of various planetary properties
#'    with the end value in each case being a tibble. All sourceable values
#'    produce both a value and a `source_{value}` variable. Missing values for
#'    source variables indicate noncanon data.
#'
#' @examples
#'
#' # read in Earth's data directly from GitHub
#' terra <- read_planetary_data("https://raw.githubusercontent.com/MegaMek/mekhq/refs/heads/master/MekHQ/data/universe/planetary_systems/canon_systems/Terra.yml")
#'
#' @export
read_planetary_data <- function(yaml_path) {

  raw_data <- yaml::read_yaml(yaml_path)

  # get base system data
  system_data <-  get_values(raw_data, PLANETARY_SYSTEM_VARS)

  system_event_data <- NULL
  if(!is.null(raw_data$event)) {
    system_event_data <- purrr::map(raw_data$event, function(event) {
      get_values(event, PLANETARY_SYSTEM_EVENT_VARS) |>
        dplyr::mutate(date = lubridate::as_date(date))
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(date = lubridate::as_date(date))
  }

  # get base planet data
  planet_data <- purrr::map(raw_data$planet, function(p) {
    get_values(p, PLANET_VARS)
  }) |>
    dplyr::bind_rows()

  planetary_events <- purrr::map(raw_data$planet, function(p) {
    if(!is.null(p$event)) {
      purrr::map(p$event, function(event) {
        get_values(event, PLANETARY_EVENT_VARS)
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

#' Write a single planetary system in memory to a file in yaml format
#'
#' @description
#' This function writes a single planetary system object read in by
#' [read_planetary_data()].
#'
#' @param planetary_system A planetary system object created by [read_planetary_data()].
#'
#' @param yaml_path character string providing the path to the yaml file.
#'
#' @examples
#'
#' # read in Earth's data directly from GitHub
#' terra <- read_planetary_data("https://raw.githubusercontent.com/MegaMek/mekhq/refs/heads/master/MekHQ/data/universe/planetary_systems/canon_systems/Terra.yml")
#' write_planetary_data(terra, ".")
#'
#' @export
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
                                           PLANETARY_SYSTEM_EVENT_VARS)
  }

  # get base planet information
  planets <- write_tibble_list(planetary_system$planets, PLANET_VARS)

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
      planet$event <- write_tibble_list(events, PLANETARY_EVENT_VARS)
    }

    planets[[i]] <- planet
  }

  planet_list$planet <- planets

  yaml::write_yaml(planet_list, file = path, indent.mapping.sequence = TRUE,
                   precision = 12)
}

# helper function
write_sourceable_value <- function(value, source) {
  if(is.na(source)) {
    return(value)
  } else {
    return(list(source = source, value = value))
  }
}

# helper function
write_tibble_list <- function(df, variables) {
  df |>
    purrr::pmap(function(...) {
      row <- tibble::tibble(...)
      data_list <- list()
      for(variable in variables) {
        if(variable %in% colnames(row) && !is.na(dplyr::pull(row, variable))) {
          value <- dplyr::pull(row, variable)
          if(lubridate::is.Date(value)) {
            # special hack for dates, which need to be converted back
            # to characters
            value <- as.character(value)
          }
          if(variable == "faction") {
            # factions are special as you can have multiple so need to be
            #inside unnamed list
            value <- as.list(stringr::str_split_1(value, ","))
          }
          if(variable %in% SOURCEABLE_VARS) {
            # we need to write a sourceable value
            source <- dplyr::pull(row, paste("source_", variable, sep = ""))
            data_list[[variable]] <- write_sourceable_value(value, source)
          } else {
            # we can just write the value
            data_list[[variable]] <- value
          }

        }
      }
      return(data_list)
    })
}

# helper function
get_values <- function(data_source, value_names) {
  df <- value_names |>
    purrr::map(function(x) {
      temp <- data_source[[x]]
      if(is.null(temp)) {
        return(NULL)
      }
      if(!is.list(temp)) {
        if(x == "faction") {
          paste(temp, collapse = ", ")
        }
        if(!(x %in% SOURCEABLE_VARS)) {
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
    dplyr::bind_cols() |>
    # ensure that all source variables are recorded as character values
    dplyr::mutate(dplyr::across(dplyr::starts_with("source_"), as.character))

  # check to see if df contains certain variables that need to be converted to
  # factors
  if("type" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(type = factor(type, levels = PLANET_TYPE_LEVELS))
  }
  if("atmosphere" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(atmosphere = factor(atmosphere, levels = ATMOSPHERE_LEVELS))
  }
  if("pressure" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(pressure = factor(pressure, levels = PRESSURE_LEVELS))
  }
  if("lifeForm" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(lifeForm = factor(lifeForm, levels = LIFEFORM_LEVELS))
  }
  if("lifeForm" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(lifeForm = factor(lifeForm, levels = LIFEFORM_LEVELS))
  }
  if("hpg" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(hpg = factor(hpg, levels = HPG_LEVELS))
  }
  if("hiringHall" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(hiringHall = factor(hiringHall, levels = HIRING_HALL_LEVELS))
  }

  return(df)
}


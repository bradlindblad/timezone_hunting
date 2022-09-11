

library(dplyr)
library(here)
library(lubridate)



get_sunrise_sunset_table <- function() {


  raw <- read.csv(here::here("data/raw_timetable.csv")) %>%
    dplyr::mutate(date = lubridate::as_date(date, format = "%a, %b %d")) %>%
    dplyr::mutate(date_mnth = lubridate::month(date)) %>%
    dplyr::mutate(date_day = lubridate::day(date)) %>%

    # dplyr::mutate(sunrise = strftime(as.POSIXct(sunrise, format = "%H:%M:%S"), format="%H:%M")) %>%
    # dplyr::mutate(sunset = strftime(as.POSIXct(sunset, format = "%H:%M:%S"), format="%H:%M")) %>%
    dplyr::mutate(sunrise = as.POSIXct(sunrise, format = "%H:%M")) %>%
    dplyr::mutate(sunset = as.POSIXct(sunset, format = "%H:%M")) %>%

    dplyr::arrange(date_mnth, date_day)  %>%
    dplyr::select(date_mnth, date_day, sunrise, sunset)

  return(raw)

}

get_today_date <- function() {

  m <- lubridate::today() %>%
    lubridate::month()

  d <- lubridate::today() %>%
    lubridate::day()
  return(c(m,d))

}

get_minutes_diff <- function(city) {

  if(toupper(city) == 'FARGO') {

    return(round(196/12.5))

  }

  if(toupper(city) == 'VALLEY CITY') {

    return(round(135/12.5))

  }

  if(toupper(city) == 'JAMESTOWN') {

    return(round(102/12.5))

  }

  if(toupper(city) == 'BISMARCK') {

    return(0)

  }

  if(toupper(city) == 'DICKINSON') {

    return(round(-98/12.5))

  }

  if(toupper(city) == 'BELFIELD') {

    return(round(-118/12.5))

  }




}

get_sunrise_sunset <- function(city) {

  # city = "fargo"
  diff <- get_minutes_diff(city)
  date <- get_today_date()
  df <- get_sunrise_sunset_table()

  final <- df %>%
    dplyr::filter(date_mnth == date[1],
                  date_day == date[2]) %>%
    dplyr::mutate(
      sunrise = sunrise - lubridate::duration(diff, "minutes"),
      sunset = sunset - lubridate::duration(diff, "minutes")
    ) %>%

    dplyr::mutate(
      shooting_start = strftime(sunrise  - lubridate::duration(30, "minutes"), format = "%H:%M"),
      shooting_end = strftime(sunset + lubridate::duration(30, "minutes"), format = "%H:%M"),
      location = city,
      date = paste(date_mnth, "/", date_day)
    ) %>%
    dplyr::select(location,date ,shooting_start, shooting_end)

return(final)

}


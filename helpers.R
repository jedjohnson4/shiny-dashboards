## Obtain API key and register to Google Static Map API ========================

API_KEY <- Sys.getenv("API_KEY")

register_google(API_KEY, account_type = "standard")


## Helper Functions ============================================================

plot_route <- function(file, maptype) {
  
  req(file)
  
  gpx <- read_gpx(file$datapath)
  
  tracks <- gpx$tracks
  
  df <- tracks[[1]]
  
  bbox <- c(min(df$Longitude), min(df$Latitude), max(df$Longitude), max(df$Latitude))
  
  map <- get_map(bbox, source = "google", maptype = tolower(maptype))
  
  ggmap(map) +
    geom_path(data = df,
              aes(x = Longitude, y = Latitude),
              color = "red",
              linewidth = 0.8) +
    theme_void()
  
}


plot_summary <- function(file, unit) {
  
  if (unit == "Imperial") {
    conversion <- 1609.34
    elevation_conversion <- 3.28
    unit_label <- "mi"
  } else {
    conversion <- 1000
    elevation_conversion <- 1
    unit_label <- "km"
  }
  
  req(file)
  
  gpx <- read_gpx(file$datapath)
  
  tracks <- gpx$tracks
  
  df <- tracks[[1]]
  
  df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S")
  
  RADIUS <- 6371000
  DEGREE_TO_RADIAN <- pi/180
  
  df1 <- df |> 
    select(c("Elevation", "Time", "Latitude", "Longitude", "hr")) |> 
    rename(
      "elev" = "Elevation",
      "time" = "Time",
      "lat" = "Latitude",
      "long" = "Longitude"
    ) |> 
    mutate(lat = lat * DEGREE_TO_RADIAN) |> 
    mutate(long = long * DEGREE_TO_RADIAN) |> 
    mutate(d = ifelse(is.na(lag(lat)), 0, RADIUS * acos((sin(lag(lat)) * sin(lat)) + cos(lag(lat)) * cos(lat) * cos(long - lag(long))))) |>
    mutate(distance = cumsum(d))
  
  lap_data <- df1 |> 
    mutate(mile = floor(distance / conversion)) |> 
    mutate(newMile = ifelse(mile == lag(mile), FALSE, TRUE)) |> 
    mutate(newMile = c(newMile[-n()], TRUE)) |> 
    mutate(newMile = ifelse(is.na(newMile), TRUE, newMile)) |> 
    filter(newMile) |> 
    mutate(mile = c(mile[-n()], (distance[n()] - distance[(n()-1)]) / conversion)) |> 
    mutate(mile = round(mile, 1)) |>
    mutate(dt = as.numeric(difftime(time, lag(time), units = "mins"))) |> 
    mutate(pace = dt / ((distance - lag(distance))/conversion)) |> 
    mutate(elev = elev * elevation_conversion) |> 
    mutate(dz = elev - lag(elev)) |> 
    filter(!is.na(pace)) |> 
    mutate(mile = factor(mile, rev(mile))) |> 
    mutate(normalized_pace = abs(pace - max(pace)) / (max(pace) - min(pace)) + 0.25)
  
  lap_hr <- df1 |> 
    mutate(mile = floor(distance / conversion)) |> 
    mutate(hr = as.numeric(hr)) |> 
    group_by(mile) |> 
    summarize(avg_hr = mean(hr)) |> 
    select(avg_hr)
  
  lap_data <- cbind(lap_data, lap_hr)
  
  lap_data |> 
    ggplot() +
    geom_col(aes(x = mile, y = normalized_pace),
             fill = "blue") +
    geom_text(aes(x = mile, y = -0.075, label = paste0(floor(pace), ":", sprintf("%02s", round((pace - floor(pace)) * 60, 0))))) +
    geom_text(aes(x = mile, y = 1.3, label = round(dz, 0))) +
    geom_text(aes(x = mile, y = 1.4, label = round(avg_hr, 0))) +
    annotate("text", x = length(lap_data$mile) + 1.5, y = -0.075, label = "PACE", fontface = "bold") +
    annotate("text", x = length(lap_data$mile) + 1.5, y = 1.3, label = "ELEV", fontface = "bold") +
    annotate("text", x = length(lap_data$mile) + 1.5, y = 1.4, label = "HR", fontface = "bold") +
    scale_y_continuous(limits = c(-0.1, 1.425)) +
    coord_flip(clip = "off") +
    theme_void() +
    theme(
      axis.text.y = element_text(family = "Arial",
                                 color = "black",
                                 size = 11,
                                 hjust = 1),
      plot.margin = unit(c(20,20,20,20), "pt")
    )
  
}


get_run_data_unit <- function(file, unit) {
  
  if (unit == "Imperial") {
    conversion <- 1609.34
    elev_conversion <- 3.28
    unit_label <- "mi"
    elev_label <- "ft"
  } else {
    conversion <- 1000
    elev_conversion <- 1
    unit_label <- "km"
    elev_label <- "m"
  }
  
  req(file)
  
  gpx <- read_gpx(file$datapath)
  
  tracks <- gpx$tracks
  
  df <- tracks[[1]]
  
  df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S")
  
  RADIUS <- 6371000
  DEGREE_TO_RADIAN <- pi/180
  
  df1 <- df |> 
    select(c("Elevation", "Time", "Latitude", "Longitude", "hr")) |> 
    rename(
      "elev" = "Elevation",
      "time" = "Time",
      "lat" = "Latitude",
      "long" = "Longitude"
    ) |> 
    mutate(lat = lat * DEGREE_TO_RADIAN) |> 
    mutate(long = long * DEGREE_TO_RADIAN) |> 
    mutate(d = ifelse(is.na(lag(lat)), 0, RADIUS * acos((sin(lag(lat)) * sin(lat)) + cos(lag(lat)) * cos(lat) * cos(long - lag(long))))) |>
    mutate(distance = cumsum(d)) |> 
    mutate(dz = ifelse(elev - lag(elev) > 0, elev - lag(elev), 0)) |> 
    mutate(dz = dz * elev_conversion)
  
  dt <- as.numeric(difftime(max(df1$time), min(df1$time), units = "secs"))
  dr <- max(df1$distance) / conversion
  
  avg_pace <- dt / 60 / dr
  
  c(
    paste(round(max(df1$distance) / conversion, 2), unit_label),
    paste0(floor(avg_pace), ":", sprintf("%02s", round((avg_pace - floor(avg_pace)) * 60, 0)), " min/", unit_label),
    paste(floor(sum(df1$dz, na.rm = TRUE)), elev_label)
    
  )
  
}

get_run_data_unitless <- function(file) {
  
  req(file)
  
  gpx <- read_gpx(file$datapath)
  
  tracks <- gpx$tracks
  
  df <- tracks[[1]]
  
  df1 <- df |> 
    mutate(hr = as.numeric(hr))
  
  c(
    paste(round(mean(df1$hr), 0), "bmp")
  )
  
}

sparkline_plot <- function(file) {
  
  req(file)
  
  gpx <- read_gpx(file$datapath)
  
  tracks <- gpx$tracks
  
  df <- tracks[[1]]
  
  df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S")
  
  RADIUS <- 6371000
  DEGREE_TO_RADIAN <- pi/180
  
  df1 <- df |> 
    select(c("Elevation", "Time", "Latitude", "Longitude", "hr")) |> 
    rename(
      "elev" = "Elevation",
      "time" = "Time",
      "lat" = "Latitude",
      "long" = "Longitude"
    ) |> 
    mutate(lat = lat * DEGREE_TO_RADIAN) |> 
    mutate(long = long * DEGREE_TO_RADIAN) |> 
    mutate(d = ifelse(is.na(lag(lat)), 0, RADIUS * acos((sin(lag(lat)) * sin(lat)) + cos(lag(lat)) * cos(lat) * cos(long - lag(long))))) |>
    mutate(hr = as.numeric(hr)) |> 
    mutate(dt = as.numeric(difftime(time, lag(time), units = "secs"))) |> 
    mutate(pace = ifelse(d == 0, 0, dt / d)) |> 
    mutate(normalized_pace = abs(pace - max(pace)) / (max(pace) - min(pace)))
  
  pace_sparkline <- df1 |> 
    ggplot() +
    geom_line(aes(x = time, y = normalized_pace), color = "#2800ff", linewidth = 0.75) +
    geom_area_pattern(aes(x = time, y = normalized_pace), 
                      color = NA,
                      pattern = "gradient",
                      pattern_fill = "#eae6ff",
                      pattern_fill2 = "#694dff") +
    coord_cartesian(ylim = c(0.5, 1)) +
    theme_void()
  
  hr_sparkline <- df1 |> 
    mutate(hr = hr / max(hr)) |> 
    ggplot() +
    geom_line(aes(x = time, y = hr), color = "red", linewidth = 0.75) +
    geom_area_pattern(aes(x = time, y = hr), 
                      color = NA,
                      pattern = "gradient",
                      pattern_fill = "#ffe6e6",
                      pattern_fill2 = "#ff8082") +
    theme_void()
  
  elev_sparkline <- df1 |> 
    mutate(elev = elev / max(elev)) |> 
    ggplot() +
    geom_line(aes(x = time, y = elev), color = "#ff9b00", linewidth = 0.75) +
    geom_area_pattern(aes(x = time, y = elev), 
                      color = NA,
                      pattern = "gradient",
                      pattern_fill = "#fff5e6",
                      pattern_fill2 = "#ffc366") +
    theme_void()
  
  list(
    pace_sparkline,
    hr_sparkline,
    elev_sparkline
  )
  
}


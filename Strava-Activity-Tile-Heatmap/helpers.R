## Generate Heatmap Plot =======================================================


plot_heatmap <- function(file, activity_type, fill_variable, color, fill_range) {
  
  if (color == "Green") {
    fill_color <- "#146910"
  }
  if (color == "Blue") {
    fill_color <- "#0e108c"
  }
  if (color == "Red") {
    fill_color <- "#9c1313"
  }
  if (color == "Cyan") {
    fill_color <- "#26fffb"
  }

  
  
  req(file)
  
  strava <- read.csv(file$datapath)
  
  strava_mod <- strava |> 
    select(c(Activity.Date, Activity.Type, Elapsed.Time, Distance)) |> 
    rename(datetime = Activity.Date,
           type = Activity.Type,
           time = Elapsed.Time,
           distance = Distance) |>
    mutate(datetime = as.POSIXct(datetime, format = "%b %d, %Y")) |> 
    filter(datetime > as.POSIXct("2024-01-01"))
  
  if (activity_type != "ALL") {
    strava_mod <- strava_mod |> 
      filter(type == activity_type)
  }
  
  days_run <- strava_mod |> 
    group_by(datetime) |> 
    summarize(distance = sum(distance),
              time = sum(time)) |> 
    mutate(datetime = as.Date(datetime))
  
  days_of_year <- seq(ymd("2024-01-01"), ymd("2024-12-31"), by = "day")
  
  row_number <- c()
  column_number <- c()
  
  for (i in seq(1, 52, 1)){
    for (j in seq(1, 7, 1)){
      row_number <- c(row_number, j)
      column_number <- c(column_number, i)
    }
  }
  
  row_number <- c(row_number, 1, 2, 3)
  column_number <- c(column_number, 53, 53, 53)
  
  row_number <- row_number[-1]
  column_number <- column_number[-1]
  
  year_2024 <- data.frame(day = days_of_year,
                          row = row_number,
                          col = column_number)
  
  year_2024 <- year_2024 |> 
    mutate(didRun = case_when(
      day %in% days_run$datetime ~ T,
      .default = F))
  
  combined_year_2024 <- left_join(year_2024, days_run, by = c("day" = "datetime"))
  
  combined_year_2024 |> 
    ggplot() +
    geom_tile(aes(x = col, y = row, fill = didRun, alpha = eval(as.name(tolower(fill_variable)))),
              width = 0.8,
              height = 0.8,
              color = NA,
              show.legend = FALSE) +
    scale_fill_manual(values = c("grey80", fill_color)) +
    scale_alpha_continuous(range = fill_range) +
    coord_fixed() +
    scale_x_continuous(expand = c(0, 0),
                       breaks = c(1, 5, 9,14, 18, 22, 27, 31, 35, 40, 44, 48),
                       labels = month.abb,
                       sec.axis = dup_axis()) +
    scale_y_reverse(breaks = c(1, 2, 3, 4, 5, 6, 7),
                    labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
    theme_void() +
    theme(
      axis.text.y = element_text(family = "Arial",
                                 color = "black",
                                 size = 9,
                                 margin = margin(r = 6),
                                 hjust = 1),
      axis.text.x.bottom = element_blank(),
      axis.text.x.top = element_text(family = "Arial",
                                     color = "black",
                                     size = 9,
                                     hjust = 0,
                                     angle = 90),
      plot.background = element_rect(fill = "white",
                                     color = NA),
      plot.margin = unit(c(5,10,5,5), "pt")
    )

}
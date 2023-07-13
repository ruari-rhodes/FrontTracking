
#' read_rsmc_besttrack
#'
#' @param file Path to a file containing data from RMSC best track: https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/besttrack.html
#'
#' @return A data.frame containing columns timestep, longitude, latitude, rel_vor_850
#'
read_rsmc_besttrack <- function(file) {


  # Read in raw data --------------------------------------------------------
  raw_data <- readLines(file)


  # Identify where each track begins and ends -------------------------------
  track_start <- which(substr(raw_data, 1, 5) == "66666")
  track_end <-
    c(track_start[2:length(track_start)], length(raw_data) + 1) - 1


  # Create progress bar and message to the user --------------------------
  pb = txtProgressBar(min = 0,
                      max = length(track_start),
                      initial = 0)
  message(sprintf("Processing %i tracks", length(track_start)))

  # Create output list --------------------------------------------------
  track_data <- list()


  # Iterate over all tracks, store results as a list of data.frames -------
  for (i in 1:length(track_start)) {
    # Update progress bar
    setTxtProgressBar(pb, i)

    start_row <- track_start[i] + 1
    end_row <- track_end[i]

    track_data     <- raw_data[start_row:end_row]
    meta_data       <- raw_data[track_start[i]]

    # Columns in text are determined by character count (fixed width cols)
    processed_data <- tibble::tibble(
      timestamp = substr(track_data, 1, 8),
      indicator = substr(track_data, 10, 12),
      grade     = substr(track_data, 14, 14),
      latitude  = substr(track_data, 16, 18),
      longitude = substr(track_data, 20, 23),
      cpres_hpa = substr(track_data, 25, 28),
      vmaraw_data_sustained_kt  = substr(track_data, 34, 36),
      direction_largest_50kt_radius  = substr(track_data, 42, 42),
      length_largest_50kt_radius  = substr(track_data, 43, 46),
      length_shortest_50kt_radius  = substr(track_data, 48, 51),
      direction_largest_30kt_radius = substr(track_data, 53, 53),
      length_largest_30kt_radius = substr(track_data, 54, 57),
      length_shortest_30kt_radius = substr(track_data, 59, 62),
      landfall = substr(track_data, 72, 72)
    )

    # Add metadata
    processed_data$int_number_id <- substr(meta_data, 7, 10)
    processed_data$trop_cycl_id  <- substr(meta_data, 17, 20)
    processed_data$storm_name    <- substr(meta_data, 31, 50)
    processed_data$last_revised  <- substr(meta_data, 65, 72)

    # Assign appropriate data types, process timestamps, and re-order
    processed_data <- processed_data %>%
      mutate(date = as.Date(
        sprintf("%s%s-%s-%s",
                if_else(as.numeric(substr(timestamp,1,2)) < 51, "19","20"), # y2k strikes! Assume all dates starting > 50 are 19xx, else 20xx
                substr(timestamp, 1, 2),
                substr(timestamp, 3, 4),
                substr(timestamp, 5, 6))
      ),
      time = substr(timestamp, 7,  8),
      last_revised = as.Date(
        sprintf("%s-%s-%s",
                substr(last_revised,1,4),
                substr(last_revised,5,6),
                substr(last_revised,7,8)
        )
      )) |>
      mutate_at(vars(-c(storm_name, date)), ~as.numeric(as.character(.))) |> # assign data types
      mutate(longitude = longitude / 10,
             latitude = latitude / 10) |>
      select(trop_cycl_id,
             date,
             time,
             int_number_id,
             storm_name,
             last_revised,
             indicator:landfall
      )


    track_data[[i]] <- raw_data_df
  }

  # Close out progress bar and message to user --------------------------
  close(pb)
  message("Finished")


  # Set up output as a single dataframe and declare data types ----------------
  track_output <- dplyr::bind_rows(track_data)

  return(track_output)

}

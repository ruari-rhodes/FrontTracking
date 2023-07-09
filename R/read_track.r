#
# file <- "Tracks/DJF/NH_FILT/ERA5_20132014_TRACKS_FILTERED_pos"
# start_date <- "2013-12-01"
# output <- "sf_lines"
# negative_longitudes <- TRUE
# extent = c(xmin = -70, xmax = 25, ymin = 20, ymax = 75)
# x <-  read_track(file = file, start_date = start_date, output = output, extent = extent, negative_longitudes = negative_longitudes)

#' read_track
#'
#' @param file Path to a file containing data from Hodges' TRACK algorithm
#'
#' @return A data.frame containing columns timestep, longitude, latitude, rel_vor_850
#'
read_track <- function(file) {


  # Read in raw data --------------------------------------------------------
  raw_data <- readLines(file)


  # Identify where each track begins and ends -------------------------------
  track_start <- which(substr(raw_data, 1, 8) == "TRACK_ID")
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

    start_row <- track_start[i] + 2
    end_row <- track_end[i]

    x <- tibble::tibble(raw_data = raw_data[start_row:end_row])
    x_df <-
      tidyr::separate(
        x,
        raw_data,
        into = c("timestep", "longitude", "latitude", "rel_vor_850"),
        sep = " ",
        extra = "drop"
      )
    x_df$track_id <- i

    track_data[[i]] <-
      x_df[, c("track_id",
               "timestep",
               "longitude",
               "latitude",
               "rel_vor_850")]
  }

  # Close out progress bar and message to user --------------------------
  close(pb)
  message("Finished")


  # Set up output as a single dataframe and declare data types ----------------
  track_output <- bind_rows(track_data)
  track_output$timestep <- as.numeric(track_output$timestep)
  track_output$longitude <- as.numeric(track_output$longitude)
  track_output$latitude <- as.numeric(track_output$latitude)
  track_output$rel_vor_850 <- as.numeric(track_output$rel_vor_850)


  return(track_output)

}



#' add_timestamp_to_tracks
#'  Adds date/time information to the output of read_track()
#'  Adds columns "day", "time", and (optionally) "date" (if start_date supplied)
#' @param track_df data.frame, output of read_track()
#' @param start_date (optional) a Date object indicating the start date of the track file
#'
#' @return data.frame containing all supplied columns plus day, time, date (if start_date supplied)
#'
add_timestamp_to_tracks <- function(track_df, start_date) {
  track_df$day <- ceiling(track_df$timestep / 4)
  track_df$time <- case_when(
    track_df$timestep %% 4 == 1 ~ "00:00",
    track_df$timestep %% 4 == 2 ~ "06:00",
    track_df$timestep %% 4 == 3 ~ "12:00",
    track_df$timestep %% 4 == 0 ~ "18:00",
    TRUE ~ "XX:XX"
  )
  if (!is.null(start_date)) {
    track_df$date <- as.Date(start_date) + (track_df$day - 1)
  } else {
    track_df$date <- NA
  }
  return(track_df)
}

convert_track_to_negative_longitude <- function(track_df) {
  track_df$longitude <-
    dplyr::if_else(track_df$longitude > 180,
            track_df$longitude - 360,
            track_df$longitude)

  return(track_df)

}

#' crop_tracks_to_extent
#' Crop the output of read_track to a given spatial extent
#'
#' @param track_df data.frame, output of read_track()
#' @param extent numeric vector containing entries xmin, xmax, ymin, ymax
#'
#' @return data.frame
crop_tracks_to_extent <-
  function(track_df,
           extent = c(
             xmin = -70,
             xmax = 25,
             ymin = 20,
             ymax = 75
           )) {

    dplyr::filter(
      track_df,
      longitude >= extent["xmin"],
      longitude <= extent["xmax"],
      latitude >= extent["ymin"],
      latitude <= extent["ymax"]
    )

  }


#' track_to_sf_points
#' Convert the data.frame output of read_track to a SimpleFeatures POINT object
#' for use with GIS functionality
#'
#' @param track_df data.frame, output of read_track()
#'
#' @return sf (POINTS)
track_to_sf_points <- function(track_df) {

  sf::st_as_sf(track_df,
           coords = c("longitude", "latitude"),
           crs = sf::st_crs(4326))
}

#' track_to_sf_lines
#' Convert the data.frame output of read_track to a SimpleFeatures LINES object
#' for use with GIS functionality
#'
#' @param track_df data.frame, output of read_track()
#'
#' @return sf (LINES)
track_to_sf_lines <- function(track_df) {

  # Remove tracks with <3 locations
  valid_tracks <-
    track_df %>%
    dplyr::group_by(track_id) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 2) %>%
    dplyr::pull(track_id)

  track_df %>%
    dplyr::filter(track_id %in% valid_tracks) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
             crs = sf::st_crs(4326)) %>%
    dplyr::group_by(track_id) %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING")
}

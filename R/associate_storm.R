



#'associate_storm
#'
#'Identifies the most likely storm track point to be associated
#'with a weather event in a defined interest region, based on the ability of the
#'algorithm to draw a path through a continuous region of precipitation. The
#'function first identifies regions of precipitation exceeding a given
#'threshold. It then attempts to draw lines through these regions of
#'precipitation to connect the region of interest with storm track centres. If
#'more than one track is found to be linked to the interest region via
#'precipitation bands, the track joined to the interest region via the shortest
#'path through precipitation bands is selected.
#'
#'@param storms data.frame, output of [FrontTracking::read_track()]
#'@param precip Raster or spatRaster map of precipitation
#'@param interest sf Polygons object detailing the region of interest
#'@param storm_radius numeric, a threshold identifying how near to a precip band
#'  a track centre must be to be considered to be associated with the band.
#'@param precip_threshold numeric, a threshold value identifying how intense
#'  precipitation must be to be considered to be a "band"
#'@param return_plot boolean, should a plot be generated demonstrating the output?
#'
#'@return list containing Precip, PrecipRegions, MatchedStorm, MatchedPath,
#'  Interest, AllStorms, StormBuffers, Plot
#'
#'@seealso
#'  [FrontTracking::read_track()] (required for loading Hodges' TRACK data)
#'
#'  [terra::rast()] (required for loading NCDF precipitation data)
#'
#'@examples
#'example_precip <- terra::rast(system.file("extdata", "EXAMPLE_era5_precip.tif", package = "FrontTracking"))
#'
#'results <- associate_storm(
#'  storms = example_storms,
#'  precip = example_precip,
#'  interest = england_wales_box,
#'  storm_radius = 250,
#'  precip_threshold = 1.5
#' )
associate_storm <- function(storms,   # Simple feature collection POINTS (many)
                            precip,   # spatRaster
                            interest, # Simple feature collection POLYGON (one)
                            storm_radius, # Numeric
                            precip_threshold, # Numeric
                            return_plot = TRUE
){

  # If required, convert precip from raster to spatRaster -------------------
  if(class(precip) == "RasterLayer") precip <- as(precip, "SpatRaster")



  # Find map extents from precip raster -------------------------------------
  extents <- c(terra::ext(precip)[1],
               terra::ext(precip)[2],
               terra::ext(precip)[3],
               terra::ext(precip)[4])



  # Find centroid of interest region -----------------------------------
  # NB: st_centroid will throw a warning due to the lat/lon projection of the
  # input data. Since strict accuracy is not necessary, we can suppress this
  # warning.
  interest_centroid <- suppressWarnings({
    sf::st_centroid(interest)
  })

  # Create precip contour ---------------------------------------------
  # Create contour
  precip_contour <- mapiso::mapiso(precip, breaks = precip_threshold)
  # Exclude the outer contour
  precip_contour <- subset(precip_contour, isomin == precip_threshold)



  # Process storm tracks ----------------------------------------------

  # Crop stormtracks to precip data region
  # NB: again this throws a warning due to that lat/lon projection. We are
  # safe to suppress this.
  storms_cropped <- suppressWarnings({
    sf::st_crop(storms, terra::ext(precip))
  })

  # Create 250km buffer around tracks
  storms_lambert <- sf::st_transform(storms_cropped, "EPSG:3035")
  storms_buffer <- sf::st_buffer(storms_lambert, storm_radius * 1000)

  # Reproject buffer back to lat/lon for consistency
  storms_buffer <- sf::st_transform(storms_buffer, "EPSG:4326")



  # Create a mask layer describing location of fronts, storm tracks, --------
  # and target region

  # Create mask layer to generate paths through fronts and track buffers
  mask_base <- precip
  mask_base[] <- 1

  # Need to convert masking vectors to SpatVector for compatibility with Terra
  precip_contour_sv <- terra::vect(precip_contour)
  storms_buffer_sv <- terra::vect(storms_buffer)
  interest_sv <- terra::vect(interest)

  # Add precip contours to mask
  mask_precip <- terra::mask(mask_base, precip_contour_sv)
  mask_precip[is.na(mask_precip)] <- 0

  # Add track buffers to mask
  mask_track <- terra::mask(mask_base, storms_buffer_sv)
  mask_track[is.na(mask_track)] <- 0

  # Add interest area to mask
  mask_interest <- terra::mask(mask_base, interest_sv)
  mask_interest[is.na(mask_interest)] <- 0

  # Combine mask layers
  mask_full <- mask_precip + mask_track + mask_interest
  mask_full[mask_full > 0] <- 1
  mask_full[mask_full == 0] <- NA

  # DEV NOTE: Currently gDistance only accepts raster::raster inputs,
  # not terra::spatRaster. For now, the mask will be converted to a raster
  # object for distance calculation. However, note that many of the
  # raster prerequisites are being deprecated, and the geospatial R community
  # is moving towards replacing raster operations with terra.
  # This section should be re-visited if/when gDistance is updated or
  # an alternative package produced that accepts terra inputs.
  mask_full <- as(mask_full, "Raster")

  # Find nearest connected storm centre -------------------------------------

  # Create SpatialPoints object with point locations
  # Need to automate this out of interest_centroid and storm_tracks


  # Convert storm tracks to SPDF for compatibility with gDistance
  # DEV NOTE: Note the deprecation warning above - replace with sf when possible.
  track_pts <- as(storms, "Spatial")
  interest_pt <- as(interest_centroid, "Spatial")


  # Create a transition layer using gdistance package
  tl <- gdistance::transition(mask_full, transitionFunction = mean, directions = 4)

  # Attempt to find the shortest valid path from the interest region centroid
  # to each storm track point. If no valid path can be found, NA is returned
  paths <- list()
  for(i in 1:nrow(track_pts)){
    paths[[i]] <- tryCatch(
      suppressWarnings({gdistance::shortestPath(tl, interest_pt, track_pts[i,], output = "SpatialLines")}),
      error = function(e){return(NA)})
  }

  # Continue if any valid paths are found
  if(any(!is.na(paths))){
    candidate_pts <- track_pts[which(!is.na(paths)),]
    candidate_paths <- paths[!is.na(paths)]

    # If multiple possible tracks are found, get their lengths and return the shortest
    line_lengths <- list()
    if(length(candidate_paths) > 1){
      for(i in 1:length(candidate_paths)){
        line_lengths[[i]] <- geosphere::lengthLine(candidate_paths[[i]])
      }
    } else {
      line_lengths[[1]] <- geosphere::lengthLine(candidate_paths[[1]])
    }
    line_lengths <- unlist(line_lengths)
    shortest_line <- which.min(line_lengths)
    matched_pt <- sf::st_as_sf(candidate_pts[shortest_line,])
    matched_path <- sf::st_as_sf(candidate_paths[[shortest_line]])
    sf::st_crs(matched_path) <- 4326

    # Draw map -------------------------------------------------------------
    if(return_plot){
      p <-
        ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = precip) +
        ggplot2::geom_sf(data = world_map_sf, colour = "darkgrey", fill = NA) +
        ggplot2::geom_sf(data = precip_contour, ggplot2::aes(colour = "Precipitation regions"), fill = NA) +
        ggplot2::geom_sf(data = storms, ggplot2::aes(colour = "Storm track points"), size = 3) +
        ggplot2::geom_sf(data = matched_pt, ggplot2::aes(colour = "Matched storm"), size = 3) +
        ggplot2::geom_sf(data = storms_buffer, ggplot2::aes(colour = "Storm track tolerance"), size = 2, fill = NA) +
        ggplot2::geom_sf(data = interest, ggplot2::aes(colour = "Region of interest"), size = 2, fill = NA) +
        ggplot2::geom_sf(data = matched_path, ggplot2::aes(colour = "Matched path")) +
        ggplot2::scale_fill_viridis_c("Total precip (mm/hour)") +
        ggplot2::scale_colour_manual(
          "Legend",
          values = c(
            "Precipitation regions" = "red",
            "Matched storm" = "red",
            "Storm track points" = "orange",
            "Storm track tolerance" = "orange",
            "Region of interest" = "purple",
            "Matched path" = "green"
          )
        ) +
        ggplot2::scale_x_continuous(limits = c(extents["xmin"], extents["xmax"])) +
        ggplot2::scale_y_continuous(limits = c(extents["ymin"], extents["ymax"]))
    }
  } else {
    candidate_pts <- NA
    candidate_paths <- NA
    matched_pt <- NA
    matched_path <- NA

    if(return_plot){
      p <-
        ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = precip) +
        ggplot2::geom_sf(data = world_map_sf, colour = "darkgrey", fill = NA) +
        ggplot2::geom_sf(data = precip_contour, ggplot2::aes(colour = "Precipitation regions"), fill = NA) +
        ggplot2::geom_sf(data = storms, ggplot2::aes(colour = "Storm track points"), size = 3) +
        ggplot2::geom_sf(data = storms_buffer, ggplot2::aes(colour = "Storm track tolerance"), size = 2, fill = NA) +
        ggplot2::geom_sf(data = interest, ggplot2::aes(colour = "Region of interest"), size = 2, fill = NA) +
        ggplot2::scale_fill_viridis_c("Total precip (mm/hour)") +
        ggplot2::scale_colour_manual(
          "Legend",
          values = c(
            "Precipitation regions" = "red",
            "Matched storm" = "red",
            "Storm track points" = "orange",
            "Storm track tolerance" = "orange",
            "Region of interest" = "purple",
            "Matched path" = "green"
          )
        ) +
        ggplot2::scale_x_continuous(limits = c(extents["xmin"], extents["xmax"])) +
        ggplot2::scale_y_continuous(limits = c(extents["ymin"], extents["ymax"]))
    }
  }

  # Arrange results ---------------------------------------------------------
  output <- list(
    Precip = precip,
    PrecipRegions = precip_contour,
    MatchedStorm = matched_pt,
    MatchedPath = matched_path,
    Interest = interest,
    AllStorms = storms_cropped,
    StormBuffers = storms_buffer
  )

  if(return_plot){
    output$Plot <- p
  }

  return(output)

}

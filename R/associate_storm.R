



associate_storm <- function(storms,   # Simple feature collection POINTS (many)
                            precip,   # spatRaster
                            interest, # Simple feature collection POLYGON (one)
                            storm_radius, # Numeric
                            precip_threshold # Numeric
){
  
  require(mapiso)
  require(terra)
  require(sf)
  require(gdistance)
  require(geosphere)
  require(tidyterra)
  
  
  # NB: requiring raster and sp will throw a deprecation warning. This is currently
  # required for interaction with gDistance, but should be replaced entirely
  # with terra once gDistance is updated or an alternative is produced.
  require(raster)
  require(sp)
  

  # If required, convert precip from raster to spatRaster -------------------
  if(class(precip) == "RasterLayer") precip <- as(precip, "SpatRaster")
  
  

  # Find map extents from precip raster -------------------------------------
  extents <- c(ext(precip)[1],
               ext(precip)[2],
               ext(precip)[3],
               ext(precip)[4])
  
  
  
  # Find centroid of interest region -----------------------------------
  interest_centroid <- st_centroid(interest)
  
  # Create precip contour ---------------------------------------------
  # Create contour
  precip_contour <- mapiso(precip, breaks = precip_threshold)
  # Exclude the outer contour
  precip_contour <- subset(precip_contour, isomin == precip_threshold)
  
  
  
  # Process storm tracks ----------------------------------------------
  
  # Crop stormtracks to precip data region 
  storms_cropped <- st_crop(storms, terra::ext(precip))
  
  # Create 250km buffer around tracks
  storms_lambert <- st_transform(storms_cropped, "EPSG:3035")
  storms_buffer <- st_buffer(storms_lambert, storm_radius * 1000)
  
  # Reproject buffer back to lat/lon for consistency
  storms_buffer <- st_transform(storms_buffer, "EPSG:4326")
  
  
  
  # Create a mask layer describing location of fronts, storm tracks, --------
  # and target region
  
  # Create mask layer to generate paths through fronts and track buffers
  mask_base <- precip
  mask_base[] <- 1
  
  # Need to convert masking vectors to SpatVector for compatibility with Terra
  precip_contour_sv <- vect(precip_contour)
  storms_buffer_sv <- vect(storms_buffer)
  interest_sv <- vect(interest)
  
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
  tl <- transition(mask_full, transitionFunction = mean, directions = 4)
  
  # Attempt to find the shortest valid path from the interest region centroid
  # to each storm track point. If no valid path can be found, NA is returned
  paths <- list()
  for(i in 1:nrow(track_pts)){
    paths[[i]] <- tryCatch(
      shortestPath(tl, interest_pt, track_pts[i,], output = "SpatialLines"),
      error = function(e){return(NA)})
  }
  
  candidate_pts <- track_pts[which(!is.na(paths)),]
  candidate_paths <- paths[!is.na(paths)]
  
  # If multiple possible tracks are found, get their lengths and return the shortest
  line_lengths <- list()
  if(length(candidate_paths) > 1){
    for(i in 1:length(candidate_paths)){
      line_lengths[[i]] <- lengthLine(candidate_paths[[i]])
    } 
  } else {
    line_lengths[[1]] <- lengthLine(candidate_paths[[1]])
  }
  line_lengths <- unlist(line_lengths)
  shortest_line <- which.min(line_lengths)
  matched_pt <- st_as_sf(candidate_pts[shortest_line,])
  matched_path <- st_as_sf(candidate_paths[[shortest_line]])
  
  
  # Load basemap ----------------------------------------------------------
  world_map_sf <- read_rds('data/world_map_sf.rds')
  
  
  # Draw map -------------------------------------------------------------
  p <- 
    ggplot() +
    geom_spatraster(data = precip) + 
    geom_sf(data = world_map_sf, colour = "darkgrey", fill = NA) +
    geom_sf(data = precip_contour, aes(colour = "Precipitation regions"), fill = NA) + 
    geom_sf(data = storms, aes(colour = "Storm track points"), size = 3) + 
    geom_sf(data = matched_pt, aes(colour = "Matched storm"), size = 3) +
    geom_sf(data = storms_buffer, aes(colour = "Storm track tolerance"), size = 2, fill = NA) + 
    geom_sf(data = interest, aes(colour = "Region of interest"), size = 2, fill = NA) +
    geom_sf(data = matched_path, aes(colour = "Matched path")) +
    scale_fill_viridis_c("Total precip (mm/hour)") +
    scale_colour_manual(
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
    scale_x_continuous(limits = c(extents["xmin"], extents["xmax"])) +
    scale_y_continuous(limits = c(extents["ymin"], extents["ymax"]))

  # Arrange results ---------------------------------------------------------
  output <- list(
    Precip = precip,
    PrecipRegions = precip_contour,
    MatchedStorm = matched_pt,
    MatchedPath = matched_path,
    Interest = interest,
    AllStorms = storms_cropped,
    StormBuffers = storms_buffer,
    Plot = p
  )
  
  return(output)
  
}

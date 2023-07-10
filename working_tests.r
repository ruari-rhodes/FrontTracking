

example_precip <- terra::rast(system.file("extdata", "EXAMPLE_era5_precip.tif", package = "FrontTracking"))

results <- associate_storm(
  storms = example_storms,
  precip = example_precip,
  interest = england_wales_box,
  storm_radius = 250,
  precip_threshold = 1.5

)

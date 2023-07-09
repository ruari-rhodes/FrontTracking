

# Run association function ------------------------------------------------

example_precip <- terra::rast("data/EXAMPLE_era5_precip.tif")

assoc_results <- associate_storm(example_storms, example_precip, england_wales_box, 250, 1.5)






library(sf)
library(arrow)
library(tibble)

# Load your shapefile
shp <- st_read("inputs/eCognition_LTA_20250520.shp")

# Convert to tibble and write as Parquet
shp_tbl <- tibble::as_tibble(shp)
write_parquet(shp_tbl, "outputs/full_ltas.parquet")




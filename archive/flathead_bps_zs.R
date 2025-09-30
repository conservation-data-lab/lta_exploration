
# Notes ----
## flathead nf BpS zonal stats output wranging
## Randy Swaty
## July 2, 2025



# Set up ----


library(RColorBrewer)
library(sf)
library(tidyverse)

# Read in input (raw) data
# zonal stats output from QGIS, BpSs per LTA for Ouachita NF
flathead_ltas_bps <- st_read("inputs/flathead_ltas_bps.gpkg", quiet = TRUE)


# Read in additional data (e.g. attributes) 

bps_attributes <- read_csv('inputs/LF20_BPS_220.csv') |>
  # remove unwanted columns by location-easier than spelling them out by name, but assumes they don't move!  Check output to make sure.
  select(-c(2,3, 11:20))


# Wrangle BpS-LTA data ----

# get list
column_names <- colnames(flathead_ltas_bps)

# make dataframe-easier to look at
column_names_df <- data.frame(Column_Names = column_names)

# whoa 171 columns!  We only need the ones that indicate the LTA and BPS, plus the geometry.  Luckily we can do this easily!

# Clean and pivot data
clean_flathead_ltas_bps <- flathead_ltas_bps |>
  # keep the UID, geom and columns that start with "BPS_"
  select(UID, geom, starts_with("BPS_"))

# looks good, but is wide!

clean_flathead_ltas_bps_long <- clean_flathead_ltas_bps |>
  # Pivot columns starting with "BPS_" into long format
  pivot_longer(cols = starts_with("BPS_"), names_to = "bps_value", values_to = "count") |>
  # Remove the "BPS_" prefix from the bps_value column
  mutate(bps_value = as.numeric(str_remove(bps_value, "^BPS_"))) |>
  # Reorder columns to have UID, count, bps_value, and geom
  select(UID, count, bps_value, geom)

# yay!  One more thing, we want BPS names and other information

# Join in BpS attributes

final_flathead_lta_bps <- clean_flathead_ltas_bps_long |>
  # drop geometry for now to save to .csv
  st_drop_geometry() |>
  # join in attributes, noting that the datasets have different names for the joining columns
  left_join(bps_attributes, by = c("bps_value" = "VALUE")) 

write.csv(final_flathead_lta_bps, file = "outputs/inal_flathead_lta_bps.csv")





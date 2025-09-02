# Prepare and save data into the package

library(httr)
library(sf)
library(dplyr)
library(units)
library(devtools)
library(usethis)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Antelope herd units and hunt areas ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Read latest spatial data for herd unit and hunt area boundaries directly
# from WGFD central GIS
# https://wyoming-wgfd.opendata.arcgis.com/

# Function by Eric Newkirk to read programatically
wgfd_agol_data <- function(layer_name, crs = 4326) {

  # base url
  dl_url <- httr::parse_url("https://services6.arcgis.com/")

  # path for correct dataset
  dl_url$path <- list(
    "cWzdqIyxbijuhPLw", "arcgis", "rest", "services",
    layer_name, "FeatureServer", "0", "query"
  )

  # query options including crs
  dl_url$query <- list(
    where = "1=1",
    outFields = "*",
    outSR = crs,
    f = "geojson"
  )

  # create sf object directly from url
  sf::read_sf(httr::build_url(dl_url))

}


# HUNT AREAS
huntAreas <- wgfd_agol_data("AntelopeHuntAreas")

huntAreas <- huntAreas %>%
  mutate(AREA = units::set_units(sf::st_area(huntAreas), "mi^2")) %>%
  select(HUNTAREA,
         HUNTNAME,
         HERDUNIT,
         HERDNAME,
         AREA) %>%
  arrange(HUNTAREA)

dim(huntAreas)  # 102 6


# HERD UNITS
herdUnits <- wgfd_agol_data("AntelopeHerdUnits")

herdUnits <- herdUnits %>%
  mutate(AREA = units::set_units(sf::st_area(herdUnits), "mi^2")) %>%
  select(HERDUNIT,
         HERDNAME,
         AREA) %>%
  arrange(HERDUNIT)

dim(herdUnits)  # 40 4


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Pronghorn LT distance bins ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Nominal distance bins
cutPoints <- units::set_units(c(0, 20, 45, 80, 145, 200), "m")
midPoints <- cutPoints[-length(cutPoints)] + diff(cutPoints)/2
offset <- units::set_units(65, "m")

(distanceBins <- data.frame(distBin = c("Alpha",
                                        "Bravo",
                                        "Charlie",
                                        "Delta",
                                        "Echo"),
                            left_ground = cutPoints[1:5] + offset,
                            right_ground = cutPoints[2:6] + offset,
                            left_offset = cutPoints[1:5],
                            right_offset = cutPoints[2:6],
                            mid_offset = cutPoints[1:5] + diff(cutPoints)/2,
                            width = diff(cutPoints)))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Update documentation ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Reminder to update the help documentation as needed
# For example, the huntAreas.R and herdUnits.R files have a "current as of" date.

# Then re-run the documentation creator
devtools::document()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Write out ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Save into package
# Creates .rda file for each in /data directory
usethis::use_data(huntAreas, herdUnits, distanceBins,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress = "bzip2")


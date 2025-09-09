library(sf)

load_police_force_kml <- function(filename) {
  path <- paste0("data/force kmls/", filename)
  area <- st_read(path)
  police_force <- gsub("-", "_", filename)
  police_force <- substr(police_force, 1, nchar(filename)-4)
  area$police_force <- police_force
  return(area)
}

file_names <- list.files(path = "data/force kmls")

areas <- do.call(rbind, lapply(file_names, load_police_force_kml))




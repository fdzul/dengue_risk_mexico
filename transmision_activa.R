# Step 1. load the AGEE ####
mex <- rgeomex::AGEE_inegi19_mx


# Step 3. make the tible link ####
link <- tibble::tibble(CVE_ENT = c("12", "14", "17",
                                   "19", "25", "28",
                                   "30"),
                       link = c(
                           
                           "https://lively-kheer-564a7a.netlify.app"))

# Step 3. left joint ####
mex_link <- dplyr::left_join(x = link,
                             y = mex,
                             by = "CVE_ENT") |>
    dplyr::mutate(Estado = paste0(": <a href=", 
                                  link,">", 
                                  NOMGEO, "</a>")) |>
    as.data.frame() |>
    sf::st_set_geometry(value = "geometry") 

mex_link |>
    mapview::mapview(popup = "Estado",
                     legend = FALSE,
                     color = "#e6d194", 
                     alpha.regions = 1,
                     col.regions = "#691C32")
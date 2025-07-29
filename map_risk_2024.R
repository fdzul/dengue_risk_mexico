# Step 1. load the AGEE ####
mex <- rgeomex::AGEE_inegi19_mx

# Step 3. make the tible link ####
link <- tibble::tibble(CVE_ENT = c("01","02", "03", "04", "05",
                                   
                                   "06", "07", "09", "10",
                                   
                                   "11", "12", "13", "14", "15",
                                   
                                   "16", "17", "18", "19", "20",
                                   
                                   "21", "23","24", "25",
                                   
                                   "26", "27", "28", "30",
                                   
                                   "31", "32"),
                       link = c( # 01, 02, 03, 04, 05
                           "https://dashing-manatee-5aedb8.netlify.app/#hotspots-de-vector",
                           "https://dazzling-taffy-ac0fd2.netlify.app",
                           "https://fascinating-youtiao-a1f6f7.netlify.app",
                           "https://phenomenal-hotteok-02e188.netlify.app",
                           "https://strong-kelpie-ecc3ea.netlify.app",
                           
                           # 06, 07, 08, 09, 10
                           "https://silver-moxie-30857b.netlify.app",
                           "https://adorable-tiramisu-303de1.netlify.app",
                           # 08
                           "https://euphonious-crostata-930ad9.netlify.app",
                           "https://preeminent-nougat-6307ee.netlify.app",
                           
                           # 11, 12, 13, 14, 15
                           "https://sparkly-concha-17d36d.netlify.app",
                           "https://astounding-squirrel-94dd18.netlify.app",
                           "https://incredible-arithmetic-3dc5ed.netlify.app",
                           "https://heartfelt-fudge-936865.netlify.app",
                           "https://inquisitive-tartufo-8d5181.netlify.app",
                           
                           # 16, 17, 18, 19, 20
                           "https://fabulous-mermaid-31ea6a.netlify.app",
                           "https://glistening-griffin-b8023f.netlify.app",
                           "https://idyllic-granita-6918af.netlify.app",
                           "https://starlit-lolly-ad7d14.netlify.app",
                           "https://merry-figolla-08f5bf.netlify.app",
                           
                           # 21, 22, 23, 24, 25
                           "https://snazzy-figolla-e17977.netlify.app", # puebla
                           # 22 queteraro
                           "https://helpful-pegasus-ba8bb9.netlify.app", # cancun
                           "https://wonderful-belekoy-db9c08.netlify.app",
                           "https://cosmic-dusk-10e889.netlify.app",
                           
                           # 26, 27, 28, 29 30
                           "https://soft-donut-e34403.netlify.app",
                           "https://capable-torrone-6eae7f.netlify.app",
                           "https://zingy-tarsier-e2570c.netlify.app",
                           # 29 tlaxcala
                           "https://zippy-bunny-bdd8c7.netlify.app",
                           
                           # 31, 32
                           "https://keen-dango-d1cd7e.netlify.app",
                           "https://mellifluous-banoffee-e28771.netlify.app"))

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
                     col.regions = "#E01A59")
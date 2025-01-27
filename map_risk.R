

# Step 1. load the AGEE ####
mex <- rgeomex::AGEE_inegi19_mx

# Step 3. make the tible link ####
link <- tibble::tibble(CVE_ENT = c("01","02", "03", "04", "05",
                                   
                                   "06", "07", "09", "10",
                                   
                                   "11", "12", "14", "15",
                                   
                                   "16", "17", "18", "19", "20",
                                   
                                   "21", "23", "24", "25",
                                   
                                   "26", "27", "28", "30",
                                   
                                   "31", "32"),
                       link = c( # 01, 02, 03, 04, 05
                           "https://effervescent-phoenix-422bc2.netlify.app",
                           "https://scintillating-daifuku-835184.netlify.app",
                           "https://gleaming-horse-ecd652.netlify.app",
                           "https://fastidious-crumble-b37f6b.netlify.app",
                           "https://fancy-boba-899ee9.netlify.app",
                           
                           # 06, 07, 08, 09, 10
                           "https://effulgent-buttercream-86be70.netlify.app",
                           "https://merry-rabanadas-6871ae.netlify.app",
                           #NA,
                           "https://cheery-macaron-f13158.netlify.app",
                           "https://meek-medovik-f4feae.netlify.app",
                           
                           # 11, 12, 13, 14, 15
                           "https://fantastic-sunburst-343660.netlify.app",
                           "https://loquacious-maamoul-aa9286.netlify.app",
                           #NA,
                           "https://gorgeous-moxie-6e218f.netlify.app",
                           "https://spiffy-jalebi-5a2ecb.netlify.app",
                           
                           # 16, 17, 18, 19, 20
                           "https://fabulous-mermaid-31ea6a.netlify.app",
                           "https://hilarious-zuccutto-7bf80c.netlify.app",
                           "https://lucky-panda-744c1f.netlify.app",
                           "https://dashing-douhua-6a4e76.netlify.app",
                           "https://frabjous-bienenstitch-cc1dbe.netlify.app",
                           
                           # 21, 22, 23, 24, 25
                           "https://spectacular-kringle-a40e04.netlify.app", # puebla
                           #NA,# 22 queteraro
                           "https://dazzling-marshmallow-a952b2.netlify.app", # quintana roo
                           "https://mellifluous-kashata-03f82a.netlify.app",
                           "https://rad-starlight-5b6727.netlify.app",
                           
                           # 26, 27, 28, 29 30
                           "https://curious-yeot-89bc7a.netlify.app",
                           "https://wonderful-tiramisu-d812c8.netlify.app",
                           "https://peppy-valkyrie-33e65e.netlify.app",
                           #NA, # 29 tlaxcala
                           "https://glowing-trifle-5f0837.netlify.app",
                           
                           # 31, 32
                           "https://jocular-croissant-637cbb.netlify.app",
                           "https://peaceful-pegasus-cda4c9.netlify.app"))

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

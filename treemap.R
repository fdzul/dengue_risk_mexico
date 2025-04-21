path_sinave <- "/Users/felipedzul/Library/CloudStorage/OneDrive-Personal/datasets/DGE/denv/2025/DENGUE2_.txt"
x <- data.table::fread(path_sinave,
                       #select = vect_cols2,
                       encoding = "Latin-1",
                       quote="",
                       fill=TRUE)
#extrafont::loadfonts(quiet = TRUE)
densnv::mp_treemap(country = TRUE,
                   year = 2025,
                   snv_dataset = x)


x  |>
    dplyr::filter(ANO == 2025)  |>
    dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                      "OTROS PAISES DE LATINOAMERICA",
                                      "ESTADOS UNIDOS DE NORTEAMERICA"))  |>
    dplyr::filter(DES_DIAG_FINAL %in%
                      c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE",
                        "DENGUE GRAVE"))  |>
    dplyr::group_by(DES_EDO_REP, DES_DIAG_FINAL)  |>
    dplyr::summarise(value = dplyr::n(),
                     .groups = "drop")  |>
    dplyr::mutate(DES_EDO_REP = stringr::str_to_title(DES_EDO_REP),
                  DES_DIAG_FINAL = stringr::str_to_title(DES_DIAG_FINAL))  |>
    dplyr::mutate(DES_DIAG_FINAL = factor(DES_DIAG_FINAL,
                                          levels = c("Dengue Con Signos De Alarma",
                                                     "Dengue Grave",
                                                     "Dengue No Grave"),
                                          labels = c("DSA", "DG", "DNG")))  |>
    ggplot2::ggplot(ggplot2::aes(area = value,
                                 fill = DES_EDO_REP,
                                 subgroup = DES_EDO_REP,
                                 subgroup2 = value,
                                 label = paste(DES_DIAG_FINAL, value, sep = "\n"))) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_subgroup_border(color = "white",
                                             size = 2) +
    treemapify::geom_treemap_subgroup2_border(color = "white",
                                              size = 0.3) +
    treemapify::geom_treemap_text(fontface = "italic",
                                  colour = "black",
                                  place = "bottom",
                                  #ggplot2::aes(color=angle),
                                  alpha = 0.5,
                                  size = 10,
                                  grow = FALSE) +
    treemapify::geom_treemap_subgroup_text(place = "middle",
                                           colour = "White",
                                           #alpha = 0.8,
                                           grow = T) +
    ggplot2::theme(legend.position = "none")  +
    ggplot2::scale_fill_viridis_d()


densnv::mp_treemap(country = FALSE,
                   year = 2025,
                   cve_edo = "14",
                   snv_dataset = x)

densnv::mp_treemap(country = FALSE,
                   year = 2025,
                   cve_edo = "30",
                   snv_dataset = x)

densnv::mp_treemap(country = FALSE,
                   year = 2025,
                   cve_edo = "25",
                   snv_dataset = x)
densnv::mp_treemap(country = FALSE,
                   year = 2025,
                   cve_edo = "28",
                   snv_dataset = x)


path_sinave <- "/Users/felipedzul/Library/CloudStorage/OneDrive-Personal/datasets/DGE/denv/2025/DENGUE2_.txt"
x <- data.table::fread(path_sinave,
                       #select = vect_cols2,
                       encoding = "Latin-1",
                       quote="",
                       fill=TRUE) 

densnv::mp_treemap(country = FALSE,
                   year = 2025,
                   cve_edo = 3,
                   snv_dataset = x)

densnv::mp_treemap(country = FALSE,
                   year = 2025,
                   cve_edo = 24,
                   snv_dataset = x)




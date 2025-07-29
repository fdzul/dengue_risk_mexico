#' Static Bump Map
#'
#' @param dataset is the dengue dataset.
#' @param year is a numerica valur for the year.
#' @param state is a logical value for the state. if state is true the map is by state, else by municipality
#' @param pal_vir is the palleter of viridis.
#' @param cve_edo is 
#' @param axis_start 
#' @param country_text_x 
#' @param country_text_y 
#' @param size_text_value 
#' @param size_text_country 
#' @param line_size 
#' @param value_text 
#'
#' @return
#' @export
#'
#' @examples
static_bump_map <- function(dataset, year, state,  
                            pal_vir = NULL, cve_edo = NULL, 
                            axis_start = NULL,
                            country_text_x,
                            country_text_y, 
                            size_text_value, 
                            size_text_country,
                            line_size,
                            value_text = NULL){
    
    if(state == TRUE){
        df <- dataset |>
            dplyr::filter(ANO == year) |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES", 
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(DES_DIAG_FINAL %in% 
                              c("DENGUE CON SIGNOS DE ALARMA", 
                                "DENGUE NO GRAVE", 
                                "DENGUE GRAVE")) |>
            dplyr::group_by(CVE_EDO_REP) |>
            dplyr::summarise(value = dplyr::n(), .groups = "drop") |>
            dplyr::mutate(CVE_EDO_REP = stringr::str_pad(CVE_EDO_REP,
                                                         side = "left",
                                                         pad = "0",
                                                         width = 2))
        sf_df <- dplyr::left_join(x = rgeomex::AGEE_inegi19_mx,
                                  y = df,
                                  by = c("CVE_ENT" = "CVE_EDO_REP")) |>
            dplyr::filter(!is.na(value)) |>
            dplyr::mutate(NOMGEO = stringr::str_trim(NOMGEO, side = "both")) |>
            sf::st_make_valid()
        
        
        
        ranking <- sf::st_geometry(sf_df) |> 
            sf::st_point_on_surface() |> 
            sf::st_coordinates() |> 
            dplyr::as_tibble() |>
            dplyr::bind_cols(dplyr::tibble(fine_cap = BBmisc::normalize(rank(sf_df$value), 
                                                                        range = c(10, 35), 
                                                                        method = "range"),
                                           country = sf_df$NOMGEO,
                                           xend = -85,
                                           x_axis_start = xend + 5,
                                           fine_cap_x = BBmisc::normalize(sf_df$value, 
                                                                          range = c(dplyr::first(x_axis_start), 100), 
                                                                          method = "range"),
                                           val_txt = paste0(format(sf_df$value, digits = 1, nsmall = 2))))
        sdf <- sf_df |> 
            dplyr::bind_cols(ranking |> dplyr::select(fine_cap))
        
        ggplot2::ggplot() + 
            ggplot2::geom_sf(data = rgeomex::AGEE_inegi19_mx,
                             size = .01, 
                             fill = "gray85", 
                             color = "white") +
            ggplot2::geom_sf(data =  sdf, 
                             size = .01, 
                             #ggplot2::aes(fill = value),
                             ggplot2::aes(fill = fine_cap),
                             color = "white") +
            # Sigmoid from country to start of barchart
            ggbump::geom_sigmoid(data = ranking, 
                                 ggplot2::aes(x = X, 
                                              y = Y, 
                                              xend = x_axis_start , 
                                              yend = fine_cap, 
                                              group = country, 
                                              color = fine_cap), 
                                 alpha = .3, 
                                 smooth = 10, 
                                 linewidth = line_size) +
            # Line from xstart to value
            #ggplot2::geom_segment(data = ranking, 
            #                    ggplot2::aes(x = x_axis_start, 
            #                                  y = fine_cap, 
            #                                  xend = fine_cap_x, 
            #                                  yend = fine_cap, 
            #                                  color = fine_cap), 
            #                    alpha = .6, 
            #                    size = 1, 
            #                    lineend = "round") +
            # dot on centroid of country in map
        ggplot2::geom_point(data = ranking, 
                            ggplot2::aes(x = X, 
                                         y = Y, 
                                         fill =  fine_cap),
                            col = "white",
                            shape = 21,
                            #stroke = 1,
                            size = 2) +
            # Country text
            ggrepel::geom_text_repel(data = ranking, 
                                     ggplot2::aes(x = x_axis_start - country_text_x, 
                                                  y = fine_cap, 
                                                  label = country), ###
                                     hjust = 1, 
                                     color = "gray85",
                                     max.overlaps = getOption("ggrepel.max.overlaps", default = 212),
                                     size = size_text_country, 
                                     nudge_y = country_text_y) +
            # Value text
            ggplot2::geom_text(data = ranking, 
                               ggplot2::aes(x = x_axis_start, 
                                            y = fine_cap, 
                                            label = sf_df$value, 
                                            color = fine_cap),
                               hjust = 0, 
                               size = size_text_value, 
                               nudge_x = 1) +
            ggplot2::scale_fill_viridis_c(option =pal_vir) +
            ggplot2::scale_color_viridis_c(option = pal_vir) +
            ggplot2::theme_void() +
            ggplot2::coord_sf(clip = "off") +
            ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                           legend.position = "none",
                           #plot.background = ggplot2::element_rect(fill = "black"),
                           plot.caption = ggplot2::element_text(color = "gray40"),
                           plot.title = ggplot2::element_text(color = "gray40", 
                                                              size = 16, 
                                                              family = "Helvetica", 
                                                              face = "bold"),
                           plot.subtitle = ggplot2::element_text(color = "gray40", 
                                                                 size = 8))
        
    } else{
        df <- dataset |>
            dplyr::filter(ANO == year) |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES", 
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(DES_DIAG_FINAL %in% 
                              c("DENGUE CON SIGNOS DE ALARMA", "DENGUE NO GRAVE", 
                                "DENGUE GRAVE")) |>
            dplyr::mutate(CVE_EDO_REP = stringr::str_pad(CVE_EDO_REP,
                                                         side = "left",
                                                         pad = "0",
                                                         width = 2),
                          CVE_MPO_REP = stringr::str_pad(CVE_MPO_REP,
                                                         side = "left",
                                                         pad = "0",
                                                         width = 3)) |>
            dplyr::filter(CVE_EDO_REP %in% c(cve_edo)) |>
            dplyr::group_by(CVE_EDO_REP, CVE_MPO_REP) |>
            dplyr::summarise(value = dplyr::n(), .groups = "drop")
        
        sf_df <- dplyr::left_join(x = rgeomex::AGEM_inegi19_mx,
                                  y = df,
                                  by = c("CVE_ENT" = "CVE_EDO_REP",
                                         "CVE_MUN" = "CVE_MPO_REP")) |>
            dplyr::filter(!is.na(value)) |>
            dplyr::mutate(NOMGEO = stringr::str_trim(NOMGEO, side = "both"))
        
        
        x <- rgeomex::AGEE_inegi19_mx |> 
            dplyr::filter(CVE_ENT %in% c(cve_edo))
        
        ranking <- sf::st_geometry(sf_df) |> 
            sf::st_point_on_surface() |> 
            sf::st_coordinates() |> 
            dplyr::as_tibble() |>
            dplyr::bind_cols(dplyr::tibble(fine_cap = BBmisc::normalize(rank(sf_df$value), 
                                                                        range = c(sf::st_bbox(x)[2], 
                                                                                  sf::st_bbox(x)[4]), 
                                                                        method = "range"),
                                           country = sf_df$NOMGEO,
                                           xend = sf::st_bbox(x)[3],
                                           x_axis_start = xend + axis_start,
                                           fine_cap_x = BBmisc::normalize(sf_df$value, 
                                                                          range = c(dplyr::first(x_axis_start), 100), 
                                                                          method = "range"),
                                           val_txt = paste0(format(sf_df$value, digits = 1, 
                                                                   nsmall = 2))))
        sdf <- sf_df |> 
            dplyr::bind_cols(ranking |> 
                                 dplyr::select(fine_cap))
        
        ggplot2::ggplot() + 
            ggplot2::geom_sf(data =  rgeomex::AGEM_inegi19_mx |> 
                                 dplyr::filter(CVE_ENT %in% c(cve_edo)),
                             size = .01, 
                             fill = "gray85", 
                             color = "white") +
            ggplot2::geom_sf(data = sdf, 
                             size = .01, 
                             ggplot2::aes(fill = fine_cap), 
                             color = "white") +
            ggplot2::geom_sf(data = x,
                             size = .01, 
                             fill = "transparent", 
                             color = "black") +
            # Sigmoid from country to start of barchart
            ggbump::geom_sigmoid(data = ranking, 
                                 ggplot2::aes(x = X, 
                                              y = Y, 
                                              xend = x_axis_start , 
                                              yend = fine_cap, 
                                              group = country, 
                                              color = fine_cap),
                                 geom = "line",
                                 alpha = 0.2, 
                                 smooth = 10, 
                                 linewidth = line_size) +
            # Line from xstart to value
            #ggplot2::geom_segment(data = ranking, 
            #                    ggplot2::aes(x = x_axis_start, 
            #                                  y = fine_cap, 
            #                                  xend = fine_cap_x, 
            #                                  yend = fine_cap, 
            #                                  color = fine_cap), 
            #                    alpha = .6, 
            #                    size = 1, 
            #                    lineend = "round") +
            # dot on centroid of country in map
        ggplot2::geom_point(data = ranking, 
                            ggplot2::aes(x = X, 
                                         y = Y, 
                                         color = fine_cap), 
                            #col = "white",
                            shape = 21,
                            stroke = 1,
                            size = 2) +
            # Country text
            #ggrepel::geom_text_repel()
            ggrepel::geom_text_repel(data = ranking, 
                                     ggplot2::aes(x = x_axis_start - country_text_x, 
                                                  y = fine_cap, 
                                                  label = country, 
                                                  color = fine_cap),
                                     hjust = 1, 
                                     size = size_text_country , 
                                     nudge_y =  country_text_y) +
            # Value text
            ggplot2::geom_text(data = ranking, 
                               ggplot2::aes(x = x_axis_start - value_text, 
                                            y = fine_cap, 
                                            label = sf_df$value, 
                                            color = fine_cap),
                               hjust = 0, 
                               size = size_text_value, 
                               nudge_x = .4) +
            ggplot2::scale_fill_viridis_c(option = pal_vir) +
            ggplot2::scale_color_viridis_c(option = pal_vir) +
            ggplot2::theme_void() +
            ggplot2::coord_sf(clip = "off") +
            ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                           legend.position = "none",
                           #plot.background = ggplot2::element_rect(fill = "black"),
                           plot.caption = ggplot2::element_text(color = "gray40"),
                           plot.title = ggplot2::element_text(color = "gray40", 
                                                              size = 16, 
                                                              family = "Helvetica", 
                                                              face = "bold"),
                           plot.subtitle = ggplot2::element_text(color = "gray40", 
                                                                 size = 8))
    }
    
}
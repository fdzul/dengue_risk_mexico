#' Heatmap of dengue cases.
#'
#' @param dataset is the dengue cases dataset.
#' @param year s the target year.
#' @param size_text is the font size of the text.
#' @param state is the target state.
#' @param JS is a logical value, if JS TRUE the heatmap is by state else by Jurisdiction or Municipality.
#'
#' @return
#' @export
#'
#' @examples
heatmap_hospitalizados <- function(dataset,
                                   year,
                                   size_text,
                                   state,
                                   JS){
    
    if(JS == TRUE){
        x <- dataset |>
            dplyr::filter(ANO == year) |>
            dplyr::filter(DES_EDO_RES %in% c(state)) |>
            dplyr::filter(!is.na(FEC_INGRESO)) |>
            dplyr::group_by(SEM, DES_JUR_RES) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            tidyr::pivot_wider(id_cols = c(DES_JUR_RES),
                               names_from = SEM,
                               values_from = n,
                               values_fill = 0) |> 
            tidyr::pivot_longer(cols = -DES_JUR_RES,
                                names_to = "week",
                                values_to = "n") |>
            dplyr::mutate(DES_JUR_RES = stringr::str_to_title(DES_JUR_RES),
                          week = as.numeric(week))
        z <- dataset |>
            dplyr::filter(ANO == year) |>
            dplyr::filter(DES_EDO_RES %in% c(state)) |>
            dplyr::filter(!is.na(FEC_INGRESO)) |>
            dplyr::group_by(DES_JUR_RES) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            dplyr::arrange(dplyr::desc(-n)) |>
            dplyr::mutate(DES_JUR_RES = stringr::str_to_title(DES_JUR_RES))
        
        x$DES_JUR_RES <- factor(x$DES_JUR_RES, levels = z$DES_JUR_RES)
        
        
        plotly::ggplotly(ggplot2::ggplot(data = x, 
                                         ggplot2::aes(y = DES_JUR_RES,
                                                      x = week,
                                                      fill = n,
                                                      label = n)) +
                             ggplot2::geom_tile(alpha = 1)+
                             ggplot2::scale_fill_viridis_c() +
                             ggplot2::geom_text(col = ifelse(x$n >= quantile(x$n, probs = .75), 
                                                             "black","gray"), 
                                                size = size_text, 
                                                alpha = 0.4) +
                             cowplot::theme_cowplot() +
                             ggplot2::ylab("") +
                             ggplot2::xlab("") +
                             ggplot2::labs(fill = "")+
                             ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold", 
                                                                                size = 9),
                                            axis.text.x = ggplot2::element_text(size = 6)) +
                             ggplot2::scale_x_continuous(breaks = c(1:max(unique(x$week)))) +
                             ggplot2::theme(legend.position = "bottom") +
                             ggplot2::theme(legend.key.size = ggplot2::unit(.1, "cm"),
                                            legend.key.width = ggplot2::unit(.5,"cm"),
                                            legend.margin = ggplot2::margin(0,0,0,0),
                                            legend.box.margin = ggplot2::margin(-20,0,0,0)))
        
        
        
        
    } else{
        x <- dataset |>
            dplyr::filter(ANO == year) |>
            dplyr::filter(DES_EDO_RES %in% c(state)) |>
            dplyr::filter(!is.na(FEC_INGRESO)) |>
            dplyr::group_by(DES_MPO_RES, SEM) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            tidyr::pivot_wider(id_cols = c(DES_MPO_RES),
                               names_from = SEM,
                               values_from = n,
                               values_fill = 0) |> 
            tidyr::pivot_longer(cols = -DES_MPO_RES,
                                names_to = "week",
                                values_to = "n") |>
            dplyr::arrange(dplyr::desc(-n)) |>
            dplyr::mutate(mpo = stringr::str_to_title(DES_MPO_RES),
                          week = as.numeric(week))
        
        z <- dataset |>
            dplyr::filter(ANO == year) |>
            dplyr::filter(DES_EDO_RES %in% c(state)) |>
            dplyr::filter(!is.na(FEC_INGRESO)) |>
            dplyr::group_by(DES_MPO_RES) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            dplyr::arrange(dplyr::desc(-n)) |>
            dplyr::mutate(mpo = stringr::str_to_title(DES_MPO_RES))
        
        x$mpo <- factor(x$mpo, levels = z$mpo)
        plotly::ggplotly(ggplot2::ggplot(data = x, 
                                         ggplot2::aes(y = mpo,
                                                      x = week,
                                                      fill = n,
                                                      label = n)) +
                             ggplot2::geom_tile(alpha = 1)+
                             ggplot2::scale_fill_viridis_c() +
                             ggplot2::geom_text(col = ifelse(x$n >= as.integer(quantile(x$n, 
                                                                                        probs = .75)), 
                                                             "black","white"), 
                                                size = size_text, 
                                                alpha = 0.4) +
                             cowplot::theme_cowplot() +
                             ggplot2::ylab("") +
                             ggplot2::xlab("") +
                             ggplot2::labs(fill = "")+
                             ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold", size = 9),
                                            axis.text.x = ggplot2::element_text(size = 6)) +
                             ggplot2::scale_x_continuous(breaks = c(1:max(unique(x$week)))) +
                             ggplot2::theme(legend.position = "bottom") +
                             ggplot2::theme(legend.key.size = unit(.4, "cm"),
                                            legend.key.width = unit(.5,"cm"),
                                            legend.margin = ggplot2::margin(0,0,0,0),
                                            legend.box.margin = ggplot2::margin(-20,0,0,0)))
        
        
    }
    
    
}
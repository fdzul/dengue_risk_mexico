epidemiological_channel <- function(data, year){
    
    # Step 1. filter actual dataset ###
    z <- data |>
        dplyr::filter(!DES_EDO_REP %in% c("OTROS PAISES", 
                                          "OTROS PAISES DE LATINOAMERICA",
                                          "ESTADOS UNIDOS DE NORTEAMERICA")) |>
        dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                            "DENGUE GRAVE",
                                            "DENGUE NO GRAVE",
                                            "FIEBRE HEMORRAGICA POR DENGUE",
                                            "FIEBRE POR DENGUE")) |>
        dplyr::mutate(DES_EDO_REP = stringr::str_replace_all(string = DES_EDO_REP,
                                                             pattern = "  ", 
                                                             replacement = " ")) |>
        # dplyr::filter(ANO %in% c(lubridate::year(Sys.Date())-1,
        #                         lubridate::year(Sys.Date()))) |>
        dplyr::group_by(SEM, ANO, DES_EDO_REP) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(SEM = as.numeric(SEM)) |>
        dplyr::arrange(SEM)
    
    ######
    titleLab <- unique(z[,c("DES_EDO_REP"),])
    nORI <- length(titleLab)
    choiceP1 <- vector("list",nORI)
    for (i in 1:nORI){
        choiceP1[[i]] <- list(method="restyle", 
                              args = list("transforms[0].value", 
                                          unique(z$DES_EDO_REP)[i]), 
                              label= titleLab[i])
    }
    
    #####
    trans1 <- list(list(type ='filter',
                        target = ~DES_EDO_REP, 
                        operation ="=", 
                        value = unique(z$DES_EDO_REP[1])))
    
    
    
    #####
    x <- boldenr::dendata_epichannel |>
        dplyr::rename(DES_EDO_REP = DES_EDO_RES)
    
    titleLab <- unique(x[,c("DES_EDO_REP"),])
    nORI <- length(titleLab)
    choiceP2 <- vector("list",nORI)
    for (i in 1:nORI){
        choiceP2[[i]] <- list(method="restyle", 
                              args = list("transforms[0].value", 
                                          unique(x$DES_EDO_REP)[i]), 
                              label= titleLab[i])
    }
    
    #####
    trans2 <- list(list(type ='filter',
                        target = ~DES_EDO_REP, 
                        operation ="=", 
                        value = unique(x$DES_EDO_REP[1])))
    
    plotly::plot_ly() |>
        plotly::add_trace(data = x, 
                          x = ~SEM, 
                          y = ~q25, 
                          name = "Zona de Éxito", 
                          type = "scatter", 
                          mode = "none", 
                          transforms = trans2,
                          linetype = 2,
                          line = list(color = "white",
                                      width = 1.5),
                          stackgroup = "one",
                          fillcolor = "#50CB86") |>
        plotly::add_trace(data = x, 
                          x = ~SEM, 
                          type = "scatter", 
                          mode = "none", 
                          transforms = trans2,
                          linetype = 2,
                          line = list(color = "white",
                                      width = 1.5),
                          stackgroup = "one",
                          y = ~q50,
                          name = 'Zona de Seguridad', 
                          fillcolor = "#ECB22E") |>
        plotly::add_trace(data = x, 
                          x = ~SEM, 
                          type = "scatter", 
                          mode = "none", 
                          transforms = trans2,
                          linetype = 2,
                          line = list(color = "white",
                                      width = 1.5),
                          stackgroup = "one",
                          y = ~q75,
                          name = 'Zona de Alerta', 
                          fillcolor = "#E01E5A") |>
        plotly::layout(plot_bgcolor = 'rgb(229,229,229)',
                       xaxis = list(title = "Semanas Epidemiológicas",
                                    gridcolor = 'rgb(255,255,255)',
                                    showgrid = TRUE,
                                    showline = FALSE,
                                    showticklabels = TRUE,
                                    tickcolor = 'rgb(127,127,127)',
                                    ticks = 'outside',
                                    zeroline = FALSE),
                       yaxis = list(title = "Número de Casos",
                                    gridcolor = 'rgb(255,255,255)',
                                    showgrid = TRUE,
                                    showline = FALSE,
                                    showticklabels = TRUE,
                                    tickcolor = 'rgb(127,127,127)',
                                    ticks = 'outside',
                                    zeroline = FALSE),
                       legend = list(yanchor = "top",
                                     y = 0.99,
                                     bgcolor = 'rgba(0,0,0,0)',
                                     xanchor = "left",
                                     x = 0.01)) |>
        plotly::add_trace(data = z |>
                              dplyr::filter(ANO %in% c(year)),
                          y = ~n,
                          x = ~SEM, 
                          transforms = trans1,
                          line = list(color = "black", 
                                      width = 4, 
                                      dash = 'dot'),
                          name = paste("<b>", year, sep = ""),
                          mode = "lines") |>
        plotly::layout(xaxis = list(rangeslider = list(visible = T)),
                       updatemenus= list(list(type='dropdown',
                                              active = 1, 
                                              buttons=choiceP2)))
    
}
heatmap_dengue <- function(dataset, state, year, status, hospitalizados){
    
    if(is.null(state) == FALSE){
        
        if(hospitalizados == TRUE){
            # Step 1. extract the dengue cases by municipality ####
            z <- dataset |>
                dplyr::mutate(FEC_INGRESO = lubridate::ymd(FEC_INGRESO)) |>
                dplyr::mutate(hospitalizado = ifelse(is.na(FEC_INGRESO) == TRUE, 
                                                     "No Hospitalizado",
                                                     "Hospitalizado")) |>
                dplyr::filter(ANO == year) |>
                dplyr::filter(DES_EDO_RES %in% c(state)) |>
                dplyr::filter(hospitalizado %in% c("Hospitalizado")) |>
                dplyr::group_by(DES_MPO_RES) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                dplyr::arrange(dplyr::desc(-n)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_replace_all(DES_MPO_RES,
                                                                     pattern = " Quint Roo| Yuc| Ver| Mich| Nvo Leon| Tab",
                                                                     replacement = ""))
            
            
            
            # Step 2. extract the dengue cases ####
            x <- dataset |>
                dplyr::mutate(FEC_INGRESO = lubridate::ymd(FEC_INGRESO)) |>
                dplyr::mutate(hospitalizado = ifelse(is.na(FEC_INGRESO) == TRUE, 
                                                     "No Hospitalizado",
                                                     "Hospitalizado")) |>
                dplyr::filter(ANO == year) |>
                dplyr::filter(DES_EDO_RES %in% c(state)) |>
                dplyr::filter(hospitalizado %in% c("Hospitalizado")) |>
                dplyr::group_by(SEM, DES_MPO_RES, DES_EDO_RES) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                tidyr::pivot_wider(id_cols = dplyr::starts_with("DES"),
                                   names_from = SEM,
                                   values_from = n,
                                   values_fill = 0) |> 
                tidyr::pivot_longer(cols = -dplyr::starts_with("DES"),
                                    names_to = "week") |>
                dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES),
                              DES_EDO_RES = stringr::str_to_title(DES_EDO_RES)) |>
                dplyr::arrange(dplyr::desc(value), dplyr::desc(DES_MPO_RES)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_replace_all(DES_MPO_RES,
                                                                     pattern = " Quint Roo| Yuc| Ver| Mich| Nvo Leon| Tab",
                                                                     replacement = "")) |>
                dplyr::mutate(DES_MPO_RES = factor(DES_MPO_RES, levels = z$DES_MPO_RES)) |>
                dplyr::mutate(week = as.numeric(week))
            
            
            # Step 3. plot the datase
            plotly::plot_ly() |>
                plotly::add_trace(data = x,
                                  x = ~week,
                                  y = ~DES_MPO_RES,
                                  z = ~value,
                                  showlegend = TRUE, 
                                  ids = ~value,
                                  colors = "viridis",
                                  #transforms = ~trans,
                                  type = "heatmap") |>
                plotly::layout(title = list(text = "<b>Casos Hospitalizados</b>",
                                            font = list(color = "#E01E5A",
                                                        size = 14)),
                               xaxis = list(title= "<b>Semanas Epidemiológicas</b>",
                                            color = "#E01E5A",
                                            tick = list(color = "#006AFF"),
                                            tickfont = list(color = "#006AFF")),
                               yaxis = list(title="<b>Municipios</b>",
                                            color = "#E01E5A",
                                            showticklabels = TRUE,
                                            tickfont = list(color = "#006AFF"))) |>
                plotly::layout(xaxis = list(rangeslider = list(visible = T))) |>
                plotly::layout(legend = list(font = list(family = "sans-serif",
                                                         size = 5,
                                                         color = "green")))
        } else {
            # Step 1. extract the dengue cases by municipality ####
            z <- dataset |>
                dplyr::filter(ANO == year) |>
                dplyr::filter(DES_EDO_RES %in% c(state)) |>
                dplyr::filter(ESTATUS_CASO %in% c(status)) |>
                dplyr::group_by(DES_MPO_RES) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                dplyr::arrange(dplyr::desc(-n)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_replace_all(DES_MPO_RES,
                                                                     pattern = " Quint Roo| Yuc| Ver| Mich| Nvo Leon| Tab",
                                                                     replacement = ""))
            
            # Step 2. extract the dengue cases ####
            x <- dataset |>
                dplyr::filter(ANO == year) |>
                dplyr::filter(DES_EDO_RES %in% c(state)) |>
                dplyr::filter(ESTATUS_CASO %in% c(status)) |>
                dplyr::group_by(SEM, DES_MPO_RES, DES_EDO_RES) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                tidyr::pivot_wider(id_cols = dplyr::starts_with("DES"),
                                   names_from = SEM,
                                   values_from = n,
                                   values_fill = 0) |> 
                tidyr::pivot_longer(cols = -dplyr::starts_with("DES"),
                                    names_to = "week") |>
                dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES),
                              DES_EDO_RES = stringr::str_to_title(DES_EDO_RES)) |>
                dplyr::arrange(dplyr::desc(value), dplyr::desc(DES_MPO_RES)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_replace_all(DES_MPO_RES,
                                                                     pattern = " Quint Roo| Yuc| Ver| Mich| Nvo Leon| Tab",
                                                                     replacement = "")) |>
                dplyr::mutate(DES_MPO_RES = factor(DES_MPO_RES, levels = z$DES_MPO_RES)) |>
                dplyr::mutate(week = as.numeric(week))
            
            
            # Step 3. plot the datase
            plotly::plot_ly() |>
                plotly::add_trace(data = x,
                                  x = ~week,
                                  y = ~DES_MPO_RES,
                                  z = ~value,
                                  showlegend = TRUE, 
                                  ids = ~value,
                                  colors = "viridis",
                                  #transforms = ~trans,
                                  type = "heatmap") |>
                plotly::layout(title = list(text = "<b>Casos Confirmados de Dengue</b>",
                                            font = list(color = "#E01E5A",
                                                        size = 14)),
                               xaxis = list(title= "<b>Semanas Epidemiológicas</b>",
                                            color = "#E01E5A",
                                            tick = list(color = "#006AFF"),
                                            tickfont = list(color = "#006AFF")),
                               yaxis = list(title="<b>Municipios</b>",
                                            color = "#E01E5A",
                                            showticklabels = TRUE,
                                            tickfont = list(color = "#006AFF"))) |>
                plotly::layout(xaxis = list(rangeslider = list(visible = T))) |>
                plotly::layout(legend = list(font = list(family = "sans-serif",
                                                         size = 5,
                                                         color = "green")))
            
        } 
    } else{
        
        if(hospitalizados == TRUE){
            # Step 1. extract the dengue cases by municipality ####
            z <- dataset |>
                dplyr::mutate(DES_MPO_RES = paste(DES_MPO_RES, CVE_EDO_RES, sep = "_")) |>
                dplyr::mutate(FEC_INGRESO = lubridate::ymd(FEC_INGRESO)) |>
                dplyr::mutate(hospitalizado = ifelse(is.na(FEC_INGRESO) == TRUE, 
                                                     "No Hospitalizado",
                                                     "Hospitalizado")) |>
                dplyr::filter(ANO == year) |>
                dplyr::filter(hospitalizado %in% c("Hospitalizado")) |>
                dplyr::group_by(DES_MPO_RES) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                dplyr::arrange(dplyr::desc(-n)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_replace_all(DES_MPO_RES,
                                                                     pattern = " Quint Roo| Yuc| Ver| Mich| Nvo Leon| Tab",
                                                                     replacement = ""))
            
            
            
            # Step 2. extract the dengue cases ####
            x <- dataset |>
                dplyr::mutate(DES_MPO_RES = paste(DES_MPO_RES, CVE_EDO_RES, sep = "_")) |>
                dplyr::mutate(FEC_INGRESO = lubridate::ymd(FEC_INGRESO)) |>
                dplyr::mutate(hospitalizado = ifelse(is.na(FEC_INGRESO) == TRUE, 
                                                     "No Hospitalizado",
                                                     "Hospitalizado")) |>
                dplyr::filter(ANO == year) |>
                dplyr::filter(hospitalizado %in% c("Hospitalizado")) |>
                dplyr::group_by(SEM, DES_MPO_RES, DES_EDO_RES) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                tidyr::pivot_wider(id_cols = dplyr::starts_with("DES"),
                                   names_from = SEM,
                                   values_from = n,
                                   values_fill = 0) |> 
                tidyr::pivot_longer(cols = -dplyr::starts_with("DES"),
                                    names_to = "week") |>
                dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES),
                              DES_EDO_RES = stringr::str_to_title(DES_EDO_RES)) |>
                dplyr::arrange(dplyr::desc(value), dplyr::desc(DES_MPO_RES)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_replace_all(DES_MPO_RES,
                                                                     pattern = " Quint Roo| Yuc| Ver| Mich| Nvo Leon| Tab",
                                                                     replacement = "")) |>
                dplyr::mutate(DES_MPO_RES = factor(DES_MPO_RES, levels = z$DES_MPO_RES)) |>
                dplyr::mutate(week = as.numeric(week))
            
            
            # Step 3. plot the datase
            plotly::plot_ly() |>
                plotly::add_trace(data = x,
                                  x = ~week,
                                  y = ~DES_MPO_RES,
                                  z = ~value,
                                  showlegend = TRUE, 
                                  ids = ~value,
                                  colors = "viridis",
                                  #transforms = ~trans,
                                  type = "heatmap") |>
                plotly::layout(title = list(text = "<b>Casos Hospitalizados</b>",
                                            font = list(color = "#E01E5A",
                                                        size = 14)),
                               xaxis = list(title= "<b>Semanas Epidemiológicas</b>",
                                            color = "#E01E5A",
                                            tick = list(color = "#006AFF"),
                                            tickfont = list(color = "#006AFF")),
                               yaxis = list(title="<b>Municipios</b>",
                                            color = "#E01E5A",
                                            showticklabels = TRUE,
                                            tickfont = list(color = "#006AFF"))) |>
                #plotly::layout(xaxis = list(rangeslider = list(visible = T))) |>
                plotly::layout(legend = list(font = list(family = "sans-serif",
                                                         size = 5,
                                                         color = "green")))
        } else {
            # Step 1. extract the dengue cases by municipality ####
            z <- dataset |>
                dplyr::mutate(DES_MPO_RES = paste(DES_MPO_RES, CVE_EDO_RES, sep = "_")) |>
                dplyr::filter(ANO == year) |>
                dplyr::filter(ESTATUS_CASO %in% c(status)) |>
                dplyr::group_by(DES_MPO_RES) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                dplyr::arrange(dplyr::desc(-n)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_replace_all(DES_MPO_RES,
                                                                     pattern = " Quint Roo| Yuc| Ver| Mich| Nvo Leon| Tab",
                                                                     replacement = ""))
            
            # Step 2. extract the dengue cases ####
            x <- dataset |>
                dplyr::mutate(DES_MPO_RES = paste(DES_MPO_RES, CVE_EDO_RES, sep = "_")) |>
                dplyr::filter(ANO == year) |>
                dplyr::filter(ESTATUS_CASO %in% c(status)) |>
                dplyr::group_by(SEM, DES_MPO_RES, DES_EDO_RES) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                tidyr::pivot_wider(id_cols = dplyr::starts_with("DES"),
                                   names_from = SEM,
                                   values_from = n,
                                   values_fill = 0) |> 
                tidyr::pivot_longer(cols = -dplyr::starts_with("DES"),
                                    names_to = "week") |>
                dplyr::mutate(DES_MPO_RES = stringr::str_to_title(DES_MPO_RES),
                              DES_EDO_RES = stringr::str_to_title(DES_EDO_RES)) |>
                dplyr::arrange(dplyr::desc(value), dplyr::desc(DES_MPO_RES)) |>
                dplyr::mutate(DES_MPO_RES = stringr::str_replace_all(DES_MPO_RES,
                                                                     pattern = " Quint Roo| Yuc| Ver| Mich| Nvo Leon| Tab",
                                                                     replacement = "")) |>
                dplyr::mutate(DES_MPO_RES = factor(DES_MPO_RES, levels = z$DES_MPO_RES)) |>
                dplyr::mutate(week = as.numeric(week))
            
            
            # Step 3. plot the datase
            plotly::plot_ly() |>
                plotly::add_trace(data = x,
                                  x = ~week,
                                  y = ~DES_MPO_RES,
                                  z = ~value,
                                  showlegend = TRUE, 
                                  ids = ~value,
                                  colors = "viridis",
                                  #transforms = ~trans,
                                  type = "heatmap") |>
                plotly::layout(title = list(text = "<b>Casos Confirmados de Dengue</b>",
                                            font = list(color = "#E01E5A",
                                                        size = 14)),
                               xaxis = list(title= "<b>Semanas Epidemiológicas</b>",
                                            color = "#E01E5A",
                                            tick = list(color = "#006AFF"),
                                            tickfont = list(color = "#006AFF")),
                               yaxis = list(title="<b>Municipios</b>",
                                            color = "#E01E5A",
                                            showticklabels = TRUE,
                                            tickfont = list(color = "#006AFF"))) |>
                #plotly::layout(xaxis = list(rangeslider = list(visible = T))) |>
                plotly::layout(legend = list(font = list(family = "sans-serif",
                                                         size = 5,
                                                         color = "green")))
            
        } 
        
        
    }
    
    
}
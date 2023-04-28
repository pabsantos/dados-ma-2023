condutores <- read_ods("data/condutores-2014-2022.ods") |> as_tibble()

tab_condutores <- condutores |> 
    mutate(
        Total = `A-Condutor Moto` + `AB-Condutor Moto/Carro` +
            `AC-Condutor Moto/Caminhão` + `AD-Condutor Moto/Ônibus` +
            `AE-Condutor Moto/Carreta` + `B-Condutor Carro` +
            `C-Condutor Caminhão` + `D-Condutor Ônibus` + `E-Condutor Carreta`
    ) |> 
    pivot_longer(-ano, names_to = "tipo", values_to = "quantidade") |> 
    pivot_wider(names_from = ano, values_from = quantidade) |> 
    mutate(variacao_decada = (`2022` - `2014`) / `2014`)


plot_condutores <- 
    ggplot(tab_condutores, aes(x = ano, y = quantidade, fill = tipo)) +
    geom_col() +
    theme_minimal() +
    scale_x_continuous(
        breaks = seq(2014, 2022, 1),
        minor_breaks = NULL
    ) +
    scale_y_continuous(minor_breaks = NULL) +
    scale_fill_manual(
        values = c(
            "#f7951d", "#00496d", "#f05f22", "#737373",
            "#EC0D6B", "#1FA149", "#262626", "#d51f29", "grey90"
        )
    ) +
    labs(
        x = "",
        y = "Condutores habilitados",
        fill = "Tipo"
    )

gt_condutores <- tab_condutores |> 
    gt(rowname_col = "tipo", groupname_col = "Tipo") |> 
    cols_label(
        variacao_decada = "Variação total"
    ) |> 
    fmt_number(
        columns = `2014`:`2022`,
        decimals = 0,
        dec_mark = ",",
        sep_mark = "."
    ) |> 
    fmt_percent(
        columns = variacao_decada,
    ) |> 
    data_color(
        columns = variacao_decada,
        rows = tipo != "Total",
        palette = "BrBG"
    ) |> 
    data_color(
        columns = `2014`:`2022`,
        rows = tipo != "Total",
        palette = "Blues"
    )


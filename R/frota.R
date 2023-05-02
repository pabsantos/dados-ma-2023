frota_names <- list.files("data/", pattern = "^frota")
frota_paths <- paste0("data/", frota_names)

frota_total <- 
    map(frota_paths, read_excel, range = "B3:W4") |> reduce(bind_rows)

frota_ano <- c(2014, 2018, 2015, 2016, 2017, 2019, 2022, 2020, 2021, 2013)

tabela_frota <- frota_total |> 
    clean_names() |> 
    add_column(ano = frota_ano) |> 
    arrange(ano) |> 
    mutate(
        `Automóvel` = automovel + caminhonete + camioneta + utilitario,
        `Motocicleta` = motocicleta + motoneta + quadriciclo + triciclo +
            ciclomotor,
        `Ônibus` = onibus + microonibus,
        `Caminhão` = caminhao + caminhao_trator + chassi_plataforma + reboque +
            semi_reboque,
        `Outros` = bonde + side_car + outros + trator_esteira + trator_rodas
    ) |> 
    select(
        ano, `Automóvel`, `Motocicleta`, `Ônibus`, `Caminhão`, `Outros`, 
        Total = total
    ) |> 
    pivot_longer(-ano, names_to = "tipo", values_to = "n") |> 
    pivot_wider(names_from = ano, values_from = n) |> 
    mutate(var_perc = (`2022` - `2013`) / `2013`)

frota_gt <- tabela_frota |> 
    gt(rowname_col = "ano") |> 
    cols_label(
        ano = "Ano",
        TOTAL = "Frota",
        var_anual = "Variação anual"
    ) |> 
    fmt_number(
        columns = TOTAL,
        sep_mark = ".",
        dec_mark = ",",
        decimals = 0
    )

plot_frota <- tabela_frota |> 
    ggplot(aes(x = ano, y = TOTAL)) +
    geom_col(fill = "#00496D") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(2014, 2023), minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    labs(
        x = "",
        y = "Frota total"
    )


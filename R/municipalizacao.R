municipios_snt <- read_ods("data/municipalizados.ods") |> as_tibble()

tab_snt <- municipios_snt |> 
    mutate(
        var = (municipalizados - lag(municipalizados)) / lag(municipalizados)
    ) |> 
    filter(ano > 2013)

gt_snt <- 
    tab_snt |> 
    gt(rowname_col = "ano") |> 
    fmt_number(
        columns = municipalizados,
        sep_mark = ".",
        dec_mark = ",",
        decimals = 0
    ) |> 
    fmt_percent(
        columns = var,
        sep_mark = ".",
        dec_mark = ","
    ) |> 
    cols_label(
        municipalizados = "Municípios integrados ao SNT",
        var = "Variação anual"
    )
frota_names <- list.files("data/", pattern = "^frota")
frota_paths <- paste0("data/", frota_names)

frota_total <- 
    map(frota_paths, read_excel, range = "B3:B4") |> reduce(bind_rows)

frota_ano <- c(2014, 2018, 2015, 2016, 2017, 2019, 2022, 2020, 2021, 2013)

tabela_frota <- frota_total |> 
    add_column(ano = frota_ano) |> 
    arrange(ano) |> 
    mutate(var_anual = (TOTAL - lag(TOTAL)) / lag(TOTAL)) |> 
    replace_na(list(var_anual = 0)) |>
    mutate(var_anual = scales::percent(var_anual, decimal.mark = ",")) |> 
    select(ano, TOTAL, var_anual) |> 
    filter(ano != 2013)

frota_gt <- tabela_frota |> 
    gt() |> 
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
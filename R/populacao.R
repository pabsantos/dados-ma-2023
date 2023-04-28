pop_tabela <- populacao |> 
    drop_na() |> 
    pivot_longer(-`Unidade da Federação`, names_to = "ano", values_to = "pop") |> 
    group_by(ano) |> 
    summarise(pop = sum(pop)) |> 
    mutate(var = (pop - lag(pop)) / lag(pop)) |> 
    filter(ano > 2013)

pop_gt <- 
    pop_tabela |> 
    gt(rowname_col = "ano") |> 
    fmt_number(
        columns = pop,
        sep_mark = ".",
        dec_mark = ",",
        decimals = 0
    ) |> 
    fmt_percent(
        columns = var,
        dec_mark = ",",
        sep_mark = "."
    ) |> 
    cols_label(
        pop = "População",
        var = "Variação anual",
    )
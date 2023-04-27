load("data/datasus-sim-2020.rda")

make_obito_table <- function() {
    datasus_sim |> 
        mutate(ano = year(data_ocorrencia)) |> 
        filter(ano > 2010) |> 
        count(ano)
}

make_obito_plot <- function() {
    make_obito_table() |> 
        ggplot(aes(x = ano, y = n)) +
        geom_col(fill = "#00496D") +
        geom_text(
            aes(x = ano, label = n),
            nudge_y = -2000,
            size = 3,
            color = "white"
        ) +
        scale_x_continuous(breaks = seq(2011, 2020, 1)) +
        scale_y_continuous(limits = c(0, 50000)) +
        labs(
            x = "",
            y = "Quantidade de Óbitos",
            title = "Quantidade de Óbitos Causados por Sinistros de Trânsito",
            subtitle = "Cenário da última década"
        ) +
        theme_minimal()
}

obitos_decada <- make_obito_table() |> pull(n) |> sum()

populacao <- 
    read_csv2("data/populacao.csv", locale = locale(encoding = "latin1"))

pop_uf <- populacao |> 
    pivot_longer(
        -`Unidade da Federação`,
        names_to = "ano",
        values_to = "pop"
    ) |> 
    separate(
        `Unidade da Federação`,
        into = c("cod_uf", "nome_uf"),
        sep = " ",
        extra = "merge"
    ) |> 
    select(-cod_uf)
    

tabela_indice <- 
    datasus_sim |> 
    mutate(ano = year(data_ocorrencia)) |> 
    filter(ano > 2010) |> 
    count(nome_regiao, nome_uf, ano) |> 
    mutate(ano = as.character(ano)) |> 
    left_join(pop_uf, by = c("nome_uf", "ano")) |> 
    mutate(ind_mortes = n / pop * 100000) |> 
    select(nome_regiao, nome_uf, ano, ind_mortes) |> 
    pivot_wider(names_from = ano, values_from = ind_mortes) |> 
    drop_na()

indice_gt <- tabela_indice |>
    group_by(nome_regiao) |> 
    gt(rowname_col = "nome_uf") |> 
    fmt_number(columns = c(`2011`:`2020`), dec_mark = ",", sep_mark = ".") |> 
    tab_header(
        title = "Índice de Óbitos por 100 mil habitantes em cada Unidade da Federação"
    ) |> 
    data_color(
        columns = c(`2011`:`2020`),
        palette = "Blues"
    )

tabela_modais <- 
    datasus_sim |> 
    mutate(ano = year(data_ocorrencia)) |> 
    filter(ano > 2010) |> 
    count(ano, modal_vitima) |> 
    pivot_wider(names_from = ano, values_from = n) |> 
    mutate(
        total = `2011` + `2012` + `2013` + `2014` + `2015` + `2016` + `2017` +
            `2018` + `2019` + `2020`
    ) |> 
    arrange(modal_vitima)

modais_gt <- tabela_modais |> 
    gt(rowname_col = "modal_vitima") |> 
    cols_label(total = "Total") |> 
    tab_style(
        style = list(
            cell_borders(
                sides = "left",
                color = "grey"
            )
        ),
        locations = list(
            cells_body(
                columns = total
            )
        )
    ) |> 
    data_color(
        columns = c(`2011`:total),
        palette = "Blues"
    ) |> 
    fmt_number(
        columns = c(`2011`:total),
        sep_mark = ".",
        dec_mark = ",",
        decimals = 0
    )

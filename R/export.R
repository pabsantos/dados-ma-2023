library(writexl)

tab_obito <- datasus_sim |> 
    filter(ano_ocorrencia > 2011) |> 
    count(ano_ocorrencia)

tab_idade <- datasus_sim |>
    filter(ano_ocorrencia > 2010) |> 
    count(faixa_etaria_vitima, sexo_vitima) |> 
    drop_na() |> 
    pivot_wider(names_from = sexo_vitima,  values_from = n) |> 
    arrange(desc(faixa_etaria_vitima))

export_list <- list(
    tab_obito, 
    tabela_indice,
    tabela_modais,
    tab_idade,
    tabela_frota,
    tab_condutores,
    pop_tabela,
    tab_snt
)

path_list <- paste0("table/", c(
    "tab_obito.xlsx",
    "tab_indice.xlsx",
    "tab_modais.xlsx",
    "tab_idade.xlsx",
    "tab_frota.xlsx",
    "tab_condutores.xlsx",
    "tab_pop.xlsx",
    "tab_snt.xlsx"
))

map2(export_list, path_list, write_xlsx)

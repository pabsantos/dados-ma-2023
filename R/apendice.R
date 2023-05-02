populacao_municipios <- read_csv("data/pop_mun_ibge.csv")

tab_pop_municipios <- 
    populacao_municipios |> 
    mutate(
        cod_muni = as.character(cod_muni),
        cod_muni = str_sub(cod_muni, 1, 6)
    ) |> 
    separate(Município, sep = " \\(", into = c("municipio", "uf")) |> 
    mutate(
        uf = str_remove(uf, "\\)"),
        regiao = case_match(
            str_sub(cod_muni, 1, 1),
            "1" ~ "Norte",
            "2" ~ "Nordeste",
            "3" ~ "Sudeste",
            "4" ~ "Sul",
            "5" ~ "Centro-Oeste"
        )
    ) |> 
    pivot_longer(`2011`:`2021`, names_to = "ano", values_to = "populacao")

frota_names <- list.files("data/", pattern = "^frota")
frota_paths <- paste0("data/", frota_names)

read_excel(frota_paths)

map(frota_paths, read_excel)

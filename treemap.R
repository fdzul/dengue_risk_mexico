path_sinave <- "/Users/felipedzul/Library/CloudStorage/OneDrive-Personal/datasets/DGE/denv/2025/DENGUE2_.txt"
x <- data.table::fread(path_sinave,
                       #select = vect_cols2,
                       encoding = "Latin-1",
                       quote="",
                       fill=TRUE)

densnv::mp_treemap(country = TRUE,
                   year = 2025,
                   snv_dataset = x)
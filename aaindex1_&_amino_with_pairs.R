# Wczytanie danych
hexapeptide_with_pairs <- read.csv2("data/amino_with_pairs.csv", header = TRUE)
pairs_df <- data.frame(hexapeptide_with_pairs)

# Wczytanie danych z pliku CSV
aaindex1_data <- read.csv2("data/encoding_matrix_aaindex1.csv", header = TRUE)

# Funkcja do wyciągania znaków od 2 do 7 po przecinku z każdego wiersza
get_substring <- function(row) {
    sapply(row, function(x) substr(as.character(x), 2, 7))
}

# Inicjalizacja output_data jako pustej ramki danych
output_data <- data.frame(matrix(nrow = nrow(pairs_df), ncol = 1))
colnames(output_data) <- "encoded_aminoacids"

for (i in 1:nrow(pairs_df)) {
    # Wydobycie pojedynczych aminokwasów z pliku
    row <- pairs_df[i, ]
    data_parts <- strsplit(row, ",")[[1]]
    extracted_data <- data_parts[2:7]

    # Tworzenie ramki danych o 6 kolumnach i 566 wierszach
    encoded_aminoacids_df <- data.frame(matrix(nrow = 566, ncol = 6))
    for (j in 1:6) {
        col_name <- paste("amino", as.character(j), sep = "_")
        colnames(encoded_aminoacids_df)[j] <- col_name
    }

    # Przypisywanie wartości do kolumn w encoded_aminoacids_df
    for (k in 1:6) {
        amino_acid <- extracted_data[k]
        encoded_aminoacids_df[, k] <- aaindex1_data[, amino_acid]
    }

    # Dodawanie danych z encoded_aminoacids_df do output_data
    output_data[i, "encoded_aminoacids"] <- toString(unlist(encoded_aminoacids_df))
}


file_path_save <- "data/data_ready_aaindex1.csv"
write.table(output_data, file = file_path_save, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)

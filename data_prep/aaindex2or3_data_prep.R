library(tidyr)

# Wczytanie danych
aaindex2_file_name <- "data/intermediate_data/extracted_values_aaindex2.csv"
aaindex3_file_name <- "data/intermediate_data/extracted_values_aaindex3.csv"
input_data <- read.csv(aaindex2_file_name, header = FALSE)

# Konwersja danych wejściowych do odpowiedniego formatu
input_data$V21 <- NULL
row_indexes_to_remove <- seq(from = 1, to = 988, by = 21)
input_data <- input_data[-row_indexes_to_remove, ]
rownames(input_data) <- NULL

# Generacja i zmiana nazw kolumn tabeli wyjściowej
output_data <- data.frame(matrix(nrow = 0, ncol = 400))
aminoacid_names <- c("a", "r", "n", "d", "c", "q", "e", "g", "h", "i", "l", "k", "m", "f", "p", "s", "t", "w", "y", "v")
col_names <- c()
for (i in aminoacid_names) {
    for (j in aminoacid_names) {
        col_names <- c(col_names, sprintf("%s%s", j, i))
    }
}
colnames(output_data) <- col_names

# Wypełnienie macierzy kodowania na podstawie danych wejściowych
output_data_row_count <- 0
output_data_col_count <- 0
for (i in 1:nrow(input_data)) {
    if (i %% 20 == 1) {
        output_data_row_count <- output_data_row_count + 1
        output_data_col_count <- 0
    }
    for (j in 1:ncol(input_data)) {
        output_data_col_count <- output_data_col_count + 1
        output_data[output_data_row_count, output_data_col_count] <- input_data[i, j]
    }
}

# Kopiowanie danych dla indeksów z "lustrzanymi odbiciami"
col_names_out_df <- colnames(output_data)
for (i in 1:nrow(output_data)) {
    for (j in 1:ncol(output_data)) {
        if (is.na(output_data[i, j])) {
            name_vec <- unlist(strsplit(col_names_out_df[j], ""))
            col_name_to_search <- sprintf("%s%s", name_vec[2], name_vec[1])
            data_to_copy <- output_data[i, col_name_to_search]
            output_data[i, j] <- data_to_copy
        }
    }
}

# Usunięcie indexów z brakującymi danymi
output_data <- drop_na(output_data)

# Zapis do pliku
aaindex2_out_file_path <- "data/intermediate_data/aaindex2_encoding_matrix.csv"
aaindex3_out_file_path <- "data/intermediate_data/aaindex3_encoding_matrix.csv"
write.table(output_data, file = aaindex2_out_file_path, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
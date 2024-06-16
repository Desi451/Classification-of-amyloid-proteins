# Funkcje wykorzystywane do standaryzacji i normalizacji macierzy kodowania
standarize <- function(data_to_standarize) {
    transposed_data <- as.data.frame(t(data_to_standarize))
    transposed_data[] <- lapply(transposed_data, as.numeric)
    standarized_data <- as.data.frame(scale(transposed_data))
    standarized_data <- as.data.frame(t(standarized_data))
    rownames(standarized_data) <- NULL
    return(standarized_data)
}

min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalize <- function(data_to_normalize) {
    column_names <- colnames(data_to_normalize)
    transposed_data <- as.data.frame(t(data_to_normalize))
    transposed_data[] <- lapply(transposed_data, as.numeric)
    normalized_data <- as.data.frame(lapply(transposed_data, min_max_scaling))
    normalized_data <- as.data.frame(t(normalized_data))
    colnames(normalized_data) <- column_names
    rownames(normalized_data) <- NULL
    return(normalized_data)
}

# Normalizacja i standaryzacja mcierzy kodowania z aaindex1
aaindex1_encoding_matrix <- read.csv2("data/intermediate_data/encoding_matrix_aaindex1.csv")
aaindex1_encoding_matrix_standarized <- standarize(aaindex1_encoding_matrix)
aaindex1_encoding_matrix_normalized <- normalize(aaindex1_encoding_matrix)

# Normalizacja i standaryzacja mcierzy kodowania z aaindex2 i aaindex3
aaindex2_encoding_matrix <- read.csv2("data/intermediate_data/aaindex2_encoding_matrix.csv", check.names = FALSE)
aaindex3_encoding_matrix <- read.csv2("data/intermediate_data/aaindex3_encoding_matrix.csv", check.names = FALSE)
aaindex23_encoding_matrix <- rbind(aaindex2_encoding_matrix, aaindex3_encoding_matrix)
aaindex23_encoding_matrix_standarized <- standarize(aaindex23_encoding_matrix)
aaindex23_encoding_matrix_normalized <- normalize(aaindex23_encoding_matrix)

# Zapis danych do pliku
aaindex1_standarized <- "data/intermediate_data/aaindex1_standarized.csv"
aaindex1_normalized <- "data/intermediate_data/aaindex1_normalized.csv"
aaindex23_standarized <- "data/intermediate_data/aaindex23_standarized.csv"
aaindex23_normalized <- "data/intermediate_data/aaindex23_normalized.csv"
write.table(aaindex1_encoding_matrix_standarized, file = aaindex1_standarized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(aaindex1_encoding_matrix_normalized, file = aaindex1_normalized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(aaindex23_encoding_matrix_standarized, file = aaindex23_standarized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(aaindex23_encoding_matrix_normalized, file = aaindex23_normalized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
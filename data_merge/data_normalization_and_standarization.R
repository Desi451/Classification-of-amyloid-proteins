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
aaindex2_encoding_matrix_standarized <- standarize(aaindex2_encoding_matrix)
aaindex2_encoding_matrix_normalized <- normalize(aaindex2_encoding_matrix)
aaindex3_encoding_matrix <- read.csv2("data/intermediate_data/aaindex3_encoding_matrix.csv", check.names = FALSE)
aaindex3_encoding_matrix_standarized <- standarize(aaindex3_encoding_matrix)
aaindex3_encoding_matrix_normalized <- normalize(aaindex3_encoding_matrix)

# Zapis danych do pliku
aaindex1_standarized <- "data/intermediate_data/aaindex1_standarized.csv"
aaindex1_normalized <- "data/intermediate_data/aaindex1_normalized.csv"
aaindex2_standarized <- "data/intermediate_data/aaindex2_standarized.csv"
aaindex2_normalized <- "data/intermediate_data/aaindex2_normalized.csv"
aaindex3_standarized <- "data/intermediate_data/aaindex3_standarized.csv"
aaindex3_normalized <- "data/intermediate_data/aaindex3_normalized.csv"
write.table(aaindex1_encoding_matrix_standarized, file = aaindex1_standarized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(aaindex1_encoding_matrix_normalized, file = aaindex1_normalized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(aaindex2_encoding_matrix_standarized, file = aaindex2_standarized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(aaindex2_encoding_matrix_normalized, file = aaindex2_normalized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(aaindex3_encoding_matrix_standarized, file = aaindex3_standarized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(aaindex3_encoding_matrix_normalized, file = aaindex3_normalized, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
# Wczytanie danych do zakodowania
data_to_encode <- read.csv2("data/intermediate_data/amino_with_pairs.csv")

# Wybór opcji kodowania
standarized_encoding <- FALSE
normalized_encoding <- TRUE

# Co zakodować
encode_aminoacids <- TRUE
encode_pairs <- FALSE

if (standarized_encoding) {
    # Wczytanie ustandaryzowanych macierzy kodowania
    aaindex1_encoding_matrix <- read.csv2("data/intermediate_data/aaindex1_standarized.csv", check.names = FALSE)
    aaindex23_encoding_matrix <- read.csv2("data/intermediate_data/aaindex23_standarized.csv", check.names = FALSE)
} else if (normalized_encoding) {
    # Wczytanie znormalizowanych macierzy kodowania
    aaindex1_encoding_matrix <- read.csv2("data/intermediate_data/aaindex1_normalized.csv", check.names = FALSE)
    aaindex23_encoding_matrix <- read.csv2("data/intermediate_data/aaindex23_normalized.csv", check.names = FALSE)
} else {
    # Wczytanie macierzy kodowania 
    aaindex1_encoding_matrix <- read.csv2("data/intermediate_data/encoding_matrix_aaindex1.csv", check.names = FALSE)
    aaindex2_encoding_matrix <- read.csv2("data/intermediate_data/aaindex2_encoding_matrix.csv", check.names = FALSE)
    aaindex3_encoding_matrix <- read.csv2("data/intermediate_data/aaindex3_encoding_matrix.csv", check.names = FALSE)
    aaindex23_encoding_matrix <- rbind(aaindex2_encoding_matrix, aaindex3_encoding_matrix)
}

# Inicializacja zakodowanej macierzy aminokwasów
coding_vector_length <- nrow(aaindex1_encoding_matrix)
number_of_aminoacids <- 6
encoded_aminoacids_df <- data.frame(matrix(nrow = 0, ncol = number_of_aminoacids * coding_vector_length))
for (i in 1:number_of_aminoacids) {
    for (j in 1:coding_vector_length) {
        col_name <- paste("a", as.character(i), "_", as.character(j), sep = "")
        colnames(encoded_aminoacids_df)[(i - 1) * coding_vector_length + j] <- col_name
    }
}
rm(i, j, col_name, number_of_aminoacids)

# Inicializacja zakodowanej macierzy par aminokwasów
coding_vector_pairs_length <- nrow(aaindex23_encoding_matrix)
number_of_aminoacids_pairs <- 5
encoded_aminoacids_pairs_df <- data.frame(matrix(nrow = 0, ncol = number_of_aminoacids_pairs * coding_vector_pairs_length))
for (i in 1:number_of_aminoacids_pairs) {
    for (j in 1:coding_vector_pairs_length) {
        col_name <- paste("p", as.character(i), "_", as.character(j), sep = "")
        colnames(encoded_aminoacids_pairs_df)[(i - 1) * coding_vector_pairs_length + j] <- col_name
    }
}
rm(i, j, col_name, number_of_aminoacids_pairs)

# Wypełnianie zakodowanej macierzy aminokwasów
if (encode_aminoacids) {
    aminoacids_separated <- data_to_encode[, 2:7]
    for (i in seq_len(nrow(aminoacids_separated))) {
        print(i)
        for (j in seq_len(ncol(aminoacids_separated))) {
            beginning_of_insertion <- j * coding_vector_length - coding_vector_length + 1
            end_of_insertion <- j * coding_vector_length
            aminoacid_to_encode <- aminoacids_separated[i, j]
            encoded_aminoacids_df[i, beginning_of_insertion:end_of_insertion] <- unlist(aaindex1_encoding_matrix[ , aminoacid_to_encode])
        }
    }
    rm(i, j, beginning_of_insertion, end_of_insertion)
}

# Wypełnianie zakodowanej macierzy par aminokwasów
if (encode_pairs) {
    pairs_separated <- data_to_encode[, 8:12]
    for (i in seq_len(nrow(pairs_separated))) {
        for (j in seq_len(ncol(pairs_separated))) {
            beginning_of_insertion <- j * coding_vector_pairs_length - coding_vector_pairs_length + 1
            end_of_insertion <- j * coding_vector_pairs_length
            pair_to_encode <- pairs_separated[i, j]
            encoded_aminoacids_pairs_df[i, beginning_of_insertion:end_of_insertion] <- unlist(aaindex23_encoding_matrix[ , pair_to_encode])
        }
    }
    rm(i, j, beginning_of_insertion, end_of_insertion)
}

# Łączenie wyników kodowań
out_data <- data.frame(Classification = data_to_encode[, 1])
if (encode_aminoacids) {
    out_data <- cbind(out_data, encoded_aminoacids_df)
}
if (encode_pairs) {
    out_data <- cbind(out_data, encoded_aminoacids_pairs_df)
}

# Zapis do pliku
file_path <- "data/final_data/"

if (encode_aminoacids && encode_pairs) {
    file_path <- paste(file_path, "aaindex123_data_encoded", sep = "")
} else if (encode_aminoacids) {
    file_path <- paste(file_path, "aaindex1_data_encoded", sep = "")
} else if (encode_pairs) {
    file_path <- paste(file_path, "aaindex23_data_encoded", sep = "")
}

if (standarized_encoding) {
    file_path <- paste(file_path, "_standarized.csv", sep = "")
} else if (normalized_encoding) {
    file_path <- paste(file_path, "_normalized.csv", sep = "")
} else {
    file_path <- paste(file_path, ".csv", sep = "")
}
write.table(out_data, file = file_path, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
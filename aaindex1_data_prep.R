file_path_read <- "data/aaindex1"

lines <- readLines(file_path_read)

encoding_matrix <- data.frame(matrix(nrow = 0, ncol = 20))
colnames(encoding_matrix) <- c(
    "A", "R", "N", "D", "C", "Q", "E", "G", "H", "I",
    "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")

lines_length <- length(lines)
for (line_number in 1:lines_length) {
    string_to_check <- lines[line_number]
    if (string_to_check == "//") {
        word_list_1 <- strsplit(lines[line_number - 2], "\\s+")
        word_list_2 <- strsplit(lines[line_number - 1], "\\s+")

        number_vector_1 <- as.double(unlist(word_list_1))
        number_vector_2 <- as.double(unlist(word_list_2))
        number_vector_1 <- number_vector_1[-1]
        number_vector_2 <- number_vector_2[-1]
        combined_number_vector <- c(number_vector_1, number_vector_2)

        row_number_to_insert <- nrow(encoding_matrix) + 1
        encoding_matrix[row_number_to_insert, ] <- combined_number_vector
    }
}

file_path_save <- "data/encoding_matrix_aaindex1.csv"
write.table(encoding_matrix, file = file_path_save, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
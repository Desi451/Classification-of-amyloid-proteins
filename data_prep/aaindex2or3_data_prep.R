input_data <- read.csv("data/intermediate_data/extracted_values_aaindex2.csv", header = FALSE)
input_data$V21 <- NULL
for (i in 1:nrow(input_data)) {
    if (input_data[i, 1] == "X1" || is.na(input_data[i, 1])) {
        input_data <- input_data[-i, ]
    }
}
rownames(input_data) <- NULL

output_data <- data.frame(matrix(nrow = 0, ncol = 400))
aminoacid_names <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
col_names <- c()
for (i in aminoacid_names) {
    for (j in aminoacid_names) {
        col_names <- c(col_names, sprintf("%s%s", j, i))
    }
}
colnames(output_data) <- col_names

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

col_names_out_df <- colnames(output_data)
for (i in 1:nrow(output_data)) {
    for (j in 1:ncol(output_data)) {
        if (is.na(output_data[i, j])) {
            name_vec <- unlist(strsplit(col_names_out_df[j], ""))
            col_name_to_search <- sprintf("%s%s", name_vec[2], name_vec[1])
            output_data[i, j] <- output_data[i, col_name_to_search]
        }
    }
}
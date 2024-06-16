# Reading data

hexapeptide_data <- read.csv2("data/raw_data/waltzdb_export(csv).csv")
hexapeptide_df <- data.frame(hexapeptide_data)

# Removing hexapeptides with missing amino acids data

rows_to_remove <- c()
for (i in seq_len(nrow(hexapeptide_df))) {
    if (is.na(hexapeptide_df[i, "Sequence"]) || nchar(hexapeptide_df[i, "Sequence"]) != 6) {
        rows_to_remove <- c(rows_to_remove, i)
    }
}
hexapeptide_df <- hexapeptide_df[-rows_to_remove, ]
rownames(hexapeptide_df) <- NULL
rm(rows_to_remove, i)

# Separating aminoacids sequence data to separate columns

sequence_column <- hexapeptide_df[, "Sequence", drop = FALSE]
hexapeptide_separated_df <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(hexapeptide_separated_df) <- c(
    "Amino_1",
    "Amino_2",
    "Amino_3",
    "Amino_4",
    "Amino_5",
    "Amino_6"
)

for (i in seq_len(nrow(sequence_column))) {
    aminoacid_vector <- unlist(strsplit(sequence_column[i, "Sequence"], ""))
    hexapeptide_separated_df[i, ] <- unlist(aminoacid_vector)
}
rm(i, aminoacid_vector)

# Creating encoding matrix of aminoacids

unique_aminoacids <- unique(unlist(hexapeptide_separated_df))
encoded_aminoacids_matrix <- model.matrix(~ unique_aminoacids - 1)
colnames(encoded_aminoacids_matrix) <- unique_aminoacids

# Initializing encoded aminoacids sequence data frame

encoded_aminoacids_df <- data.frame(matrix(nrow = 0, ncol = 120))
for (i in 1:6) {
    for (j in 1:20) {
        col_name <- paste(as.character(i), ":", as.character(j), sep = "")
        colnames(encoded_aminoacids_df)[(i - 1) * 20 + j] <- col_name
    }
}
rm(i, j, col_name)

# Filling encoded aminoacids sequence data frame

for (i in seq_len(nrow(hexapeptide_separated_df))) {
    for (j in seq_len(ncol(hexapeptide_separated_df))) {
        beginning_of_insertion <- j * 20 - 19
        end_of_insertion <- j * 20
        encoded_aminoacids_df[i, beginning_of_insertion:end_of_insertion] <- unlist(encoded_aminoacids_matrix[ , hexapeptide_separated_df[i, j]])
    }
}
rm(i, j, beginning_of_insertion, end_of_insertion)

# Merging encoded data with normal data

encoded_hexapeptide_df <- hexapeptide_df[, 1:2, drop = FALSE]
encoded_hexapeptide_df <- cbind(encoded_hexapeptide_df, encoded_aminoacids_df)

# Saving data

file_path <- "data/final_data/encoded_hexapeptide_data.csv"
write.table(encoded_hexapeptide_df, file = file_path, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
rm(file_path)
# Reading data
hexapeptide_data <- read.csv2("data/waltzdb_export(csv).csv")
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
hexapeptide_with_pairs_df <- data.frame(matrix(nrow = nrow(hexapeptide_df), ncol = 12))
colnames(hexapeptide_with_pairs_df) <- c(
    "Classification", "a_1", "a_2", "a_3", "a_4", "a_5", "a_6",
    "aa_1", "aa_2", "aa_3", "aa_4", "aa_5"
)

# Filling the data frame with single aminoacids and pairs of aminoacids
for (i in seq_len(nrow(hexapeptide_df))) {
    # Adding Classification
    hexapeptide_with_pairs_df[i, "Classification"] <- hexapeptide_df[i, "Classification"]

    sequence <- as.character(hexapeptide_df[i, "Sequence"])
    aminoacid_vector <- strsplit(sequence, "")[[1]]
    
    # Adding individual aminoacids
    hexapeptide_with_pairs_df[i, 2:7] <- aminoacid_vector[1:6]
    
    # Adding pairs of aminoacids
    for (j in 1:5) {
        hexapeptide_with_pairs_df[i, j+7] <- paste0(aminoacid_vector[j], aminoacid_vector[j+1])
    }
}

file_path <- "data/amino_with_pairs.csv"
write.csv(hexapeptide_with_pairs_df, file = file_path, row.names = FALSE, col.names = TRUE, quote = FALSE)
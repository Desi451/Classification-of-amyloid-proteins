input_data <- read.csv("data/intermediate_data/extracted_values_aaindex2.csv", header = FALSE)

output_data <- array(list(), dim = c(20, 20), dimnames = list(NULL, NULL))

row <- 1
col <- 1
counter <- 0

for (i in 2:nrow(input_data)) {
    for (j in 1:ncol(input_data)) {
        value <- input_data[i, j]

        if (is.na(value)) {
            counter <- counter + 1

            row <- row + 1

            if (row == 22) {
                row <- 1
                col <- col + 1
            }

            if (counter == 22) {
                row <- 1
                col <- 1
            }
        } else {
            if (row < 21 && col < 21) {
                if (length(output_data[[col, row]]) == 0) {
                    output_data[[col, row]] <- c(value)
                    counter <- 0
                } else {
                    output_data[[col, row]] <- c(output_data[[col, row]], value)
                    counter <- 0
                }
            }

            row <- row + 1

            if (row == 22) {
                row <- 1
                col <- col + 1
            }
        }
    }
}

write.table(output_data, file = "data/intermediate_data/output_data_aaindex2.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)

# # alternatywa

# # Otwieranie pliku do zapisu
# file <- file("output_data.csv", "w")

# # Iteracja przez każdy element listy i zapisywanie go jako osobny wiersz
# for (i in 1:20) {
#  for (j in 1:20) {
#    if (length(output_data[[i, j]]) > 0) {
#      writeLines(paste(output_data[[i, j]], collapse = ","), file)
#    }
#  }
# }

# # Zamykanie pliku
# close(file)

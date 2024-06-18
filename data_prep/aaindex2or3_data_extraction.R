# Funkcja do przetwarzania sekcji
process_section <- function(section) {
    # Inicjalizacja pustej ramki danych
    df <- data.frame(stringsAsFactors = FALSE)

    # Usuń nagłówki
    section <- section[-1]

    # Ustal długość najdłuższego wiersza
    max_length <- max(sapply(strsplit(section, "\\s+"), length))

    # Przetwarzanie każdego wiersza
    for (line in section) {
        # Usuń dodatkowe białe znaki
        line <- trimws(line)

        # Podziel wiersz na wartości
        values <- as.numeric(strsplit(line, "\\s+")[[1]])

        # Uzupełnij brakujące wartości NA
        values <- c(values, rep(NA, max_length - length(values)))

        # Utwórz ramkę danych dla pojedynczego wiersza
        row_df <- data.frame(t(values), stringsAsFactors = FALSE)

        # Dołącz ramkę danych do wynikowej ramki
        df <- rbind(df, row_df)
    }

    return(df)
}


file_paths <- c("data/raw_data/aaindex2.txt", "data/raw_data/aaindex3")

# Funkcja do odczytywania i przetwarzania pliku
process_file <- function(file_path, output_csv_path) {
    # Odczytanie pliku linia po linii
    lines <- readLines(file_path)

    # Inicjalizacja zmiennej do przechowywania flagi, czy jesteśmy wewnątrz interesującego nas fragmentu
    inside_section <- FALSE

    # Inicjalizacja listy do przechowywania wyodrębnionych fragmentów
    extracted_sections <- list()
    current_section <- c()

    # Przetwarzanie każdej linii osobno
    for (line in lines) {
        if (grepl("^M rows = ARNDCQEGHILKMFPSTWYV, cols = ARNDCQEGHILKMFPSTWYV", line)) {
            inside_section <- TRUE
        }

        if (inside_section) {
            current_section <- c(current_section, line)
            if (grepl("^//", line)) {
                inside_section <- FALSE
                extracted_sections <- append(extracted_sections, list(current_section))
                current_section <- c()
            }
        }
    }

    # Przetwarzanie wszystkich sekcji
    all_data <- do.call(rbind, lapply(extracted_sections, process_section))

    # Zapisanie do pliku CSV
    write.csv(all_data, file = output_csv_path, row.names = FALSE, col.names = FALSE)
}

output_csv_paths <- c("data/intermediate_data/extracted_values_aaindex2.csv", "data/intermediate_data/extracted_values_aaindex3.csv")

# Przetwarzanie plików
for (i in seq_along(file_paths)) {
    process_file(file_paths[i], output_csv_paths[i])
}

# Ścieżka do pliku tekstowego
file_path <- "data/aaindex3"

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

# Przetwarzanie sekcji
process_section <- function(section) {
    # Inicjalizacja pustej ramki danych
    df <- data.frame(stringsAsFactors = FALSE)

    # Usuń nagłówki
    section <- section[2:length(section)]

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

# Przetwarzanie wszystkich sekcji
all_data <- do.call(rbind, lapply(extracted_sections, process_section))

# Ścieżka do pliku CSV
output_csv_path <- "data/extracted_values_aaindex3.csv"

# Zapisanie do pliku CSV
write.csv(all_data, file = output_csv_path, row.names = FALSE)

cat("Wyodrębnione dane zostały zapisane do pliku:", output_csv_path)

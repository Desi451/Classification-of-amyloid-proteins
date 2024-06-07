# Stworzenie pustej tablicy dwuwymiarowej o wymiarach 20x20, gdzie każdy element jest pustą tablicą jednowymiarową o nieznanej długości
output_data <- array(list(), dim = c(21, 21), dimnames = list(NULL, NULL))

# Dane wejściowe
input_data <- c(
    -2.6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -3.4, -4.3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -3.1, -4.1, -3.2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -2.8, -3.9, -3.1, -2.7, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -4.2, -5.3, -4.9, -4.2, -7.1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -3.5, -4.5, -3.8, -3.2, -5, -3.4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -3, -4.2, -3.4, -3.3, -4.4, -3.6, -2.8, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -3.8, -4.5, -4, -3.7, -5.4, -4.4, -3.8, -3.9, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -4, -4.9, -4.4, -4.3, -5.6, -4.7, -4.5, -4.7, -4.9, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -5.9, -6.2, -5.8, -5.4, -7.3, -5.9, -5.7, -6.3, -6.6, -8.2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -4.8, -5.1, -4.6, -4.3, -6.2, -5, -4.6, -5.2, -5.6, -7.5, -6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -3.1, -3.6, -3.3, -3.2, -4.4, -3.7, -3.8, -3.8, -4.1, -5.6, -4.6, -2.7, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    -4.6, -5, -4.2, -4.3, -6.2, -3.5, -4.6, -5.1, -5.4, -7.4, -6.3, -4.7, -5.8, NA, NA, NA, NA, NA, NA, NA, NA,
    -5.1, -5.8, -5, -4.9, -6.8, -5.3, -5, -5.6, -6.4, -8, -7, -4.9, -6.6, -7.1, NA, NA, NA, NA, NA, NA, NA,
    -3.4, -4.2, -3.6, -3.3, -5.3, -4, -3.5, -4.2, -4.5, -6, -4.8, -3.6, -5.1, -5.2, -3.5, NA, NA, NA, NA, NA, NA,
    -2.9, -3.8, -3.1, -2.7, -4.6, -3.6, -3.2, -3.8, -4.3, -5.5, -4.4, -3, -4.1, -4.7, -3.4, -2.5, NA, NA, NA, NA, NA,
    -3.3, -4, -3.5, -3.1, -4.8, -3.7, -3.3, -4.1, -4.5, -5.9, -4.8, -3.3, -4.6, -5.1, -3.6, -3.3, -3.1, NA, NA, NA, NA,
    -5.2, -5.8, -5.3, -5.1, -6.9, -5.8, -5.2, -5.8, -6.5, -7.8, -6.8, -5, -6.9, -7.4, -5.6, -5, -5.1, -6.8, NA, NA, NA,
    -4.7, -5.6, -5, -4.7, -6.6, -5.2, -4.9, -5.4, -6.1, -7.4, -6.2, -4.9, -6.1, -6.6, -5.2, -4.7, -4.9, -6.8, -6, NA, NA,
    -4.3, -4.9, -4.3, -4, -6, -4.7, -4.2, -5.1, -5.3, -7.3, -6.2, -4.2, -6, -6.5, -4.7, -4.2, -4.4, -6.5, -5.9, -5.5, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    36, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    20, 14, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    39, 34, 22, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    36, 35, 31, 24, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    15, 13, 21, 10, 48, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    34, 29, 30, 17, 13, 7, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    27, 34, 27, 38, 9, 19, 9, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    115, 58, 80, 83, 53, 75, 54, 75, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    25, 17, 25, 32, 11, 17, 24, 40, 8, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    119, 33, 51, 43, 33, 29, 35, 109, 27, 81, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    174, 47, 63, 60, 49, 53, 53, 155, 49, 230, 167, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    55, 20, 39, 54, 14, 32, 70, 92, 21, 53, 86, 20, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    26, 9, 7, 13, 11, 1, 12, 32, 7, 44, 58, 21, 6, NA, NA, NA, NA, NA, NA, NA, NA,
    62, 32, 27, 32, 32, 21, 22, 67, 38, 103, 175, 32, 20, 53, NA, NA, NA, NA, NA, NA, NA,
    33, 20, 22, 23, 24, 23, 17, 62, 15, 41, 46, 34, 16, 19, 11, NA, NA, NA, NA, NA, NA,
    64, 44, 42, 37, 28, 44, 41, 145, 43, 65, 93, 51, 14, 36, 38, 34, NA, NA, NA, NA, NA,
    70, 36, 46, 38, 22, 28, 28, 116, 36, 67, 110, 50, 16, 39, 32, 69, 29, NA, NA, NA, NA,
    38, 15, 21, 24, 17, 22, 15, 52, 23, 40, 68, 20, 17, 41, 19, 31, 20, 7, NA, NA, NA,
    65, 45, 50, 52, 43, 32, 40, 103, 44, 81, 105, 64, 19, 45, 42, 71, 53, 32, 33, NA, NA,
    101, 44, 50, 53, 53, 46, 39, 183, 42, 216, 306, 67, 52, 114, 58, 106, 79, 62, 85, 152, NA
)

# Dodatkowy debug:
row <- 1
col <- 1
counter <- 0

for (i in 1:length(input_data)) {
    value <- input_data[i]

    # Sprawdź, czy wartość to NA
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

print(output_data)

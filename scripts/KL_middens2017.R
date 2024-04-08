# Connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

# Read in census table and filter for KL 2017
census <- collect(tbl(con, "census"))
class(census)
census <- as.data.table(census)

KLcensus <- census[gr == "KL"]
KLcensus <- KLcensus[census_date == "2017-05-15" | census_date == "2017-08-15"]

class(KLcensus$reflo)

KLmiddens2017 <- KLcensus %>%
  select(reflo, locx, locy) %>%
  distinct() %>%
  na.omit()

fwrite(KLmiddens2017, "Output/KLmiddens2017.csv")

# Function to convert letters to numbers
letter_to_number <- function(x) {
  # Define a vector of letters
  letters <- LETTERS
  # Replace letters with their numerical representation
  result <- sapply(x, function(y) {
    if (grepl("[A-Z]", y)) {
      # Split the string by decimal point
      parts <- unlist(strsplit(y, "\\."))
      # Convert the first part (before the decimal) from letter to number
      letter <- parts[1]
      number <- which(letters == letter)
      # Combine with the second part (after the decimal) if it exists
      if (length(parts) > 1) {
        result <- paste0(number, ".", parts[2])
      } else {
        result <- as.character(number)
      }
    } else {
      result <- y
    }
    result
  })
  result
}
# Convert letters to numbers in locx
KLmiddens2017$locx <- as.numeric(letter_to_number(as.character(KLmiddens2017$locx)))

class(KLmiddens2017$locx)
class(KLmiddens2017$locy)

KLmiddens2017$locy <- as.numeric(KLmiddens2017$locy)





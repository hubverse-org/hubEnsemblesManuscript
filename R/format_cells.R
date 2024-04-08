# Function adapted from answered question on Stack Overflow
# Apply table formatting with a function
# Source: https://stackoverflow.com/a/49656650
format_cells <- function(df, rows, cols,
                         value = c("italics", "bold", "strikethrough")) {
  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]

  for (r in rows) {
    for (c in cols) {
      # Make sure values are not factors
      df[[c]] <- as.character(df[[c]])

      # Update formatting
      df[r, c] <- paste0(markup, df[r, c], markup)
    }
  }

  return(df)
}

#' Apply formatting to cells in a data frame
#'
#' Function adapted from answered question on Stack Overflow.
#' Source: https://stackoverflow.com/a/49656650
#'
#' @param df a data.frame
#' @param rows rows to format
#' @param cols columns to format
#' @param value type of formatting to apply
#'
#' @export
format_cells <- function(df, rows, cols,
                         value = c("italics", "bold", "strikethrough", "code")) {
  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("*", "**", "~~", "`"), c("italics", "bold", "strikethrough", "code"))
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

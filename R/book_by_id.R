# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve a Recommended Book by ID
#'
#' Fetches detailed information about a specific recommended book from the
#' `adem.books` table based on its unique book ID.
#'
#' @param book_id A single integer representing the unique ID of the book
#'   to retrieve. Must be a single, non-missing, integer value.
#'
#' @return A `data.frame` containing one row with columns `book_id`, `title`,
#'   `author`, and `skill_id` if the book is found. Returns an empty (0-row)
#'   `data.frame` if no book matches the provided `book_id`.
#'   Stops with an error if the database query fails or if the input
#'   `book_id` is invalid.
#'   *(Note: Unlike other `_by_id` functions returning NULL, this follows the
#'   original spec of returning a 0-row data.frame if not found.)*
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get details for book with ID 101 (replace with a valid ID)
#' book_details <- get_book_by_id(101)
#' if (nrow(book_details) > 0) {
#'   print(book_details)
#' } else {
#'   print("Book with ID 101 not found.")
#' }
#'
#' # Try an ID that likely doesn't exist
#' non_existent <- get_book_by_id(99999)
#' print(non_existent) # Should print a 0-row data frame
#' print(nrow(non_existent)) # Should print 0
#'
#' # Examples of invalid input (will cause errors)
#' try(get_book_by_id("abc"))
#' try(get_book_by_id(c(1, 2)))
#' try(get_book_by_id(10.5))
#' }
#'
#' @seealso [get_books()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id
#' @importFrom glue glue glue_sql
#' @export
get_book_by_id <- function(book_id) {
  if (!is.numeric(book_id) || length(book_id) != 1) stop("Invalid book_id")
  sql <- "SELECT book_id, title, author, skill_id FROM adem.books WHERE book_id = $1;"

}


get_book_by_id(101)

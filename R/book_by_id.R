# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve a Book by ID
#'
#' Fetches details for a specific book from `adem.book_recommendations` based on its book ID.
#' ## NOTE: Verify 'adem.book_recommendations' is the correct table name. ##
#' ##       If it should be 'adem.books', update the code below.         ##
#'
#' @param book_id Integer. The unique ID of the book.
#'   Must be a single, non-missing, positive integer value.
#'
#' @return A `data.frame` containing one row with `book_id`, `title`, `author`,
#'   `skill_id` if the book is found. Returns an empty (0-row) `data.frame`
#'   if the `book_id` is not found. Stops with an error if the query fails
#'   or `book_id` is invalid.
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get book 101
#' book_101 <- get_book_by_id(101)
#' if (nrow(book_101) > 0) {
#'   print(book_101)
#'   print(book_101$title) # Access columns directly
#' } else {
#'   print("Book 101 not found.")
#' }
#'
#' # Non-existent book
#' fake_book <- get_book_by_id(99999)
#' print(paste("Rows found for book 99999:", nrow(fake_book))) # Should be 0
#'
#' # Invalid input
#' try(get_book_by_id("abc"))
#' try(get_book_by_id(NA))
#' try(get_book_by_id(c(1, 2)))
#' try(get_book_by_id(10.5))
#' try(get_book_by_id(0)) # Check for positive
#' }
#'
#' @seealso [connect_db()] # Add get_books() if it exists
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id dbIsValid
#' @importFrom glue glue glue_sql
#' @export
get_book_by_id <- function(book_id) {
  # --- Input Validation ---
  if (
    !is.numeric(book_id) ||
    length(book_id) != 1 ||
    is.na(book_id) ||
    book_id %% 1 != 0 ||
    book_id <= 0 # Check for positive integer
  ) {
    stop("'book_id' must be a single, non-missing, positive integer value.", call. = FALSE)
  }
  book_id <- as.integer(book_id) # Ensure integer type

  # Initialize connection
  con <- NULL

  tryCatch({
    # Establish connection
    con <- connect_db()
    # Ensure robust disconnection
    on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

    # --- Prepare SQL Query ---
    # !! VERIFY TABLE NAME HERE !! ('books' or 'book_recommendation')
    book_table <- DBI::Id(schema = 'adem', table = 'book_recommendations')
    # --- Use Parameterized Query ($1) ---
    sql <- glue::glue_sql(
      "SELECT book_id, title, author, skill_id
       FROM {`book_table`}
       WHERE book_id = $1;", # Use placeholder $1
      .con = con
    )

    # --- Execute Query with params ---
    result_df <- DBI::dbGetQuery(con, sql, params = list(book_id)) # Pass book_id via params

    # --- Return Result (Standard Data Frame) ---
    # dbGetQuery returns 0-row data.frame if no match, which is the desired output
    return(result_df)

  }, error = function(e) {
    # --- Improved Error Handling ---
    error_message <- glue::glue("Failed to retrieve book with book_id = {book_id}. Database error: {conditionMessage(e)}")
    stop(error_message, call. = FALSE)
  })
}



# Load necessary libraries (assuming they are loaded elsewhere or via NAMESPACE)
# library(DBI)
# library(RPostgres) # Or your specific driver
# library(glue)

#' Retrieve a Book by ID
#'
#' Fetches details for a specific book from `adem.book_recommendations` based on its book ID.
#' ## NOTE: Verify 'adem.books' is the correct table name. ##
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
#' try(get_book_by_id(-5))
#' }
#'
#' @seealso [get_books()], [connect_db()] # Assuming get_books exists
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
    book_id %% 1 != 0 || # Check if it's a whole number
    book_id <= 0         # Typically IDs are positive integers
  ) {
    stop("'book_id' must be a single, non-missing, positive integer value.", call. = FALSE)
  }
  book_id <- as.integer(book_id) # Ensure integer type

  con <- NULL
  tryCatch({
    # --- Database Connection ---
    con <- connect_db()
    on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

    # --- Prepare SQL Query ---
    # !! VERIFY TABLE NAME HERE !!
    book_table <- DBI::Id(schema = 'adem', table = 'book_recommendations')
    sql <- glue::glue_sql(
      "SELECT book_id, title, author, skill_id
       FROM {`book_table`}
       WHERE book_id = $1;",
      .con = con
    )

    # --- Execute Query ---
    result_df <- DBI::dbGetQuery(con, sql, params = list(book_id))

    # --- Return Result ---
    return(result_df)

  }, error = function(e) {
    # --- Error Handling ---
    error_message <- glue::glue("Failed to retrieve book with book_id = {book_id}. Database error: {conditionMessage(e)}")
    stop(error_message, call. = FALSE)
  })
}
# --- Example Call (outside function definition) ---
book_result <- get_book_by_id(101)
 print(book_result)

#' Retrieve Book Details by ID
#'
#' This function retrieves detailed information about a book from the database
#' using the book's unique identifier.
#'
#' @param book_id Integer ID of the book (must be a single integer value)
#' @return A list with book information (book_id, title, author, skill_id)
#'         or NULL if not found
#' @export
#' @examples
#' \dontrun{
#' # Get information for book with ID 101
#' book_info <- get_book_by_id(101)
#' }
get_book_by_id <- function(book_id) {
  # Validate input
  if (!is.numeric(book_id) || length(book_id) != 1 || is.na(book_id) || book_id %% 1 != 0) {
    stop("'book_id' must be a single integer value", call. = FALSE)
  }

  con <- NULL
  tryCatch({
    # Connect to database
    con <- connect_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Build and execute query
    query <- glue::glue_sql(
      "SELECT book_id, title, author, skill_id
       FROM {DBI::Id(schema = 'adem', table = 'book_recommendations')}
       WHERE book_id = {book_id};",
      .con = con
    )

    result <- DBI::dbGetQuery(con, query)

    # Return NULL if not found, otherwise return as named list
    if (nrow(result) == 0) {
      return(NULL)
    } else {
      return(as.list(result))
    }

  }, error = function(e) {
    err_msg <- paste("Failed to retrieve book ID", book_id, ":", e$message)
    stop(err_msg, call. = FALSE)
  })
}


get_book_by_id(101)

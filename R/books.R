# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve Recommended Books
#'
#' Queries the `adem.books` table to retrieve recommended books,
#' optionally filtering by a specific skill ID. Results are ordered by book ID.
#'
#' @param skill Optional character string. If provided, filters for books
#'   associated with this specific skill ID (e.g., `"skill_sql"`).
#'   Must be a single, non-empty string if specified. Defaults to `NULL` (no
#'   skill filter, returns all books).
#'
#' @return A `data.frame` containing columns `book_id`, `title`, `author`,
#'   and `skill_id` for the retrieved books, ordered by `book_id`.
#'   Returns an empty data frame if no books match the criteria.
#'   Stops with an error if the database query fails or if the input
#'   `skill` (if provided) is invalid.
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get all recommended books (consider adding limit if table is large)
#' all_books <- get_books()
#' print(head(all_books))
#'
#' # Get books specifically for the skill "skill_python"
#' python_books <- get_books(skill = "skill_python")
#' print(python_books)
#'
#' # Example of invalid input (will cause an error)
#' try(get_books(skill = c("a", "b")))
#' try(get_books(skill = ""))
#' }
#'
#' @seealso [get_book_by_id()], [get_skills()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id SQL
#' @importFrom glue glue glue_sql
#' @export
#'
get_books <- function(skill = NULL) {

  # --- Input Validation ---
  if (!is.null(skill) && (!is.character(skill) || length(skill) != 1 || nchar(skill) == 0)) {
    stop("'skill' must be a single non-empty character string or NULL.", call. = FALSE)
  }

  con <- NULL # Initialize connection variable
  tryCatch({
    con <- connect_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE) # Ensure disconnection

    # --- Prepare SQL Components ---
    books_table <- DBI::Id(schema = "adem", table = "book_recommendations")

    select_clause <- DBI::SQL("SELECT book_id, title, author, skill_id")
    from_clause <- glue::glue_sql("FROM {`books_table`}", .con = con)
    orderby_clause <- DBI::SQL("ORDER BY book_id") # Order results

    # Initialize optional clauses and params
    where_clause <- DBI::SQL("")
    params <- list()

    # Add WHERE for skill filter if skill is provided
    if (!is.null(skill)) {
      # Assumes skill_id column exists directly in the books table
      where_clause <- glue::glue_sql("WHERE skill_id = $1", .con = con)
      params <- list(skill)
    }

    # --- Assemble Final Query using glue_sql ---
    final_sql <- glue::glue_sql(
      "{select_clause} {from_clause} {where_clause} {orderby_clause};",
      .con = con
    )

    # --- Execute Query ---
    result <- DBI::dbGetQuery(con, final_sql, params = params)
    return(result)

  }, error = function(e) {
    # --- Error Handling ---
    filter_desc <- if (!is.null(skill)) paste0(" for skill '", skill, "'") else ""
    error_message <- glue::glue("Failed to retrieve books{filter_desc}: {e$message}")
    stop(error_message, call. = FALSE)
  })
}



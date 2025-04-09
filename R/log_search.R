# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Log a User Search Query
#'
#' Inserts a record into the `adem.search_logs` table to log a search query
#' performed by a user. Includes the user ID, the query text, and the
#' current timestamp.
#'
#' @param user_id A single integer representing the unique ID of the user
#'   performing the search. Must be a single, non-missing, integer value.
#' @param query A character string representing the search query text entered
#'   by the user. Must be a single, non-empty string.
#'
#' @return `TRUE` if the log entry was successfully inserted into the database,
#'   `FALSE` otherwise (e.g., due to database connection issues, constraint
#'   violations, or other errors during execution). Warnings may be issued
#'   on failure.
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Log a search for user 1
#' success <- log_search(user_id = 1, query = "data science jobs zurich")
#' if (success) {
#'   print("Search logged successfully.")
#' } else {
#'   print("Failed to log search.")
#' }
#'
#' # Log another search
#' log_search(user_id = 5, query = "R programming courses")
#'
#' # Examples of invalid input (will cause errors before DB interaction)
#' try(log_search(user_id = "abc", query = "test"))
#' try(log_search(user_id = 1, query = ""))
#' try(log_search(user_id = c(1, 2), query = "multi user"))
#' }
#'
#' @seealso [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbExecute Id
#' @importFrom glue glue glue_sql
#' @export
log_search <- function(user_id, query) {

  # --- Input Validation ---
  if (!is.numeric(user_id) || length(user_id) != 1 || is.na(user_id) || user_id %% 1 != 0) {
    stop("'user_id' must be a single integer value.", call. = FALSE)
  }
  if (!is.character(query) || length(query) != 1 || nchar(query) == 0 || is.na(query)) {
    stop("'query' must be a single, non-empty character string.", call. = FALSE)
  }
  # Ensure user_id is integer
  user_id <- as.integer(user_id)

  con <- NULL # Initialize connection variable
  success <- FALSE # Default return value

  tryCatch({
    con <- connect_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE) # Ensure disconnection

    # --- Construct INSERT statement using glue_sql ---
    # Assumes columns: user_id, query, logged_at (using NOW())
    # Use $1, $2 for parameter placeholders
    sql_insert <- glue::glue_sql(
      "INSERT INTO {DBI::Id(schema = 'adem', table = 'search_logs')} (user_id, query, logged_at)
       VALUES ($1, $2, NOW());",
      .con = con # Context for quoting
    )

    # --- Execute INSERT statement ---
    # dbExecute returns the number of affected rows
    rows_affected <- DBI::dbExecute(con, sql_insert, params = list(user_id, query))

    # Check if insert likely succeeded (usually affects 1 row)
    if (rows_affected >= 1) {
      success <- TRUE
    } else {
      # This case might indicate an issue, though technically not an error from dbExecute
      warning(glue::glue("Log search for user {user_id} reported {rows_affected} rows affected (expected 1)."), call. = FALSE)
      success <- FALSE # Treat as failure if 0 rows affected
    }

  }, error = function(e) {
    # --- Error Handling ---
    # Issue a warning instead of stopping, return FALSE
    warning(glue::glue("Failed to log search for user ID {user_id}: {e$message}"), call. = FALSE)
    success <- FALSE # Explicitly set success to FALSE on error
  })

  return(success)
}



# Load necessary libraries
library(DBI)
library(RPostgres)
library(glue)

#' Retrieve Learning Tracks
#'
#' Queries the `adem.learning_tracks` table to retrieve learning tracks,
#' optionally filtering by a required skill ID. Results are ordered by track ID.
#'
#' @param skill_id Optional character string. If provided, filters for learning
#'   tracks associated with this specific skill ID (e.g., `"skill_r"`).
#'   Must be a single, non-empty string if specified. Defaults to `NULL` (no
#'   skill filter, returns all tracks).
#' @param limit Optional integer. Maximum number of records to return.
#'   Defaults to NULL (no limit).
#' @param offset Optional integer. Number of records to skip before returning results.
#'   Useful for pagination. Defaults to 0.
#'
#' @return A `data.frame` containing columns `track_id`, `title`, `description`,
#'   and `url` for the retrieved learning tracks, ordered by `track_id`.
#'   Returns an empty data frame if no tracks match the criteria.
#'
#' @examples
#' \dontrun{
#' # Get all learning tracks
#' all_tracks <- get_learning_tracks()
#'
#' # Get learning tracks for R skills with pagination
#' r_tracks_page1 <- get_learning_tracks(skill_id = "skill_r", limit = 10)
#' r_tracks_page2 <- get_learning_tracks(skill_id = "skill_r", limit = 10, offset = 10)
#' }
#'
#' @seealso [get_learning_track_by_id()], [get_skills()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery dbExecute Id SQL
#' @importFrom glue glue glue_sql
#' @export
get_learning_tracks <- function(skill_id = NULL, limit = NULL, offset = 0) {
  # Input validation
  if (!is.null(skill_id)) {
    if (!is.character(skill_id) || length(skill_id) != 1 || nchar(skill_id) == 0) {
      stop("'skill_id' must be a single non-empty character string or NULL.", call. = FALSE)
    }
  }

  if (!is.null(limit)) {
    if (!is.numeric(limit) || length(limit) != 1 || limit <= 0 || !is.finite(limit)) {
      stop("'limit' must be a positive number or NULL.", call. = FALSE)
    }
    limit <- as.integer(limit)
  }

  if (!is.numeric(offset) || length(offset) != 1 || offset < 0 || !is.finite(offset)) {
    stop("'offset' must be a non-negative number.", call. = FALSE)
  }
  offset <- as.integer(offset)

  # Database connection
  con <- NULL
  tryCatch({
    con <- connect_db()

    # --- Prepare SQL Components ---
    tracks_table <- DBI::Id(schema = "adem", table = "learning_tracks")
    track_skills_table <- DBI::Id(schema = "adem", table = "track_skills")

    select_clause <- DBI::SQL("SELECT DISTINCT lt.track_id, lt.title, lt.description, lt.url")
    from_clause <- glue::glue_sql("FROM {`tracks_table`} lt", .con = con)
    orderby_clause <- DBI::SQL("ORDER BY lt.track_id")

    # Handle skill_id filter
    if (!is.null(skill_id)) {
      join_clause <- glue::glue_sql("INNER JOIN {`track_skills_table`} ts ON lt.track_id = ts.track_id", .con = con)
      # Use a direct string interpolation in a safe way for the skill_id
      where_clause <- glue::glue_sql("WHERE ts.skill_id = {skill_id}", .con = con, skill_id = skill_id)
    } else {
      join_clause <- DBI::SQL("")
      where_clause <- DBI::SQL("")
    }

    # Handle pagination
    limit_clause <- DBI::SQL("")
    if (!is.null(limit)) {
      limit_clause <- glue::glue_sql("LIMIT {limit}", .con = con, limit = limit)
    }

    offset_clause <- DBI::SQL("")
    if (offset > 0) {
      offset_clause <- glue::glue_sql("OFFSET {offset}", .con = con, offset = offset)
    }

    # Construct final SQL
    final_sql <- glue::glue_sql(
      "{select_clause} {from_clause} {join_clause} {where_clause} {orderby_clause} {limit_clause} {offset_clause};",
      .con = con
    )

    # Execute query - use a simpler query execution without the params argument
    result <- DBI::dbGetQuery(con, final_sql)

    # Return results
    return(result)

  }, error = function(e) {
    filter_desc <- if (!is.null(skill_id)) paste0(" for skill_id '", skill_id, "'") else ""
    error_message <- glue::glue("Failed to retrieve learning tracks{filter_desc}: {e$message}")
    stop(error_message, call. = FALSE)

  }, finally = {
    # Ensure database connection is closed
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  })
}

# Example usage
# Uncomment to run:
tracks <- get_learning_tracks(skill_id = "skill_r", limit = 20)
print(tracks)

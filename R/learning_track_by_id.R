# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve Learning Track Details and Associated Skills by ID
#'
#' Fetches detailed information about a specific learning track from
#' `adem.learning_tracks` and retrieves all associated skills by joining
#' with `adem.skills` through the `adem.track_skills` table.
#'
#' @param track_id A single integer representing the unique ID of the learning
#'   track to retrieve details for. Must be a single, non-missing, integer value.
#'
#' @return A list containing two data frames:
#'   \describe{
#'     \item{`track`}{A 1-row data frame with columns `track_id`, `title`,
#'       `description`, `url` for the specified learning track.}
#'     \item{`skills`}{A data frame (potentially with 0 rows) containing
#'       columns `skill_id` and `skill_label` for all skills associated
#'       with that learning track.}
#'   }
#'   Returns `NULL` if the `track_id` is not found in the `adem.learning_tracks`
#'   table. Stops with an error if the database query fails or if the input
#'   `track_id` is invalid.
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get details for track with ID 71 (replace with a valid ID)
#' track_details <- get_learning_track_by_id(71)
#' if (!is.null(track_details)) {
#'   print("Track Info:")
#'   print(track_details$track)
#'   print("Associated Skills:")
#'   print(track_details$skills)
#' } else {
#'   print("Track with ID 71 not found.")
#' }
#'
#' # Try an ID that likely doesn't exist
#' non_existent <- get_learning_track_by_id(99999)
#' print(non_existent) # Should print NULL
#'
#' # Examples of invalid input (will cause errors)
#' try(get_learning_track_by_id("abc"))
#' try(get_learning_track_by_id(c(1, 2)))
#' try(get_learning_track_by_id(10.5))
#' }
#'
#' @seealso [get_learning_tracks()], [get_skill_by_id()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id
#' @importFrom glue glue glue_sql
#' @export
get_learning_track_by_id <- function(track_id) {

  # --- Input Validation ---
  # Check for single, non-missing, integer value
  if (!is.numeric(track_id) || length(track_id) != 1 || is.na(track_id) || track_id %% 1 != 0) {
    stop("'track_id' must be a single integer value.", call. = FALSE)
  }
  # Ensure it's treated as integer for parameters
  track_id <- as.integer(track_id)

  con <- NULL # Initialize connection variable
  tryCatch({
    con <- connect_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE) # Ensure disconnection

    # --- Query 1: Track Info using glue_sql ---
    sql_track <- glue::glue_sql(
      "SELECT track_id, title, description, url
       FROM {DBI::Id(schema = 'adem', table = 'learning_tracks')}
       WHERE track_id = $1;",
      .con = con # Context for quoting
    )
    track_info <- DBI::dbGetQuery(con, sql_track, params = list(track_id))

    # --- Check if track exists ---
    if (nrow(track_info) == 0) {
      return(NULL) # Return NULL if track not found
    }

    # --- Query 2: Skills Info using glue_sql (only if track found) ---
    sql_skills <- glue::glue_sql(
      "SELECT s.skill_id, s.skill_label
       FROM {DBI::Id(schema = 'adem', table = 'skills')} s
       INNER JOIN {DBI::Id(schema = 'adem', table = 'track_skills')} ts ON s.skill_id = ts.skill_id
       WHERE ts.track_id = $1;",
      .con = con # Context for quoting
    )
    skills_info <- DBI::dbGetQuery(con, sql_skills, params = list(track_id))

    # --- Return combined list ---
    return(list(track = track_info, skills = skills_info))

  }, error = function(e) {
    # --- Error Handling ---
    error_message <- glue::glue("Failed to retrieve details for learning track ID {track_id}: {e$message}")
    stop(error_message, call. = FALSE)
  })
}

get_learning_track_by_id(71)

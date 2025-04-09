# Load necessary libraries (assuming they are loaded elsewhere or via NAMESPACE)
# library(DBI)
# library(RPostgres) # Or your specific driver
# library(glue)

#' Retrieve Learning Track Details and Associated Skills by ID
#'
#' This function retrieves detailed information about a learning track and its associated skills
#' from the database using the track's unique identifier.
#'
#' @param track_id Integer. The unique ID of the learning track. Must be a single,
#'   non-missing, positive integer value.
#'
#' @return A list containing two elements:
#'   \describe{
#'     \item{`track`}{A 1-row `data.frame` with track details (`track_id`, `title`, `description`, `url`) from `adem.learning_tracks`.}
#'     \item{`skills`}{A `data.frame` with details (`skill_id`, `skill_label`) of skills associated with the track, joined from `adem.skills` via `adem.track_skills`. Can be 0 rows if no skills are associated.}
#'   }
#'   Returns `NULL` if no learning track with the given `track_id` is found.
#'   Stops with an error on database issues or invalid input.
#'
#' @export
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get information for track with ID 71
#' track_details <- get_learning_track_by_id(71)
#' if (!is.null(track_details)) {
#'   print("Track Info:")
#'   print(track_details$track)
#'   print("Associated Skills:")
#'   print(track_details$skills)
#' } else {
#'   print("Track ID 71 not found.")
#' }
#'
#' # Non-existent track
#' non_existent <- get_learning_track_by_id(9999)
#' print(is.null(non_existent)) # Should be TRUE
#'
#' # Invalid input
#' try(get_learning_track_by_id("abc"))
#' try(get_learning_track_by_id(0)) # Added check for positive ID
#' try(get_learning_track_by_id(NA))
#' try(get_learning_track_by_id(c(1, 2)))
#' }
#' @seealso [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id dbIsValid
#' @importFrom glue glue glue_sql
get_learning_track_by_id <- function(track_id) {

  # --- Input Validation ---
  # Added check for positive integer
  if (!is.numeric(track_id) || length(track_id) != 1 || is.na(track_id) || track_id %% 1 != 0 || track_id <= 0) {
    stop("'track_id' must be a single, non-missing, positive integer value.", call. = FALSE)
  }
  # Ensure it's treated as integer for parameter passing
  track_id <- as.integer(track_id)

  con <- NULL
  tryCatch({
    # --- Database Connection ---
    con <- connect_db() # Assumes this function exists
    # Improved on.exit check
    on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

    # --- Get track information ---
    # Use placeholder $1 for the parameter
    track_table <- DBI::Id(schema = 'adem', table = 'learning_tracks')
    sql_track <- glue::glue_sql(
      "SELECT track_id, title, description, url
       FROM {`track_table`}
       WHERE track_id = $1;", # Use placeholder $1
      .con = con
    )
    # Pass track_id via the params argument
    track_info <- DBI::dbGetQuery(con, sql_track, params = list(track_id))

    # --- Check if track found ---
    if (nrow(track_info) == 0) {
      return(NULL)
    }

    # --- Get associated skills ---
    # Use placeholder $1 for the parameter
    skills_table <- DBI::Id(schema = 'adem', table = 'skills')
    track_skills_table <- DBI::Id(schema = 'adem', table = 'track_skills')
    sql_skills <- glue::glue_sql(
      "SELECT s.skill_id, s.skill_label
       FROM {`skills_table`} s
       JOIN {`track_skills_table`} ts ON s.skill_id = ts.skill_id
       WHERE ts.track_id = $1;", # Use placeholder $1
      .con = con
    )
    # Pass track_id via the params argument
    skills_info <- DBI::dbGetQuery(con, sql_skills, params = list(track_id))

    # --- Return results ---
    return(list(track = track_info, skills = skills_info))

  }, error = function(e) {
    # --- Error Handling ---
    # Use glue for consistent message formatting and conditionMessage for robustness
    error_message <- glue::glue("Failed to retrieve learning track details for track_id = {track_id}. Database error: {conditionMessage(e)}")
    stop(error_message, call. = FALSE)
  })
}




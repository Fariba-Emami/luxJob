#' Retrieve Learning Track Details and Associated Skills by ID
#'
#' This function retrieves detailed information about a learning track and its associated skills
#' from the database using the track's unique identifier.
#'
#' @param track_id Integer ID of the learning track (must be a single integer value)
#' @return A list with two named components:
#'   \itemize{
#'     \item track - A data frame containing track information (track_id, title, description, url)
#'     \item skills - A data frame containing associated skills (skill_id, skill_label)
#'   }
#'   Returns NULL if the track is not found.
#' @export
#' @examples
#' \dontrun{
#' # Get information for track with ID 71
#' track_info <- get_learning_track_by_id(71)
#' }
get_learning_track_by_id <- function(track_id) {
  # Validate input
  if (!is.numeric(track_id) || length(track_id) != 1 || is.na(track_id) || track_id %% 1 != 0) {
    stop("'track_id' must be a single integer value", call. = FALSE)
  }

  con <- NULL
  tryCatch({
    # Connect to database
    con <- connect_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Get track information
    sql_track <- glue::glue_sql(
      "SELECT track_id, title, description, url
       FROM {DBI::Id(schema = 'adem', table = 'learning_tracks')}
       WHERE track_id = {track_id};",
      .con = con
    )
    track_info <- DBI::dbGetQuery(con, sql_track)

    if (nrow(track_info) == 0) {
      return(NULL)
    }

    # Get associated skills
    sql_skills <- glue::glue_sql(
      "SELECT s.skill_id, s.skill_label
       FROM {DBI::Id(schema = 'adem', table = 'skills')} s
       JOIN {DBI::Id(schema = 'adem', table = 'track_skills')} ts
         ON s.skill_id = ts.skill_id
       WHERE ts.track_id = {track_id};",
      .con = con
    )
    skills_info <- DBI::dbGetQuery(con, sql_skills)

    list(track = track_info, skills = skills_info)

  }, error = function(e) {
    # Create error message first
    err_msg <- paste("Failed to retrieve learning track ID", track_id, ":", e$message)
    # Then stop with the message
    stop(err_msg, call. = FALSE)
  })
}
get_learning_track_by_id(71)

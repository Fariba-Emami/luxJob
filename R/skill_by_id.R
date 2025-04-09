# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)
# Assume connect_db() is defined elsewhere and connects to the database

#' Retrieve a Skill by its ID
#'
#' Queries the `adem.skills` table for a specific skill based on its unique
#' identifier.
#'
#' @param skill_id A character string representing the unique ID of the skill
#'   to retrieve (e.g., `"skill_r"`). Must be a single, non-empty string.
#'
#' @return A `data.frame` containing one row with columns `skill_id` and
#'   `skill_label` if the skill is found. Returns `NULL` if no skill matches
#'   the provided `skill_id`. The function will stop with an error if the
#'   database query fails or if the input `skill_id` is invalid.
#'
#' @examples
#' \dontrun{
#' # Ensure connect_db() is defined and database credentials are set
#'
#' # Example: Get the skill with ID "skill_r"
#' r_skill_details <- get_skill_by_id("skill_r")
#' if (!is.null(r_skill_details)) {
#'   print(r_skill_details)
#' } else {
#'   print("Skill 'skill_r' not found.")
#' }
#'
#' # Example: Try to get a skill that likely doesn't exist
#' non_existent_skill <- get_skill_by_id("skill_imaginary_123")
#' print(non_existent_skill) # Should print NULL if not found
#'
#' # Example of invalid input (will cause an error)
#' try(get_skill_by_id(123))
#' try(get_skill_by_id(c("a", "b")))
#' try(get_skill_by_id(""))
#' }
#'
#' @seealso [get_skills()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @export
get_skill_by_id <- function(skill_id) {
  con <- connect_db()
  on.exit(DBI::dbDisconnect(con))

  if (!is.character(skill_id) || length(skill_id) != 1 || nchar(skill_id) == 0) {
    stop("'skill_id' must be a non-empty character string.")
  }

  sql <- "SELECT skill_id, skill_label FROM adem.skills WHERE skill_id = $1;"
  tryCatch({
    result <- DBI::dbGetQuery(con, sql, params = list(skill_id))
    if (nrow(result) == 0) {
      return(NULL) # Return NULL if not found
    }
    return(result)
  }, error = function(e) {
    stop("Failed to retrieve skill by ID: ", e$message, call. = FALSE)
  })
}




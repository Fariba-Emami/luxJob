# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve Vacancy Details and Required Skills by ID
#'
#' Fetches detailed information about a specific job vacancy from
#' `adem.vacancies` and retrieves all associated required skills by joining
#' with `adem.skills` through the `adem.vacancy_skills` table.
#'
#' @param vacancy_id A single integer representing the unique ID of the vacancy
#'   to retrieve details for. Must be a single, non-missing, integer value.
#'
#' @return A list containing two data frames:
#'   \describe{
#'     \item{`vacancy`}{A 1-row data frame with columns `vacancy_id`,
#'       `company_id`, `occupation`, `canton`, `year`, `month` for the
#'       specified vacancy.}
#'     \item{`skills`}{A data frame (potentially with 0 rows) containing
#'       columns `skill_id` and `skill_label` for all skills required
#'       for that vacancy.}
#'   }
#'   Returns `NULL` if the `vacancy_id` is not found in the `adem.vacancies`
#'   table. Stops with an error if the database query fails or if the input
#'   `vacancy_id` is invalid.
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get details for vacancy with ID 123456 (replace with a valid ID)
#' details <- get_vacancy_by_id(123456)
#' if (!is.null(details)) {
#'   print("Vacancy Info:")
#'   print(details$vacancy)
#'   print("Required Skills:")
#'   print(details$skills)
#' } else {
#'   print("Vacancy with ID 123456 not found.")
#' }
#'
#' # Try an ID that likely doesn't exist
#' non_existent <- get_vacancy_by_id(99999999)
#' print(non_existent) # Should print NULL
#'
#' # Examples of invalid input (will cause errors)
#' try(get_vacancy_by_id("abc"))
#' try(get_vacancy_by_id(c(1, 2)))
#' try(get_vacancy_by_id(10.5))
#' }
#'
#' @seealso [get_vacancies()], [get_skill_by_id()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id
#' @importFrom glue glue glue_sql
#' @export
get_vacancy_by_id <- function(vacancy_id) {
  if (!is.numeric(vacancy_id) || length(vacancy_id) != 1) stop("Invalid vacancy_id")

  con <- connect_db()data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql_vacancy <- "SELECT vacancy_id, company_id, occupation, canton, year, month FROM adem.vacancies WHERE vacancy_id = $1;"
  sql_skills <- "SELECT s.skill_id, s.skill_label FROM adem.skills s JOIN adem.vacancy_skills vs ON s.skill_id = vs.skill_id WHERE vs.vacancy_id = $1;"

  tryCatch({
    vacancy_info <- DBI::dbGetQuery(con, sql_vacancy, params = list(vacancy_id))
    if (nrow(vacancy_info) == 0) return(NULL) # Not found
    skills_info <- DBI::dbGetQuery(con, sql_skills, params = list(vacancy_id))
    list(vacancy = vacancy_info, skills = skills_info)
  }, error = function(e) {
    stop("Failed to get vacancy details: ", e$message, call. = FALSE)
  })
}


get_vacancy_by_id(123456)






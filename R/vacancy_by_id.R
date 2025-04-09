# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres) # Or your specific driver
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve Vacancy Details and Required Skills by ID
#'
#' Fetches detailed information about a specific job vacancy from
#' `adem.vacancies` and retrieves all associated required skills by joining
#' with `adem.skills` through the `adem.vacancy_skills` table.
#' Uses secure parameterized queries and proper error handling.
#'
#' @param vacancy_id Integer. The unique ID of the vacancy to retrieve details for.
#'   Must be a single, non-missing, positive integer value.
#'
#' @return A list containing two data frames:
#'   \describe{
#'     \item{`vacancy`}{A 1-row `data.frame` with columns `vacancy_id`,
#'       `company_id`, `occupation`, `canton`, `year`, `month` for the
#'       specified vacancy.}
#'     \item{`skills`}{A `data.frame` (potentially with 0 rows) containing
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
#' print(is.null(non_existent)) # Should be TRUE
#'
#' # Examples of invalid input (will cause errors)
#' try(get_vacancy_by_id("abc"))
#' try(get_vacancy_by_id(c(1, 2)))
#' try(get_vacancy_by_id(10.5))
#' try(get_vacancy_by_id(0)) # Added check for positive ID
#' try(get_vacancy_by_id(NA))
#' }
#'
#' @seealso [get_vacancies()], [get_skill_by_id()], [connect_db()] # Add others if they exist
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id dbIsValid
#' @importFrom glue glue glue_sql
#' @export
get_vacancy_by_id <- function(vacancy_id) {

  # --- Comprehensive Input Validation ---
  if (!is.numeric(vacancy_id) || length(vacancy_id) != 1 || is.na(vacancy_id) || vacancy_id %% 1 != 0 || vacancy_id <= 0) {
    stop("'vacancy_id' must be a single, non-missing, positive integer value.", call. = FALSE)
  }
  # Ensure integer type for parameter passing
  vacancy_id <- as.integer(vacancy_id)

  con <- NULL # Initialize connection variable
  tryCatch({
    # --- Database Connection ---
    con <- connect_db() # Assumes this function exists
    # Ensure disconnection, checking validity first
    on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

    # --- Prepare SQL using glue_sql and DBI::Id ---
    vac_table <- DBI::Id(schema = "adem", table = "vacancies")
    skills_table <- DBI::Id(schema = "adem", table = "skills")
    vac_skills_table <- DBI::Id(schema = "adem", table = "vacancy_skills")

    sql_vacancy <- glue::glue_sql(
      "SELECT vacancy_id, company_id, occupation, canton, year, month
       FROM {`vac_table`}
       WHERE vacancy_id = $1;",
      .con = con
    )

    sql_skills <- glue::glue_sql(
      "SELECT s.skill_id, s.skill_label
       FROM {`skills_table`} s
       JOIN {`vac_skills_table`} vs ON s.skill_id = vs.skill_id
       WHERE vs.vacancy_id = $1;",
      .con = con
    )

    # --- Execute Queries using Parameterized Method ---
    vacancy_info <- DBI::dbGetQuery(con, sql_vacancy, params = list(vacancy_id))

    # --- Check if Vacancy Found ---
    if (nrow(vacancy_info) == 0) {
      return(NULL) # Return NULL if vacancy itself doesn't exist
    }

    # --- Get Associated Skills ---
    # This query runs even if the previous one found the vacancy
    skills_info <- DBI::dbGetQuery(con, sql_skills, params = list(vacancy_id))

    # --- Return Results ---
    return(list(vacancy = vacancy_info, skills = skills_info))

  }, error = function(e) {
    # --- Error Handling ---
    # Provide context (the ID) and the original database error message
    error_message <- glue::glue("Failed to retrieve details for vacancy_id = {vacancy_id}. Database error: {conditionMessage(e)}")
    stop(error_message, call. = FALSE) # Use conditionMessage(e) for robustness
  })
}








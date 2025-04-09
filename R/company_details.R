# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve Company Details and Vacancies
#'
#' Fetches information about a specific company from `adem.companies` and
#' all associated vacancies from `adem.vacancies` based on the company ID.
#'
#' @param company_id A single integer representing the unique ID of the company
#'   to retrieve details for. Must be a single, non-missing, integer value.
#'
#' @return A list containing two data frames:
#'   \describe{
#'     \item{`company`}{A 1-row data frame with columns `company_id`, `name`,
#'       and `sector` for the specified company.}
#'     \item{`vacancies`}{A data frame (potentially with 0 rows) containing
#'       columns `vacancy_id`, `occupation`, `canton`, `year`, `month` for all
#'       vacancies associated with that company.}
#'   }
#'   Returns `NULL` if the `company_id` is not found in the `adem.companies`
#'   table. Stops with an error if the database query fails or if the input
#'   `company_id` is invalid.
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get details for company with ID 42 (replace with a valid ID)
#' details <- get_company_details(42)
#' if (!is.null(details)) {
#'   print("Company Info:")
#'   print(details$company)
#'   print("Vacancies:")
#'   print(head(details$vacancies))
#' } else {
#'   print("Company with ID 42 not found.")
#' }
#'
#' # Try an ID that likely doesn't exist
#' non_existent <- get_company_details(999999)
#' print(non_existent) # Should print NULL
#'
#' # Examples of invalid input (will cause errors)
#' try(get_company_details("abc"))
#' try(get_company_details(c(1, 2)))
#' try(get_company_details(10.5))
#' }
#'
#' @seealso [get_companies()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id
#' @importFrom glue glue glue_sql
#' @export
# --- 5. Get Company Details ---
get_company_details <- function(company_id) {
  if (!is.numeric(company_id) || length(company_id) != 1) stop("Invalid company_id")

  con <- connect_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql_company <- "SELECT company_id, name, sector FROM adem.companies WHERE company_id = $1;"
  sql_vacancies <- "SELECT vacancy_id, occupation, canton, year, month FROM adem.vacancies WHERE company_id = $1;"

  tryCatch({
    company_info <- DBI::dbGetQuery(con, sql_company, params = list(company_id))
    if (nrow(company_info) == 0) return(NULL) # Not found
    vacancies_info <- DBI::dbGetQuery(con, sql_vacancies, params = list(company_id))
    list(company = company_info, vacancies = vacancies_info)
  }, error = function(e) {
    stop("Failed to get company details: ", e$message, call. = FALSE)
  })
}



get_company_details(42)

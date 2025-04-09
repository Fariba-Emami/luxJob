#' Retrieve Filtered Job Vacancies
#'
#' Queries the `adem.vacancies` table for job vacancies, allowing filtering
#' by required skill, company, and canton.
#'
#' @param skill Optional skill ID to filter by (e.g., "skill_python")
#' @param company Optional company ID to filter by
#' @param canton Optional canton to filter by (e.g., "Luxembourg")
#' @param limit Maximum number of results to return (default: 100)
#'
#' @return A data frame of matching vacancies
#' @export
get_vacancies <- function(skill = NULL, company = NULL, canton = NULL, limit = 100) {
  # Basic input validation
  if (!is.null(limit) && (!is.numeric(limit) || limit <= 0)) {
    stop("'limit' must be a positive number")
  }

  # Establish database connection
  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)

  # Start building the query
  query <- "
    SELECT DISTINCT v.vacancy_id, v.company_id, v.occupation, v.canton, v.year, v.month
    FROM adem.vacancies v
  "

  # Add join if we're filtering by skill
  if (!is.null(skill)) {
    query <- paste0(query, "
      INNER JOIN adem.vacancy_skills vs ON v.vacancy_id = vs.vacancy_id
    ")
  }

  # Build WHERE clause components and parameter list
  where_conditions <- c()
  params <- c()  # Use an unnamed vector instead of a named list

  if (!is.null(skill)) {
    where_conditions <- c(where_conditions, "vs.skill_id = $1")
    params <- c(params, skill)
  }

  if (!is.null(company)) {
    param_idx <- length(params) + 1
    where_conditions <- c(where_conditions, paste0("v.company_id = $", param_idx))
    params <- c(params, company)
  }

  if (!is.null(canton)) {
    param_idx <- length(params) + 1
    where_conditions <- c(where_conditions, paste0("v.canton = $", param_idx))
    params <- c(params, canton)
  }

  # Add WHERE clause if we have conditions
  if (length(where_conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(where_conditions, collapse = " AND "))
  }

  # Add ordering and limit
  query <- paste0(query, " ORDER BY v.vacancy_id LIMIT ", limit)

  # Execute query and return results
  tryCatch({
    if (length(params) > 0) {
      result <- dbGetQuery(con, query, params = params)
    } else {
      result <- dbGetQuery(con, query)
    }
    return(result)
  }, error = function(e) {
    message("Query that failed: ", query)
    message("Parameters: ", paste(params, collapse=", "))
    stop(paste("Database query failed:", e$message))
  })
}

get_vacancies(skill = "skill_python", canton = "Luxembourg")

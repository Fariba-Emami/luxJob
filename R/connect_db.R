#' Connect to the ADEM PostgreSQL Database
#'
#' Establishes a connection to the PostgreSQL database using credentials
#' and host information stored in environment variables. This function is used
#' internally by other functions that need to interact with the ADEM database.
#'
#' Environment variables expected:
#' - PG_DB: database name
#' - PG_HOST: database host
#' - PG_USER: database username
#' - PG_PASSWORD: database password
#'
#' @return A DBI connection object (class `"PqConnection"`)
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_db()
#' DBI::dbListTables(con)
#' DBI::dbDisconnect(con)
#' }
connect_db <- function() {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PG_DB"),
    host = Sys.getenv("PG_HOST"),
    user = Sys.getenv("PG_USER"),
    password = Sys.getenv("PG_PASSWORD"),
    port = 5432
  )
  return(con)
}

\
\# --- 6. Get Vacancies (Filtering - still needs dynamic SQL) ---
get_vacancies <- function(skill = NULL, company = NULL, canton = NULL, limit = 100) {
  if (!is.numeric(limit) || limit <= 0) limit <- 100

  base_sql <- "SELECT DISTINCT v.vacancy_id, v.company_id, v.occupation, v.canton, v.year, v.month FROM adem.vacancies v"
  joins <- c()
  wheres <- c()
  params <- list()
  param_idx <- 1

  if (!is.null(skill)) {
    # Basic check
    if (!is.character(skill)) stop("Invalid skill")
    joins <- c(joins, "INNER JOIN adem.vacancy_skills vs ON v.vacancy_id = vs.vacancy_id")
    wheres <- c(wheres, paste0("vs.skill_id = $", param_idx))
    params[[param_idx]] <- skill
    param_idx <- param_idx + 1
  }
  if (!is.null(company)) {
    if (!is.numeric(company)) stop("Invalid company")
    wheres <- c(wheres, paste0("v.company_id = $", param_idx))
    params[[param_idx]] <- company
    param_idx <- param_idx + 1
  }
  if (!is.null(canton)) {
    if (!is.character(canton)) stop("Invalid canton")
    wheres <- c(wheres, paste0("v.canton = $", param_idx))
    params[[param_idx]] <- canton
    param_idx <- param_idx + 1
  }

  sql <- paste(base_sql, paste(joins, collapse = " "))
  if (length(wheres) > 0) {
    sql <- paste(sql, "WHERE", paste(wheres, collapse = " AND "))
  }
  sql <- paste(sql, "ORDER BY v.vacancy_id LIMIT", paste0("$", param_idx)) # Add ORDER BY
  params[[param_idx]] <- limit

  run_query(sql, params = params)
}

# --- 7. Get Vacancy by ID ---
get_vacancy_by_id <- function(vacancy_id) {
  if (!is.numeric(vacancy_id) || length(vacancy_id) != 1) stop("Invalid vacancy_id")

  con <- connect_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql_vacancy <- "SELECT vacancy_id, company_id, occupation, canton, year, month FROM adem.vacancies WHERE vacancy_id = $1;"
  # Assumes adem.vacancy_skills join table and adem.skills table
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

# --- 8. Get Learning Tracks ---
get_learning_tracks <- function(skill_id = NULL) {
  base_sql <- "SELECT DISTINCT lt.track_id, lt.title, lt.description, lt.url FROM adem.learning_tracks lt"
  params <- list()
  sql <- base_sql

  if (!is.null(skill_id)) {
    if (!is.character(skill_id)) stop("Invalid skill_id")
    # Assumes adem.track_skills join table
    sql <- paste(sql, "INNER JOIN adem.track_skills ts ON lt.track_id = ts.track_id WHERE ts.skill_id = $1")
    params <- list(skill_id)
  }
  sql <- paste(sql, "ORDER BY lt.track_id;") # Add ORDER BY
  run_query(sql, params = params)
}

# --- 9. Get Learning Track by ID ---
get_learning_track_by_id <- function(track_id) {
  if (!is.numeric(track_id) || length(track_id) != 1) stop("Invalid track_id")

  con <- connect_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql_track <- "SELECT track_id, title, description, url FROM adem.learning_tracks WHERE track_id = $1;"
  # Assumes adem.track_skills join table and adem.skills table
  sql_skills <- "SELECT s.skill_id, s.skill_label FROM adem.skills s JOIN adem.track_skills ts ON s.skill_id = ts.skill_id WHERE ts.track_id = $1;"

  tryCatch({
    track_info <- DBI::dbGetQuery(con, sql_track, params = list(track_id))
    if (nrow(track_info) == 0) return(NULL) # Not found
    skills_info <- DBI::dbGetQuery(con, sql_skills, params = list(track_id))
    list(track = track_info, skills = skills_info)
  }, error = function(e) {
    stop("Failed to get learning track details: ", e$message, call. = FALSE)
  })
}

# --- 10. Get Books ---
get_books <- function(skill = NULL) {
  sql <- "SELECT book_id, title, author, skill_id FROM adem.books"
  params <- list()
  if (!is.null(skill)) {
    if (!is.character(skill)) stop("Invalid skill")
    sql <- paste(sql, "WHERE skill_id = $1") # Assumes skill_id in books table
    params <- list(skill)
  }
  sql <- paste(sql, "ORDER BY book_id;") # Add ORDER BY
  run_query(sql, params = params)
}

# --- 11. Get Book by ID ---
get_book_by_id <- function(book_id) {
  if (!is.numeric(book_id) || length(book_id) != 1) stop("Invalid book_id")
  sql <- "SELECT book_id, title, author, skill_id FROM adem.books WHERE book_id = $1;"
  # Returns data.frame (potentially empty) as per original spec
  run_query(sql, params = list(book_id))
}

# --- 12. Log Search ---
log_search <- function(user_id, query) {
  # Basic checks
  if (!is.numeric(user_id) || length(user_id) != 1) stop("Invalid user_id")
  if (!is.character(query) || length(query) != 1 || nchar(query) == 0) stop("Invalid query")

  # Assumes adem.search_logs table with user_id, query, logged_at columns
  sql <- "INSERT INTO adem.search_logs (user_id, query, logged_at) VALUES ($1, $2, NOW());"
  run_command(sql, params = list(user_id, query)) # Returns TRUE on success, FALSE otherwise
}













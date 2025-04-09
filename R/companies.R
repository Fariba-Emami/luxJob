
# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve Companies
#'
#' Queries the `adem.companies` table to retrieve a list of companies,
#' ordered by their ID. Allows limiting the number of results returned.
#'
#' @param limit An integer specifying the maximum number of companies to return.
#'   Must be a single positive integer. Defaults to 100 if not specified.
#'   Function stops if `limit` is invalid.
#'
#' @return A `data.frame` containing the `company_id`, `name`, and `sector`
#'   for the retrieved companies, ordered by `company_id`. The number of rows
#'   will be at most `limit`. Returns an empty data frame if no companies
#'   are found (or if the table is empty). Stops with an error if the
#'   database query fails or if the input `limit` is invalid.
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get the default number of companies (up to 100)
#' companies_default <- get_companies()
#' print(head(companies_default))
#'
#' # Get a specific number of companies
#' top_5_companies <- get_companies(limit = 5)
#' print(top_5_companies)
#'
#' # Example of invalid input (will cause an error)
#' try(get_companies(limit = -10))
#' try(get_companies(limit = 0))
#' try(get_companies(limit = "twenty"))
#' try(get_companies(limit = c(10, 20)))
#' }
#'
#' @seealso [get_company_details()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id
#' @importFrom glue glue glue_sql
#' @export
get_companies <- function(limit = 100) {


  if (!is.numeric(limit) || length(limit) != 1 || limit <= 0 || limit != floor(limit)) {
    stop("'limit' must be a single positive integer.", call. = FALSE)
    limit <- 100
  }

  con <- NULL
  tryCatch({
    con <- connect_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    target_table <- DBI::Id(schema = "adem", table = "companies")
    sql_query <- glue::glue_sql(
      "SELECT company_id, name, sector FROM {`target_table`} ORDER BY company_id LIMIT $1",
      .con = con
    )

    result <- DBI::dbGetQuery(con, sql_query, params = list(limit))
    return(result)

  }, error = function(e) {
    stop(glue::glue("Failed to retrieve skills: {e$message}"), call. = FALSE)
  })
}



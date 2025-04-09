
# Load necessary libraries (ensure they are loaded)
# library(DBI)
# library(RPostgres)
# library(glue)

# Assume connect_db() is defined and works

#' Retrieve Skills
#'
#' Queries the `adem.skills` table to retrieve a list of skills,
#' ordered by their ID. Allows limiting the number of results returned.
#'
#' @param limit An integer specifying the maximum number of skills to return.
#'   Must be a single positive integer. Defaults to 100 if not specified.
#'   Function stops if `limit` is invalid.
#'
#' @return A `data.frame` containing the `skill_id` and `skill_label`
#'   for the retrieved skills, ordered by `skill_id`. The number of rows
#'   will be at most `limit`. Returns an empty data frame if no skills
#'   are found (or if the table is empty). Stops with an error if the
#'   database query fails or if the input `limit` is invalid.
#'
#' @examples
#' \dontrun{
#' # Assumes connect_db is defined and database credentials are set
#'
#' # Get the default number of skills (up to 100)
#' skills_default <- get_skills()
#' print(head(skills_default))
#'
#' # Get a specific number of skills
#' top_10_skills <- get_skills(limit = 10)
#' print(top_10_skills)
#'
#' # Example of invalid input (will cause an error)
#' try(get_skills(limit = -5))
#' try(get_skills(limit = 0))
#' try(get_skills(limit = "ten"))
#' try(get_skills(limit = c(10, 20)))
#' }
#'
#' @seealso [get_skill_by_id()], [connect_db()]
#'
#' @importFrom DBI dbDisconnect dbGetQuery Id
#' @importFrom glue glue glue_sql
#' @export
get_skills <- function(limit = 100) {


  if (!is.numeric(limit) || length(limit) != 1 || limit <= 0 || limit != floor(limit)) {
    stop("'limit' must be a single positive integer.", call. = FALSE)
    limit <- 100
  }

  con <- NULL
  tryCatch({
    con <- connect_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    target_table <- DBI::Id(schema = "adem", table = "skills")
    sql_query <- glue::glue_sql(
      "SELECT skill_id, skill_label FROM {`target_table`} ORDER BY skill_id LIMIT $1",
      .con = con
    )

    result <- DBI::dbGetQuery(con, sql_query, params = list(limit))
    return(result)

  }, error = function(e) {
    stop(glue::glue("Failed to retrieve skills: {e$message}"), call. = FALSE)
  })
}

get_skills(limit = 20)


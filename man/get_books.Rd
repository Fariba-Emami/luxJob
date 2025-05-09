% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/books.R
\name{get_books}
\alias{get_books}
\title{Retrieve Recommended Books}
\usage{
get_books(skill = NULL)
}
\arguments{
\item{skill}{Optional character string. If provided, filters for books
associated with this specific skill ID (e.g., \code{"skill_sql"}).
Must be a single, non-empty string if specified. Defaults to \code{NULL} (no
skill filter, returns all books).}
}
\value{
A \code{data.frame} containing columns \code{book_id}, \code{title}, \code{author},
and \code{skill_id} for the retrieved books, ordered by \code{book_id}.
Returns an empty data frame if no books match the criteria.
Stops with an error if the database query fails or if the input
\code{skill} (if provided) is invalid.
}
\description{
Queries the \code{adem.books} table to retrieve recommended books,
optionally filtering by a specific skill ID. Results are ordered by book ID.
}
\examples{
\dontrun{
# Assumes connect_db is defined and database credentials are set

# Get all recommended books (consider adding limit if table is large)
all_books <- get_books()
print(head(all_books))

# Get books specifically for the skill "skill_python"
python_books <- get_books(skill = "skill_python")
print(python_books)

# Example of invalid input (will cause an error)
try(get_books(skill = c("a", "b")))
try(get_books(skill = ""))
}

}
\seealso{
\code{\link[=get_book_by_id]{get_book_by_id()}}, \code{\link[=get_skills]{get_skills()}}, \code{\link[=connect_db]{connect_db()}}
}

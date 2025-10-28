# update_citations.R
# Robust citation updater using SerpAPI
library(httr)
library(jsonlite)

api_key <- Sys.getenv("SERPAPI_KEY")
if (api_key == "") stop("SERPAPI_KEY not set")
scholar_id <- "Eotjew0AAAAJ"

url <- paste0(
  "https://serpapi.com/search.json?",
  "engine=google_scholar_author",
  "&author_id=", scholar_id,
  "&api_key=", api_key
)

res <- GET(url)
stop_for_status(res)

# Parse JSON
data <- content(res, as = "parsed", simplifyVector = FALSE)

# Extract publications
pubs <- data$articles

# Check type
str(pubs[[1]])  # Look at first entry to see its structure

# Create compact citation format safely
citation <- lapply(pubs, function(x) {
  list(
    title = x[["title"]],
    pubid = x[["publication_id"]],
    cites = ifelse(is.null(x[["cited_by"]]), 0, x[["cited_by"]])
  )
})

# Convert to JSON
json_output <- toJSON(citation, pretty = TRUE, auto_unbox = TRUE)

# Save to file
write(json_output, "citations.json")
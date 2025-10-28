# update_citations.R
# Robust citation updater using SerpAPI
library(httr)
library(digest)
library(jsonlite)

api_key <- Sys.getenv("SERPAPI_KEY")
scholar_id <- "Eotjew0AAAAJ"

url <- paste0(
  "https://serpapi.com/search.json?",
  "engine=google_scholar_author",
  "&author_id=", scholar_id,
  "&api_key=", api_key
)

res <- GET(url)
stop_for_status(res)
data <- content(res, as = "parsed", simplifyVector = FALSE)
pubs <- data$articles

# Create compact citation format
citation <- lapply(pubs, function(x) {
  cites_value <- if (!is.null(x[["cited_by"]][["value"]])) as.integer(x[["cited_by"]][["value"]]) else 0
  pubid_value <- if (!is.null(x[["citation_id"]])) x[["citation_id"]] else ""
  # Remove your Google Scholar ID prefix (everything before and including ':')
  pubid_clean <- sub("^Eotjew0AAAAJ:", "", pubid_value)
  list(title = x[["title"]], pubid = pubid_clean, cites = cites_value)})

# Convert to JSON
json_output <- toJSON(citation, pretty = TRUE, auto_unbox = TRUE)

# Save to file
write(json_output, "citations.json")
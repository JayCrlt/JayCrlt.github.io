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

citation <- lapply(pubs, function(x) {
  # Extract cites
  cites_value <- 0
  if (!is.null(x$cited_by) && is.list(x$cited_by) && !is.null(x$cited_by$value)) {
    cites_value <- as.integer(x$cited_by$value)
  }
  
  # Extract pubid from cited_by_link or fallback to hash
  pubid_value <- ""
  if (!is.null(x$cited_by_link)) {
    m <- regmatches(x$cited_by_link, regexpr("cites=([a-zA-Z0-9_-]+)", x$cited_by_link))
    if (length(m) > 0) {
      pubid_value <- sub("cites=", "", m)
    }
  }
  if (pubid_value == "") {
    # fallback: generate stable hash from title
    pubid_value <- substr(digest(x$title, algo = "md5"), 1, 12)
  }
  
  list(
    title = x$title,
    pubid = pubid_value,
    cites = cites_value
  )
})

json_output <- toJSON(citation, pretty = TRUE, auto_unbox = TRUE)
write(json_output, "citations.json")
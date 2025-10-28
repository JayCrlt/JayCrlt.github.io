# update_citations.R
# Robust citation updater using SerpAPI

library(httr)
library(jsonlite)

# Get SerpAPI key from environment
api_key <- Sys.getenv("SERPAPI_KEY")
if (api_key == "") stop("❌ SERPAPI_KEY not set in GitHub Secrets")

# Your Google Scholar ID
scholar_id <- "Eotjew0AAAAJ"

# Safe fetch with retry and error handling
safe_get_publications <- function(id, key, max_attempts = 3) {
  attempt <- 1
  while (attempt <= max_attempts) {
    try({
      url <- paste0(
        "https://serpapi.com/search.json?",
        "engine=google_scholar_author",
        "&author_id=", id,
        "&api_key=", key
      )
      res <- httr::GET(url)
      if (res$status_code != 200) stop("SerpAPI request failed with status ", res$status_code)
      data <- httr::content(res, as = "parsed", simplifyVector = TRUE)
      pubs <- data$publications
      if (is.null(pubs) || length(pubs) == 0) stop("No publications returned by SerpAPI")
      
      # Convert to simple list with title, pubid, cites
      citations <- lapply(pubs, function(pub) {
        list(
          title = pub$title,
          pubid = pub$pub_id,
          cites = pub$cited_by$total
        )
      })
      return(citations)
    }, silent = TRUE)
    
    message("⚠️ Attempt ", attempt, " failed. Retrying in 5 seconds...")
    Sys.sleep(5)
    attempt <- attempt + 1
  }
  warning("❌ Could not fetch publications after ", max_attempts, " attempts")
  return(NULL)
}

# Fetch publications
citations <- safe_get_publications(scholar_id, api_key)

# Write JSON only if data was retrieved
if (!is.null(citations)) {
  jsonlite::write_json(citations, "citations.json", pretty = TRUE, auto_unbox = TRUE)
  message("✅ Citations updated successfully via SerpAPI")
} else {
  message("⚠️ Citations not updated. Keeping existing file.")
}
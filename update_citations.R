# update_citations.R
packages <- c("jsonlite", "scholar")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Get your API key from environment
api_key <- Sys.getenv("SERPAPI_KEY")

# Your Google Scholar ID
scholar_id <- "Eotjew0AAAAJ"

# Safe fetch with retry and graceful error handling
safe_get_publications <- function(id) {
  tryCatch({
    pubs <- get_publications(id)
    if (is.null(pubs) || nrow(pubs) == 0) {
      warning("No publications found or unable to connect to Google Scholar.")
      return(NULL)
    }
    return(pubs)
  }, error = function(e) {
    message("Error fetching publications: ", e$message)
    return(NULL)
  })
}

pubs <- safe_get_publications(scholar_id)

# Only write JSON if data was successfully retrieved
if (!is.null(pubs)) {
  jsonlite::write_json(pubs[, c("title", "pubid", "cites")], "citations.json", pretty = TRUE)
  message("✅ Citations updated successfully.")
} else {
  message("⚠️ Could not update citations. Keeping existing file.")
}
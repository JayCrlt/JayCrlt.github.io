# update_citations.R
# Robust citation updater using SerpAPI
library(httr)
library(digest)
library(jsonlite)
library(readxl)

AltMetric  <- read_excel("StatPub/AltMetric.xlsx")
api_key    <- Sys.getenv("SERPAPI_KEY")
scholar_id <- "Eotjew0AAAAJ"

url <- paste0(
  "https://serpapi.com/search.json?",
  "engine=google_scholar_author",
  "&author_id=", scholar_id,
  "&api_key=", api_key)

res  <- GET(url) ; stop_for_status(res)
data <- content(res, as = "parsed", simplifyVector = FALSE)
pubs <- data$articles

# Create compact citation format
citation <- lapply(pubs, function(x) {
  cites_value <- if (!is.null(x[["cited_by"]][["value"]])) as.integer(x[["cited_by"]][["value"]]) else 0
  pubid_value <- if (!is.null(x[["citation_id"]])) x[["citation_id"]] else ""
  pubid_clean <- sub("^Eotjew0AAAAJ:", "", pubid_value)
  list(title = x[["title"]], pubid = pubid_clean, cites = cites_value)})

# Merge to citations
citation_df <- do.call(rbind, lapply(citation, as.data.frame, stringsAsFactors = FALSE))
AltMetric_df <- AltMetric[, c("pubid", "AltmetricScore")]
citation_df <- merge(citation_df, AltMetric_df, by = "pubid", all.x = TRUE)
citation_df$cites[is.na(citation_df$cites)] <- 0

# convert final data frame back to JSON
json_output <- toJSON(citation_df, pretty = TRUE, auto_unbox = TRUE)
write(json_output, "StatPub/citations.json")
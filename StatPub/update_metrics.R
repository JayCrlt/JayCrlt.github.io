# ============================================================
# Unified citation + Altmetric updater for GitHub webpage
# ============================================================

library(httr)
library(jsonlite)
library(readxl)
library(writexl)
library(digest)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ----------------------------
# CONFIG
# ----------------------------
scholar_id <- "Eotjew0AAAAJ"
SERPAPI_KEY <- Sys.getenv("SERPAPI_KEY")
ALTMETRIC_KEY <- Sys.getenv("ALTMETRIC_KEY")

if (SERPAPI_KEY == "" || ALTMETRIC_KEY == "") {
  stop("Missing SERPAPI_KEY or ALTMETRIC_KEY environment variable")
}

# ----------------------------
# LOAD BASE DATA
# ----------------------------
summary_pub <- read_excel(
  "StatPub/SummaryPub.xlsx",
  col_types = c("text", "text", "text", "text")
)

# Safety check
required_cols <- c("id", "doi", "pubid", "altid")
missing_cols <- setdiff(required_cols, colnames(summary_pub))
if (length(missing_cols) > 0) {
  stop("Missing columns in SummaryPub.xlsx: ",
       paste(missing_cols, collapse = ", "))
}

init <- summary_pub[, c("id", "doi", "pubid")]

# ============================================================
# 1. GOOGLE SCHOLAR — CITATIONS + TITLES
# ============================================================
message("Fetching Google Scholar citations...")

# SerpAPI returns only 20 articles per request by default.
# Ask for 100 per page and loop until every article is retrieved,
# otherwise low-cited (often recent) papers are silently dropped.
articles <- list()
profile  <- NULL      # profile totals (citations, h-index, i10-index) from the first page
start <- 0
repeat {
  res <- GET(
    "https://serpapi.com/search.json",
    query = list(
      engine    = "google_scholar_author",
      author_id = scholar_id,
      api_key   = SERPAPI_KEY,
      num       = 100,     # max page size (default is only 20)
      start     = start
    )
  )
  stop_for_status(res)
  
  data <- content(res, as = "parsed", simplifyVector = FALSE)
  if (is.null(profile)) profile <- data$cited_by$table
  page <- data$articles
  if (length(page) == 0) break
  
  articles <- c(articles, page)
  if (length(page) < 100) break   # last page reached
  start <- start + 100
}
message("Fetched ", length(articles), " articles from Google Scholar")

citation_list <- lapply(articles, function(x) {
  list(
    title = x$title %||% "",
    pubid = sub("^Eotjew0AAAAJ:", "", x$citation_id %||% ""),
    cites = as.integer(x$cited_by$value %||% 0)
  )
})

citation_df <- do.call(
  rbind,
  lapply(citation_list, as.data.frame, stringsAsFactors = FALSE)
)

# ============================================================
# 2. ALTMETRIC SCORES
# ============================================================
message("Fetching Altmetric scores...")

altmetric_urls <- paste0(
  "https://api.altmetric.com/v1/id/",
  summary_pub$altid,
  "?key=", ALTMETRIC_KEY
)

alt_scores <- numeric(length(altmetric_urls))

for (i in seq_along(altmetric_urls)) {
  try({
    r <- GET(altmetric_urls[i], user_agent("Mozilla/5.0"))
    if (status_code(r) == 200) {
      d <- fromJSON(content(r, "text", encoding = "UTF-8"))
      alt_scores[i] <- ceiling(d$score %||% 0)
    } else {
      alt_scores[i] <- 0
    }
  }, silent = TRUE)
}

altmetric_df <- summary_pub[, c("doi", "pubid", "altid")]
altmetric_df$AltmetricScore <- alt_scores

# ============================================================
# 3. MERGE EVERYTHING
# ============================================================
message("Merging datasets...")

final_df <- init |>
  merge(citation_df, by = "pubid", all.x = TRUE) |>
  merge(altmetric_df, by = c("doi", "pubid"), all.x = TRUE)

# Flag publications that were not found on Google Scholar
not_found <- final_df$pubid[is.na(final_df$title)]
if (length(not_found) > 0) {
  warning("Not found on Google Scholar (check pubid): ",
          paste(not_found, collapse = ", "))
}

# Replace missing values
final_df$cites[is.na(final_df$cites)] <- 0
final_df$AltmetricScore[is.na(final_df$AltmetricScore)] <- 0
final_df$title[is.na(final_df$title)] <- ""

# Restore original order
final_df <- final_df[order(as.numeric(final_df$id)), ]
final_df$id <- NULL

# Final column order (matches your website expectations)
final_df <- final_df[, c(
  "title",
  "doi",
  "pubid",
  "altid",
  "cites",
  "AltmetricScore"
)]

# ============================================================
# 4. OUTPUT FILES
# ============================================================
message("Writing outputs...")

write(
  toJSON(final_df, pretty = TRUE, auto_unbox = TRUE),
  "StatPub/citations.json"
)

# Google Scholar profile totals, exactly as shown on the profile page
pick <- function(key) {
  for (x in profile) if (!is.null(x[[key]])) return(as.integer(x[[key]]$all %||% NA))
  NA_integer_
}
write(
  toJSON(list(citations = pick("citations"), h_index = pick("h_index"),
              i10_index = pick("i10_index"), updated = format(Sys.Date())),
         pretty = TRUE, auto_unbox = TRUE),
  "StatPub/scholar_profile.json"
)
message("Scholar profile: ", pick("citations"), " citations, h-index ", pick("h_index"))

message("✅ Update completed successfully")
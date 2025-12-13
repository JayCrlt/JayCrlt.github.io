# update_metrics.R
# Unified citation + altmetric updater

library(httr)
library(jsonlite)
library(readxl)
library(writexl)
library(digest)

# ---- CONFIG ----
scholar_id <- "Eotjew0AAAAJ"
serp_key   <- Sys.getenv("SERPAPI_KEY")
alt_key    <- Sys.getenv("ALTMETRIC_KEY")

# ---- LOAD BASE DATA ----
summary_pub <- read_excel(
  "StatPub/SummaryPub.xlsx",
  col_types = c("text", "text", "text", "text")
)

init <- summary_pub[, c("id", "title", "doi", "pubid")]

# ==========================
# 1. GOOGLE SCHOLAR CITATIONS
# ==========================
url <- paste0(
  "https://serpapi.com/search.json?",
  "engine=google_scholar_author",
  "&author_id=", scholar_id,
  "&api_key=", serp_key
)

res <- GET(url)
stop_for_status(res)

data <- content(res, as = "parsed", simplifyVector = FALSE)
pubs <- data$articles

citation <- lapply(pubs, function(x) {
  list(
    title = x$title,
    pubid = sub("^Eotjew0AAAAJ:", "", x$citation_id %||% ""),
    cites = as.integer(x$cited_by$value %||% 0)
  )
})

citation_df <- do.call(rbind, lapply(citation, as.data.frame))

# ==========================
# 2. ALTMETRIC
# ==========================
url_api <- paste0(
  "https://api.altmetric.com/v1/id/",
  summary_pub$altid,
  "?key=", alt_key
)

score <- numeric(length(url_api))

for (i in seq_along(url_api)) {
  try({
    r <- GET(url_api[i], user_agent("Mozilla/5.0"))
    if (status_code(r) == 200) {
      d <- fromJSON(content(r, "text", encoding = "UTF-8"))
      score[i] <- ceiling(d$score %||% 0)
    }
  }, silent = TRUE)
}

altmetric_df <- summary_pub[, c("doi", "pubid", "altid")]
altmetric_df$AltmetricScore <- score

# ==========================
# 3. MERGE EVERYTHING
# ==========================
final_df <- init |>
  merge(citation_df, by = "pubid", all.x = TRUE) |>
  merge(altmetric_df, by = c("doi", "pubid"), all.x = TRUE)

final_df$cites[is.na(final_df$cites)] <- 0
final_df$AltmetricScore[is.na(final_df$AltmetricScore)] <- 0

final_df <- final_df[order(as.numeric(final_df$id)), ]
final_df$id <- NULL

# ==========================
# 4. OUTPUTS
# ==========================
write(
  toJSON(final_df, pretty = TRUE, auto_unbox = TRUE),
  "StatPub/citations.json"
)

write_xlsx(
  final_df[, c("doi", "pubid", "altid", "AltmetricScore")],
  "StatPub/AltMetric.xlsx"
)
# update_altmetric.R
library(httr)
library(digest)
library(jsonlite)
library(readxl)
library(writexl)

# Load data
stat_pub  <- read_excel("StatPub/SummaryPub.xlsx", col_types = c("text", "text", "text", "text"))
init      <- read_excel("StatPub/SummaryPub.xlsx", col_types = c("text", "text", "text", "text")) 
init      <- init[,c(1,2)]
citations <- fromJSON("StatPub/citations.json", simplifyVector = TRUE)
citations <- citations[, c("title", "pubid", "cites")]
api_key   <- Sys.getenv("ALTMETRIC_KEY")

# Fetch data
url_api <- paste0("https://api.altmetric.com/v1/id/", stat_pub$altid, "?key=", api_key)
resp <- vector("list", length(url_api))
data <- vector("list", length(url_api))
score <- numeric(length(url_api))
for (i in seq_along(url_api)) {
  try({resp[[i]] <- GET(url_api[i], user_agent("Mozilla/5.0"))
    if (status_code(resp[[i]]) == 200) {
      data[[i]] <- fromJSON(content(resp[[i]], "text", encoding = "UTF-8"))
      score[i] <- if (!is.null(data[[i]]$score)) ceiling(data[[i]]$score) else NA
    } else {score[i] <- NA}}, silent = TRUE)}
stat_pub$AltmetricScore <- score

# Convert each row to a list
stat_pub <- lapply(seq_len(nrow(stat_pub)), function(i) {
  row <- stat_pub[i, ]
  list(
    doi = ifelse(!is.na(row$doi), row$doi, ""),
    pubid = ifelse(!is.na(row$pubid), row$pubid, ""),
    altid = ifelse(!is.na(row$altid), as.character(row$altid), ""),
    AltmetricScore = ifelse(!is.na(row$AltmetricScore), row$AltmetricScore, 0))})

# Merge to citations
stat_pub_df <- do.call(rbind, lapply(stat_pub, as.data.frame))
stat_pub_df <- merge(stat_pub_df, citations, by = "pubid", all.x = TRUE)
stat_pub_df$cites[is.na(stat_pub_df$cites)] <- 0

# convert final data frame back to JSON
stat_pub_df = merge(init, stat_pub_df, by = "doi")
stat_pub_df$altid[stat_pub_df$altid == ""] <- NA
stat_pub_df = stat_pub_df[,c(2,1,3:4,6,5,7)]
stat_pub_df <- stat_pub_df[order(as.numeric(stat_pub_df$id)), ]
stat_pub_df = stat_pub_df[,-1]
stat_pub_json <- toJSON(stat_pub_df, pretty = TRUE, auto_unbox = TRUE)
write(stat_pub_json, "StatPub/citations.json")

# save data in xlsx
write_xlsx(stat_pub_df[, c(2,1,3,4)], "StatPub/AltMetric.xlsx")
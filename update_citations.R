library(scholar) ; library(jsonlite)

pubs <- get_publications("Eotjew0AAAAJ&hl=fr") 
write_json(pubs[, c("title", "pubid", "cites")], "citations.json", pretty = TRUE)
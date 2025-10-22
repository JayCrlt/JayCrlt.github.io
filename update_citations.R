packages <- c("jsonlite", "scholar")
for (pkg in packages) {if (!require(pkg, character.only = TRUE)) {install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE) 
  library(pkg, character.only = TRUE)}}

pubs <- get_publications("Eotjew0AAAAJ&hl") 
write_json(pubs[, c("title", "pubid", "cites")], "citations.json", pretty = TRUE)
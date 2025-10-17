library(scholar)
library(jsonlite)
library(tidyverse)
id <- "Eotjew0AAAAJ&hl=fr"
pubs <- get_publications(id) |> arrange((-year))
write_json(pubs[, c("title", "pubid", "cites")], "citations.json", pretty = TRUE)
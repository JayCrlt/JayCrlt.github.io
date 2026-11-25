# ============================================================
# Light versions of the gallery photos for the website
# ------------------------------------------------------------
# From every photo in Pictures/My_pictures/ it makes:
#   Pictures/Thumbs/<name>.jpg  -> small thumbnail for the grid (max 600 px, ~30-80 KB)
#   Pictures/Web/<name>.jpg     -> large view when a photo is clicked (max 2000 px, <= 0.5 MB)
# The originals are not touched. Photos already converted are skipped,
# so you can run it again each time you add new pictures.
#
# Run from the website folder:  source("Pictures/make_web_pictures.R")
# Needs the magick package:     install.packages("magick")
# ============================================================

library(magick)

SRC     <- "Pictures/My_pictures"
THUMBS  <- "Pictures/Thumbs"
WEB     <- "Pictures/Web"
MAX_KB  <- 500            # size limit for the large view
dir.create(THUMBS, showWarnings = FALSE)
dir.create(WEB,    showWarnings = FALSE)

files <- list.files(SRC, pattern = "\\.(jpe?g|png)$", ignore.case = TRUE, full.names = TRUE)
base  <- tools::file_path_sans_ext(basename(files))
dup   <- base[duplicated(tolower(base))]
if (length(dup)) warning("Same name with different extensions (only one will be kept): ", paste(dup, collapse = ", "))

for (k in seq_along(files)) {
  f     <- files[k]
  t_out <- file.path(THUMBS, paste0(base[k], ".jpg"))
  w_out <- file.path(WEB,    paste0(base[k], ".jpg"))
  if (file.exists(t_out) && file.exists(w_out) && file.mtime(t_out) >= file.mtime(f)) next

  img <- image_read(f) |> image_orient() |> image_strip()   # respect phone rotation, drop metadata

  # Thumbnail for the grid
  image_write(image_resize(img, "600x600>"), t_out, format = "jpeg", quality = 75)

  # Large view: keep the best quality that fits under MAX_KB
  done <- FALSE
  for (px in c(2000, 1600, 1280)) {
    w <- image_resize(img, paste0(px, "x", px, ">"))
    for (q in c(82, 76, 70)) {
      image_write(w, w_out, format = "jpeg", quality = q)
      if (file.size(w_out) <= MAX_KB * 1024) {done <- TRUE; break}
    }
    if (done) break
  }
  message(sprintf("%-28s thumb %4.0f KB | web %4.0f KB", basename(f),
                  file.size(t_out) / 1024, file.size(w_out) / 1024))
}
message("✅ Done: ", length(files), " photos in ", THUMBS, " and ", WEB)

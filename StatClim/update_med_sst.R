# ============================================================
# Mediterranean sea surface temperature updater for the website
# ------------------------------------------------------------
# Data:   NOAA OISST v2.1, daily, 0.25 deg (via NOAA CoastWatch ERDDAP)
#         - final product  : ncdcOisst21Agg_LonPM180   (~2 weeks behind)
#         - near real time : ncdcOisst21NrtAgg_LonPM180 (~1-2 days behind)
# Output: StatClim/med_sst.json   -> read by index.html
# Cache:  StatClim/med_daily.csv  -> one row per day since 1982
#         StatClim/med_thresh.rds -> per-pixel climatology + heatwave thresholds
#         StatClim/med_map_geom.json / med_map_index.rds -> map hexagons (built once)
#
# First run (no cache yet): downloads 1982 -> today year by year
# (~45 requests of ~15 MB, expect 20-40 min), builds the cache.
# Every later run: only re-downloads the last 30 days, so values
# from the near-real-time product are replaced by the final ones.
# ============================================================

suppressPackageStartupMessages({
  library(httr)
  library(ncdf4)
  library(jsonlite)
  library(matrixStats)
})

# ----------------------------
# CONFIG
# ----------------------------
ERDDAP     <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/"
DS_FINAL   <- "ncdcOisst21Agg_LonPM180"
DS_NRT     <- "ncdcOisst21NrtAgg_LonPM180"
LAT        <- c(30.125, 45.875)     # bounding box of the Mediterranean
LON        <- c(-5.875, 36.125)
BASE_YEARS <- 1982:2011             # climatological baseline (Hobday et al. 2016)
FIRST_YEAR <- 1982
MHW_PCTL   <- 0.90                  # marine heatwave threshold percentile
MHW_MINDUR <- 5                     # min. consecutive days above threshold
REFRESH    <- 30                    # days re-downloaded at every run
HEX_KM     <- 30                    # hexagon size for the anomaly map (km, edge to edge)
MAP_BOX    <- c(-6, 30, 36.5, 46)   # map extent (lon min, lat min, lon max, lat max)
MAP_CRS    <- "+proj=laea +lat_0=38 +lon_0=15 +datum=WGS84 +units=m +no_defs"

DIR        <- "StatClim"
F_DAILY    <- file.path(DIR, "med_daily.csv")
F_THRESH   <- file.path(DIR, "med_thresh.rds")
F_JSON     <- file.path(DIR, "med_sst.json")
F_GEOM     <- file.path(DIR, "med_map_geom.json")   # hexagons + coastline (built once)
F_MAPIDX   <- file.path(DIR, "med_map_index.rds")   # hexagon -> nearest SST pixel

# ----------------------------
# HELPERS
# ----------------------------
# Day of year on a 365-day calendar (29 Feb is dropped everywhere)
doy365 <- function(d) {
  j <- as.integer(format(d, "%j"))
  leap <- as.integer(format(d, "%Y")) %% 4 == 0
  ifelse(leap & j > 59, j - 1L, j)
}
drop_feb29 <- function(d) format(d, "%m-%d") != "02-29"

# Last date available in an ERDDAP dataset
last_time <- function(ds) {
  r <- GET(paste0(ERDDAP, ds, ".csv?", URLencode("time[last]", reserved = TRUE)), timeout(120))
  stop_for_status(r)
  txt <- strsplit(content(r, "text", encoding = "UTF-8"), "\n")[[1]]
  as.Date(substr(txt[3], 1, 10))            # line 1 = names, line 2 = units
}

# Download one block of days -> list(dates, lat, lon, sst[pixel(lon x lat), day])
get_block <- function(ds, d0, d1, tries = 4) {
  q <- sprintf("sst[(%sT12:00:00Z):1:(%sT12:00:00Z)][(0.0)][(%s):1:(%s)][(%s):1:(%s)]",
               d0, d1, LAT[1], LAT[2], LON[1], LON[2])
  url <- paste0(ERDDAP, ds, ".nc?", URLencode(q, reserved = TRUE))
  f <- tempfile(fileext = ".nc")
  for (k in seq_len(tries)) {
    r <- try(GET(url, write_disk(f, overwrite = TRUE), timeout(900)), silent = TRUE)
    if (!inherits(r, "try-error") && status_code(r) == 200) break
    if (k == tries) stop("ERDDAP request failed: ", ds, " ", d0, " -> ", d1)
    Sys.sleep(30 * k)
  }
  nc <- nc_open(f); on.exit({nc_close(nc); unlink(f)})
  lon  <- ncvar_get(nc, "longitude")
  lat  <- ncvar_get(nc, "latitude")
  tim  <- as.Date(as.POSIXct(ncvar_get(nc, "time"), origin = "1970-01-01", tz = "UTC"))
  sst  <- ncvar_get(nc, "sst", collapse_degen = FALSE)  # lon x lat x zlev x time
  dim(sst) <- c(length(lon) * length(lat), length(tim)) # pixels x days
  keep <- drop_feb29(tim)
  list(dates = tim[keep], lon = lon, lat = lat, sst = sst[, keep, drop = FALSE])
}

# Fetch a date range using the final product where available, NRT after that
get_range <- function(d0, d1, final_end) {
  out <- list()
  if (d0 <= final_end) out[[1]] <- get_block(DS_FINAL, d0, min(d1, final_end))
  if (d1 > final_end)  out[[length(out) + 1]] <- get_block(DS_NRT, max(d0, final_end + 1), d1)
  b <- out[[1]]
  if (length(out) == 2) {
    b$dates <- c(b$dates, out[[2]]$dates)
    b$sst   <- cbind(b$sst, out[[2]]$sst)
  }
  b$src <- ifelse(b$dates <= final_end, "final", "nrt")
  b
}

# Mediterranean mask on the bounding-box grid (removes Atlantic, Bay of Biscay,
# Sea of Marmara, Black Sea and the Gulf of Suez)
make_mask <- function(lon, lat, sst) {
  g <- expand.grid(lon = lon, lat = lat)
  ocean <- rowSums(!is.na(sst)) > 0
  out <- (g$lon < -5.6) |                       # Atlantic west of Gibraltar
         (g$lon < 0   & g$lat > 41) |           # Bay of Biscay
         (g$lon > 26.5 & g$lat > 40.2) |        # Dardanelles, Marmara, Black Sea
         (g$lon > 32  & g$lat < 30.9)           # Gulf of Suez
  list(idx = which(ocean & !out), w = cos(g$lat[ocean & !out] * pi / 180))
}

# Area-weighted basin mean for each day (columns)
basin_mean <- function(m, w) {
  ok <- !is.na(m)
  colSums(m * w, na.rm = TRUE) / colSums(ok * w)
}

# Hobday et al. (2016) per-pixel climatology (mean) and threshold (90th
# percentile) from an 11-day window centred on each day of year, both then
# smoothed with a 31-day moving average.
build_threshold <- function(base, base_doy) {
  npix <- nrow(base)
  th <- matrix(NA_real_, 365, npix); mu <- th
  for (d in 1:365) {
    win <- ((d - 6 + 0:10) %% 365) + 1
    x <- t(base[, which(base_doy %in% win), drop = FALSE])
    th[d, ] <- colQuantiles(x, probs = MHW_PCTL, na.rm = TRUE)
    mu[d, ] <- colMeans(x, na.rm = TRUE)
  }
  smooth <- function(a) {
    out <- a
    for (d in 1:365) out[d, ] <- colMeans(a[((d - 16 + 0:30) %% 365) + 1, , drop = FALSE], na.rm = TRUE)
    out
  }
  list(thresh = smooth(th), clim = smooth(mu))
}

# Map geometry, built once with sf + rnaturalearth (only needed on that run):
# a hexagon grid over the Mediterranean in a Lambert equal-area projection,
# each hexagon linked to its nearest OISST pixel, plus the country outlines.
# Everything is written in screen coordinates so the page only has to draw it.
build_map_geometry <- function(mask) {
  suppressPackageStartupMessages({library(sf); library(rnaturalearth)})
  sf_use_s2(FALSE)
  box   <- st_as_sfc(st_bbox(c(xmin = MAP_BOX[1], ymin = MAP_BOX[2], xmax = MAP_BOX[3], ymax = MAP_BOX[4]), crs = 4326))
  box_p <- st_transform(st_segmentize(box, 0.25), MAP_CRS)

  # Countries (Natural Earth 1:50m), cropped around the basin and lightly simplified
  land <- ne_countries(scale = "medium", returnclass = "sf") |> st_geometry() |> st_make_valid()
  land <- suppressWarnings(suppressMessages(st_crop(land, xmin = MAP_BOX[1] - 4, ymin = MAP_BOX[2] - 4,
                                                          xmax = MAP_BOX[3] + 4, ymax = MAP_BOX[4] + 4)))
  land <- land[!st_is_empty(land)]
  land <- st_collection_extract(st_cast(land, "GEOMETRYCOLLECTION"), "POLYGON")
  land_p <- st_simplify(st_transform(land, MAP_CRS), dTolerance = 1500)
  land_u <- st_union(land_p)

  # Hexagon grid, keeping sea hexagons with a Mediterranean SST pixel nearby
  pix <- st_transform(st_as_sf(data.frame(lon = mask$lon, lat = mask$lat), coords = c("lon", "lat"), crs = 4326), MAP_CRS)
  hex <- st_make_grid(box_p, cellsize = HEX_KM * 1000, square = FALSE)
  hex <- hex[lengths(st_intersects(hex, box_p)) > 0]
  cen <- st_centroid(hex)
  nn  <- st_nearest_feature(cen, pix)
  dst <- as.numeric(st_distance(cen, pix[nn, ], by_element = TRUE))
  keep <- lengths(st_intersects(cen, land_u)) == 0 & dst <= HEX_KM * 1000
  cen <- cen[keep]; nn <- nn[keep]

  # Screen coordinates: 1000 units wide, y pointing down
  bb <- st_bbox(box_p); W <- 1000; sc <- W / as.numeric(bb["xmax"] - bb["xmin"])
  H  <- round(as.numeric(bb["ymax"] - bb["ymin"]) * sc)
  tx <- function(xy) cbind((xy[, 1] - bb["xmin"]) * sc, (bb["ymax"] - xy[, 2]) * sc)
  ring_path <- function(r) {xy <- tx(r); paste0("M", paste(sprintf("%.1f,%.1f", xy[, 1], xy[, 2]), collapse = "L"), "Z")}
  geom_path <- function(g) {
    polys <- if (inherits(g, "MULTIPOLYGON")) unclass(g) else list(unclass(g))
    paste(unlist(lapply(polys, function(p) vapply(p, ring_path, ""))), collapse = "")
  }
  cxy <- tx(st_coordinates(cen))
  cll <- st_coordinates(st_transform(cen, 4326))
  geom <- list(
    width = W, height = H,
    r     = round(HEX_KM * 1000 / sqrt(3) * sc, 3),       # centre-to-vertex radius (pointy-top)
    hex   = unname(lapply(seq_len(nrow(cxy)), function(i) round(c(cxy[i, 1], cxy[i, 2], cll[i, 1], cll[i, 2]), 2))),
    land  = paste(vapply(land_p, geom_path, ""), collapse = "")
  )
  writeLines(toJSON(geom, auto_unbox = TRUE, digits = NA), F_GEOM)
  saveRDS(nn, F_MAPIDX)
  message("Map geometry built: ", length(nn), " hexagons")
  nn
}

# Share of the basin (area-weighted) in a marine heatwave on each day:
# a pixel counts when it has been above its threshold for >= 5 days in a row
# up to that day. `prev` = exceedance of the 4 days before the block.
mhw_share <- function(m, doys, thresh, w, prev = NULL) {
  above <- m > t(thresh[doys, , drop = FALSE])
  above[is.na(above)] <- FALSE
  full <- if (is.null(prev)) above else cbind(prev, above)
  off  <- ncol(full) - ncol(above)
  run  <- matrix(0L, nrow(full), ncol(full))
  for (j in seq_len(ncol(full))) run[, j] <- if (j == 1) as.integer(full[, 1]) else (run[, j - 1] + 1L) * full[, j]
  inmhw <- run[, (off + 1):ncol(full), drop = FALSE] >= MHW_MINDUR
  list(pct = 100 * colSums(inmhw * w) / sum(w),
       tail = above[, max(1, ncol(above) - MHW_MINDUR + 2):ncol(above), drop = FALSE])
}

# ============================================================
# 1. FIRST RUN: BUILD THE CACHE (1982 -> today)
# ============================================================
final_end <- last_time(DS_FINAL)
nrt_end   <- last_time(DS_NRT)
message("Final product until ", final_end, " | near real time until ", nrt_end)

if (!file.exists(F_DAILY) || !file.exists(F_THRESH)) {
  message("No cache found: building it from ", FIRST_YEAR, " (this takes a while)...")
  tmp <- tempfile("medsst_"); dir.create(tmp)
  mask <- NULL; base <- list(); base_doy <- NULL; daily <- NULL

  # Pass 1: download every year, keep Mediterranean pixels, basin mean
  for (y in FIRST_YEAR:as.integer(format(nrt_end, "%Y"))) {
    d0 <- as.Date(sprintf("%d-01-01", y)); d1 <- min(as.Date(sprintf("%d-12-31", y)), nrt_end)
    message("  ", y)
    b <- get_range(d0, d1, final_end)
    if (is.null(mask)) {mask <- make_mask(b$lon, b$lat, b$sst); g <- expand.grid(lon = b$lon, lat = b$lat)
                        mask$lon <- g$lon[mask$idx]; mask$lat <- g$lat[mask$idx]}
    m <- b$sst[mask$idx, , drop = FALSE]
    saveRDS(list(dates = b$dates, src = b$src, m = m), file.path(tmp, paste0(y, ".rds")))
    daily <- rbind(daily, data.frame(date = b$dates, sst = basin_mean(m, mask$w), src = b$src))
    if (y %in% BASE_YEARS) {base[[length(base) + 1]] <- m; base_doy <- c(base_doy, doy365(b$dates))}
  }

  # Thresholds per pixel
  message("Computing marine heatwave thresholds...")
  base <- do.call(cbind, base)
  bt <- build_threshold(base, base_doy); rm(base); gc()
  thresh <- bt$thresh
  saveRDS(list(mask = mask, thresh = round(bt$thresh, 3), clim = round(bt$clim, 3)), F_THRESH, compress = "xz")

  # Pass 2: marine heatwave share for every day
  message("Computing marine heatwave extent...")
  daily$mhw_pct <- NA_real_; prev <- NULL
  for (y in FIRST_YEAR:as.integer(format(nrt_end, "%Y"))) {
    z <- readRDS(file.path(tmp, paste0(y, ".rds")))
    r <- mhw_share(z$m, doy365(z$dates), thresh, mask$w, prev)
    daily$mhw_pct[match(z$dates, daily$date)] <- r$pct
    prev <- r$tail
  }
  unlink(tmp, recursive = TRUE)
  daily$sst <- round(daily$sst, 3); daily$mhw_pct <- round(daily$mhw_pct, 1)
  write.csv(daily, F_DAILY, row.names = FALSE)
  message("Cache built: ", nrow(daily), " days")
}

# ============================================================
# 2. DAILY UPDATE: REFRESH THE LAST 30 DAYS
# ============================================================
daily <- read.csv(F_DAILY, stringsAsFactors = FALSE)
daily$date <- as.Date(daily$date)
th <- readRDS(F_THRESH); mask <- th$mask; thresh <- th$thresh
if (is.null(th$clim) || is.null(mask$lon)) stop("Old cache format: delete StatClim/med_daily.csv and StatClim/med_thresh.rds, then run again.")

d0 <- max(daily$date) - REFRESH
message("Refreshing ", d0, " -> ", nrt_end)
b <- get_range(d0 - (MHW_MINDUR - 1), nrt_end, final_end)        # 4 extra days of look-back
m <- b$sst[mask$idx, , drop = FALSE]
r <- mhw_share(m, doy365(b$dates), thresh, mask$w)
new <- data.frame(date = b$dates, sst = round(basin_mean(m, mask$w), 3),
                  src = b$src, mhw_pct = round(r$pct, 1))
new <- new[new$date >= d0, ]
daily <- rbind(daily[daily$date < d0, ], new)
daily <- daily[order(daily$date), ]
write.csv(daily, F_DAILY, row.names = FALSE)

# ============================================================
# 3. WEBSITE JSON
# ============================================================
message("Writing website JSON...")
daily$doy  <- doy365(daily$date)
daily$year <- as.integer(format(daily$date, "%Y"))

# Basin climatology 1982-2011 per day of year, lightly smoothed (11-day window)
cl  <- tapply(daily$sst[daily$year %in% BASE_YEARS], daily$doy[daily$year %in% BASE_YEARS], mean)
clim <- sapply(1:365, function(d) mean(cl[((d - 6 + 0:10) %% 365) + 1]))

years <- list()
for (y in sort(unique(daily$year))) {
  v <- rep(NA_real_, 365); s <- daily[daily$year == y, ]
  v[s$doy] <- round(s$sst, 2)
  years[[as.character(y)]] <- v
}

today <- daily[nrow(daily), ]

# Anomaly map: latest day, each pixel vs its own 1982-2011 climatology,
# each hexagon takes the value of its nearest SST pixel
last <- ncol(m)
pix_anom <- m[, last] - th$clim[doy365(b$dates[last]), ]
hex_pix  <- if (file.exists(F_GEOM) && file.exists(F_MAPIDX)) readRDS(F_MAPIDX) else build_map_geometry(mask)
hex_vals <- round(pix_anom[hex_pix], 2)
hex_sst  <- round(m[hex_pix, last], 1)   # actual temperature, shown on hover
rec   <- daily[which.max(daily$sst), ]

out <- list(
  sample   = FALSE,
  updated  = format(today$date),
  source   = paste0("NOAA OISST v2.1 (", ifelse(today$src == "nrt", "preliminary", "final"),
                    "), via NOAA CoastWatch ERDDAP"),
  baseline = "1982-2011",
  today    = list(date = format(today$date), sst = round(today$sst, 2),
                  anomaly = round(today$sst - clim[today$doy], 2),
                  mhw_pct = today$mhw_pct),
  record   = list(date = format(rec$date), sst = round(rec$sst, 2)),
  map      = list(date = format(b$dates[last]), values = hex_vals, sst = hex_sst),
  climatology = round(clim, 2),
  years    = years
)
writeLines(toJSON(out, auto_unbox = TRUE, na = "null", digits = NA), F_JSON)

message("✅ ", format(today$date), ": ", round(today$sst, 2), "°C (",
        sprintf("%+.2f", today$sst - clim[today$doy]), "°C), ",
        today$mhw_pct, "% of the basin in marine heatwave")

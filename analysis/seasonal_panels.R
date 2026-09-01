#!/usr/bin/env Rscript
# Panels for the combined seasonality figures, assembled by ocean basin as
# figures/seasonality_atlantic.png and figures/seasonality_pacific.png, with the
# unweighted and daily-weighted wheels going to the supplementary
# figures/seasonality_time.png and figures/seasonality_time_norm_daily.png.
#
# Writes one file per panel into figures/panels/seasonality/, for assembly in
# Illustrator:
#
#   season_<region>.png       radial seasonality, four species rings
#   season_key_intensity.png  the shared intensity ramp (species names and counts
#                             sit in a boxed key under each region panel)
#   map_<region>_<quarter>.png  the three-month maps, at cropped frames
#   counts_by_region.tsv        per species-region totals, for the caption
#
# Run from the repository root:
#   Rscript analysis/seasonal_panels.R
#
# Two decisions worth knowing before editing:
#
# 1. Seasonality is drawn radially. Day of year is cyclic, and a linear axis has
#    to cut the year somewhere -- a Jan 1 cut slices the austral summer peak in
#    half and draws it as two peaks at opposite ends of the axis, which is the
#    single worst legibility problem in the figures this replaces. A circle has
#    no ends, so the problem does not arise and no rotation has to be justified.
#
# 2. Nothing is smoothed. Every value is a raw count in a stated bin. Bins are
#    5 days: 365 = 5 x 73, so they divide the year exactly and no sector
#    straddles the Dec 31 / Jan 1 join. Binning is on raw day of year for the
#    same reason -- rotating first and taking the modulo back puts one bin
#    astride the boundary and the scale silently drops it, leaving a gap in
#    every ring.

# ggplot2 and friends may live in an renv library rather than the system
# library. Look in this repository first, then in the folder containing it, so
# the script works from a standalone clone and from the original working folder.
# Override with PHYSALIA_RENV if the library is somewhere else.
for (root in c(Sys.getenv("PHYSALIA_RENV"), ".", "..")) {
  if (!nzchar(root)) next
  for (lib in Sys.glob(file.path(root, "renv", "library", "*", "*", "*"))) {
    if (dir.exists(file.path(lib, "ggplot2"))) .libPaths(c(lib, .libPaths()))
  }
}

# read.data.R supplies final_results, the shared `cols` palette, and the
# rnaturalearth basemap. Sourcing it rather than re-reading
# results/final_labeled_dataset.tsv also avoids a trap: that file is written
# with quote = FALSE, so apostrophes in place_guess make the default reader
# drop 1,258 rows without complaint.
suppressPackageStartupMessages(source("analysis/read.data.R"))

OUT <- "figures/panels/seasonality"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

SPP    <- c("physalis", "megalista", "minuta", "utriculus")  # ring order, inward to outward
NB     <- 73     # 5-day bins
BINW   <- 365 / NB
CAP    <- 5      # intensity at which the ramp saturates; see note below
SPARSE <- 50     # below this, a ring is a handful of records, not a season
FLAT   <- 0.40   # fixed intensity for sparse rings, as a fraction of CAP
DPI    <- 500

# Three-month map sizing. MAP_H is the full-size map height in inches; MAP_SCALE
# shrinks it relative to the seasonality panels. Point size is deliberately not
# scaled -- at 0.75 the dots would drop to 0.9 and become hard to see again.
MAP_H      <- 2.1
MAP_SCALE  <- 0.75
AXIS_ALLOW <- 0.35   # room for the degree labels, at fixed type size
MAP_PT     <- 1.2

# CAP is a real choice, not a formality: above it the figure cannot tell 5x from
# 10x. 5 sits just above the maximum of the well-sampled rings, whose ratios top
# out at 5.4x (Southern Africa P. megalista) and are otherwise all below 5. Since
# the FLAT rule below took the n < 50 rings off the ramp, that ring is the only
# bin the cap clips under the seasonal weighting the main figures use (raw and
# daily clip two each, to 6.4x and 6.3x), so saturation no longer needs
# explaining in the caption. Before the FLAT rule, 22
# of 876 populated bins exceeded the cap and 21 of them sat in the four sparse
# rings, reaching 27.2x in a ring of three records -- that concentration is what
# the FLAT rule exists to stop misreporting.

REGIONS <- tibble::tribble(
  ~region,                    ~x0,  ~x1,  ~y0,  ~y1, ~tag,
  "E Australia, New Zealand",  140,  180,  -50,  -10, "EAusNZ",
  "Southern Africa",             5,   45,  -40,    0, "SAfrica",
  "Caribbean, N America",     -100,  -60,    0,   40, "CaribNAm",
  "E South America",           -60,  -20,  -40,    0, "ESAm"
)
QUARTERS <- c("Jan.-Feb.-Mar." = "JFM", "Apr.-May-Jun." = "AMJ",
              "Jul.-Aug.-Sep." = "JAS", "Oct.-Nov.-Dec." = "OND")

save_panel <- function(p, stem, w, h) {
  ggplot2::ggsave(file.path(OUT, paste0(stem, ".png")), p,
                  width = w, height = h, dpi = DPI)
}

obs <- do.call(rbind, lapply(seq_len(nrow(REGIONS)), function(i) {
  r <- REGIONS[i, ]
  final_results %>%
    filter(species %in% SPP,
           !is.na(latitude), !is.na(longitude), !is.na(yd),
           longitude >= r$x0, longitude <= r$x1,
           latitude  >= r$y0, latitude  <= r$y1) %>%
    transmute(region = r$region, species = as.character(species),
              latitude, longitude, yd,
              quarter = cut(yd, c(0, 90, 181, 273, 366), labels = names(QUARTERS)))
})) %>%
  mutate(region  = factor(region, levels = REGIONS$region),
         species = factor(species, levels = SPP))

# --- eastern Pacific exclusion ----------------------------------------------
# The Caribbean / N America region is a lat-lon box, and that box straddles
# Central America, so the eastern Pacific coast falls inside it. 95 of the 127
# P. utriculus records in the box are Pacific-side (Ecuador and Pacific Costa
# Rica), and their seasonality is the opposite of the Atlantic ones: they peak
# Oct-Dec, while Gulf of Mexico P. utriculus peaks Jul-Sep. Pooling them hides
# the Gulf signal the manuscript describes. No P. physalis records are affected.
#
# This polygon traces the continental divide from Mexico through Panama into
# Colombia. Panama is the part that needs care: the isthmus runs east-west, so
# Panama City (8.97N) is Pacific while Colon (9.35N) is Caribbean, 0.4 degrees
# apart, and any simple latitude or longitude rule gets one of them wrong.
#
# The exclusion applies to the seasonality rings only. The maps in the space
# figure show every record inside the box, Pacific included: trimming them would
# misrepresent what was observed in the area they depict.
PAC_EXCLUDE <- sf::st_sfc(sf::st_polygon(list(rbind(
  c(-100, 0), c(-100, 20), c(-97.5, 16.5), c(-95.5, 16.0), c(-93.0, 15.0),
  c(-91.0, 14.2), c(-89.0, 13.5), c(-87.5, 13.2), c(-86.0, 12.0), c(-85.0, 10.5),
  c(-84.0, 9.5), c(-83.0, 8.7), c(-81.8, 8.6), c(-80.5, 8.9), c(-79.7, 9.15),
  c(-79.0, 9.0), c(-78.5, 8.5), c(-77.6, 7.2), c(-77.2, 6.0), c(-77.2, 0),
  c(-100, 0)))), crs = 4326)

in_pacific <- function(lon, lat) {
  as.logical(lengths(sf::st_intersects(
    sf::st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326),
    PAC_EXCLUDE)))
}
obs$pacific  <- in_pacific(obs$longitude, obs$latitude)
obs$ring_use <- !obs$pacific
cat("eastern Pacific records excluded from the rings:\n")
print(as.data.frame(obs %>% filter(pacific) %>% count(region, species)), row.names = FALSE)

# --- seasonality ------------------------------------------------------------
# Each ring is scaled against itself: the weight in a bin over the weight a
# uniform spread across the year would put there. 1 means the bin holds exactly
# its share. That removes n from the comparison, so a 127-record ring and a
# 7,622-record ring can be read side by side, and the raw n sits in the key.
#
# Three weightings are produced. "raw" counts each observation once. The other
# two divide each observation by an estimate of how much observer effort there
# was that day, taken from the beach-species baseline in norm_results, following
# analysis/norm.R:
#
#   seasonal  1 / a cyclic Poisson GAM fitted to daily baseline counts. Smooth,
#             so a single busy or quiet day cannot move a bin far.
#   daily     1 / the raw baseline count for that exact day. Follows real
#             day-to-day activity, but a day with one baseline record carries a
#             hundred times the weight of a day with a hundred, so it is the
#             noisier of the two, badly so where the baseline is thin.
build_rings <- function(dat, wt) {
  dat %>%
    filter(ring_use) %>%
    mutate(bin = pmin(floor((yd - 1) / BINW) + 1, NB), w = .data[[wt]]) %>%
    group_by(region, species, bin) %>%
    summarise(k = sum(w), raw = n(), .groups = "drop") %>%
    right_join(tidyr::expand_grid(region = REGIONS$region, species = SPP, bin = 1:NB),
               by = c("region", "species", "bin")) %>%
    mutate(k = ifelse(is.na(k), 0, k), raw = ifelse(is.na(raw), 0L, raw)) %>%
    group_by(region, species) %>%
    mutate(tot = sum(k), n = sum(raw),
           ratio = ifelse(tot == 0, 0, k / (tot / NB))) %>%
    ungroup() %>%
    mutate(region  = factor(region, levels = REGIONS$region),
           species = factor(species, levels = SPP),
           day     = (bin - 0.5) * BINW,
           # Sparse rings encode presence only, not intensity. Because the
           # ratio is normalised within each ring, a 3-record ring expects
           # 0.04 records per bin, so one observation is 24x expectation and
           # saturates instantly, while a 7,622-record ring expects 104 per bin
           # and a real seasonal peak reaches only 2-4x. Measured on the
           # seasonal weighting the main figures use, the four rings at n < 50
           # were drawn at 0.79 mean alpha with 48% of their cells fully
           # saturated, against 0.24 and 0.2% for the eight rings at n >= 50 --
           # so the rings the figure flags as too sparse to trust were also the
           # ones it drew most emphatically, and the dashed outline argued
           # against the impression the fill created. Drawing them at a fixed
           # FLAT keeps *when* the records fell without claiming how
           # concentrated they were. Rings at n >= SPARSE are unchanged.
           shown   = ifelse(n > 0 & n < SPARSE,
                            ifelse(k > 0, FLAT * CAP, 0),
                            pmin(ratio, CAP)))
}

# Effort weights, fitted per region on that region's own baseline records.
obs$w_raw <- 1
obs$w_seasonal <- NA_real_
obs$w_daily    <- NA_real_
for (i in seq_len(nrow(REGIONS))) {
  r  <- REGIONS[i, ]
  # The effort baseline is cut the same way, so the correction describes the
  # same stretch of coast as the observations it is applied to.
  bl <- norm_results %>%
    filter(!is.na(yd),
           longitude >= r$x0, longitude <= r$x1,
           latitude  >= r$y0, latitude  <= r$y1) %>%
    filter(!in_pacific(longitude, latitude)) %>%
    count(yd, name = "baseline")
  fit <- mgcv::gam(baseline ~ s(yd, bs = "cc", k = 20), data = bl, family = poisson())
  sel <- obs$region == r$region
  obs$w_seasonal[sel] <- 1 / predict(fit, newdata = obs[sel, ], type = "response")
  # Days with no baseline record at all get weight 1, as in norm.R: there is no
  # effort estimate to divide by, and dropping the observation would be worse.
  d <- bl$baseline[match(obs$yd[sel], bl$yd)]
  obs$w_daily[sel] <- ifelse(is.na(d), 1, 1 / d)
  cat(sprintf("%-26s baseline n = %6d over %3d days (min %3d/day)\n",
              r$region, sum(bl$baseline), nrow(bl), min(bl$baseline)))
}

WEIGHTINGS <- c(raw = "w_raw", seasonal = "w_seasonal", daily = "w_daily")

MON_START <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
# Three-letter abbreviations rather than initials: J, A and M each stand for two
# different months, so single letters are ambiguous exactly where the year turns.
MON_LAB   <- month.abb
# Small hole only. The counts used to sit inside it, which forced it wide and
# shrank the rings; they now sit in a strip below the circle instead.
HOLE      <- -0.9



# A dashed circle at each edge of a sparse ring, the polar equivalent of the
# dashed box used on the linear version. geom_path over the whole year renders
# as a ring outline once coord_polar is applied.
ring_outline <- function(sparse_rows) {
  if (!nrow(sparse_rows)) return(NULL)
  do.call(rbind, lapply(seq_len(nrow(sparse_rows)), function(j) {
    s <- sparse_rows[j, ]
    do.call(rbind, lapply(c(-0.45, 0.45), function(off) {
      data.frame(day = seq(0, 365, length.out = 240),
                 y = as.integer(s$species) + off,
                 grp = paste(s$species, off))
    }))
  }))
}

ramp <- tibble::tibble(v = seq(0, CAP, length.out = 400))
key_ramp <- ggplot(ramp, aes(v, 1, alpha = v)) +
  geom_raster(fill = "grey15") +
  scale_alpha_continuous(range = c(0, 1), limits = c(0, CAP), guide = "none") +
  # expand = 0 puts the "none" tick hard against the left edge of the ramp, and
  # its first letter is then clipped by the panel. A small additive expansion
  # leaves the label room without detaching it from the end of the gradient.
  scale_x_continuous(breaks = 0:5, expand = expansion(add = 0.18),
                     labels = c("none", "uniform", "2×", "3×", "4×",
                                "≥5×")) +
  labs(x = NULL, y = NULL, title = "observations") +
  theme_minimal(base_size = 8) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(size = 7, hjust = 0.5),
        axis.text.x = element_text(size = 6.5))


draw_rings <- function(rings, suffix) {
 counts <- distinct(rings, region, species, n)
 made <- list()
 for (i in seq_len(nrow(REGIONS))) {
  rg  <- REGIONS$region[i]
  rr  <- filter(rings, region == rg, n > 0)
  cc  <- filter(counts, region == rg) %>%
    # A literal 0 rather than a dash: it needs no explaining in the caption, and
    # it reads as "not recorded here", which is what the data say. It is not a
    # claim that the species is absent from the region.
    mutate(txt = paste0(n, ifelse(n > 0 & n < SPARSE, "*", "")))
  out <- ring_outline(filter(cc, n > 0, n < SPARSE))

  circle <- ggplot(rr, aes(x = day, y = as.integer(species),
                           fill = species, alpha = shown)) +
    geom_tile(width = BINW, height = 0.9, colour = NA) +
    {if (!is.null(out))
      geom_path(data = out, inherit.aes = FALSE,
                aes(x = day, y = y, group = grp),
                colour = "grey35", linewidth = 0.22, linetype = "22")} +
    coord_polar(theta = "x", start = 0) +
    scale_fill_manual(values = cols, guide = "none") +
    scale_alpha_continuous(range = c(0, 1), limits = c(0, CAP), guide = "none") +
    # oob_keep so the sector spanning the join is drawn rather than dropped
    # Labels at month midpoints, but the spokes on month *boundaries*. With both
    # at the midpoint a sector between two spokes spans mid-month to mid-month,
    # so a record in the first days of a month reads as belonging to the one
    # before it.
    scale_x_continuous(breaks = MON_START + 15, labels = MON_LAB,
                       minor_breaks = MON_START,
                       limits = c(0, 365), oob = scales::oob_keep, expand = c(0, 0)) +
    scale_y_continuous(limits = c(HOLE, length(SPP) + 0.6), breaks = NULL) +
    labs(x = NULL, y = NULL, title = rg) +
    theme_minimal(base_size = 8) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_line(colour = "grey90", linewidth = 0.2),
          plot.title = element_text(size = 8, hjust = 0.5),
          axis.text.x = element_text(size = 6),
          plot.margin = margin(2, 2, 0, 2))

  # A boxed key under each circle: swatch, species, count. Rows run top to
  # bottom in the same order the rings run outward from the center. Carrying the
  # species names here rather than in one shared key makes each panel readable
  # on its own, at the cost of repeating four names across the four panels.
  box <- ggplot(cc, aes(y = -as.integer(species))) +
    geom_tile(aes(x = 0, fill = species), width = 0.3, height = 0.6) +
    geom_text(aes(x = 0.28, label = paste0("P. ", species)),
              hjust = 0, size = 1.85, fontface = "italic", colour = "grey20") +
    geom_text(aes(x = 2.75, label = txt),
              hjust = 1, size = 1.85, colour = "grey20") +
    scale_fill_manual(values = cols, guide = "none") +
    scale_x_continuous(limits = c(-0.35, 2.85)) +
    scale_y_continuous(expand = expansion(mult = 0.22)) +
    theme_void() +
    theme(panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 0.3),
          plot.margin = margin(1, 14, 2, 14))

  panel <- patchwork::wrap_plots(circle, box, ncol = 1, heights = c(1, 0.34))
  save_panel(panel, paste0("season_", REGIONS$tag[i], suffix), 2.5, 3.15)
  made[[REGIONS$tag[i]]] <- panel
 }
 invisible(made)
}


for (wname in names(WEIGHTINGS)) {
  rg_data <- build_rings(obs, WEIGHTINGS[[wname]])
  # Count what is actually clipped on the ramp. Bins in sparse rings are drawn
  # at FLAT rather than by ratio, so their ratios -- which are the extreme ones --
  # are no longer a statement the figure makes, and counting them here would
  # overstate the clipping.
  on_ramp <- rg_data$n >= SPARSE
  over    <- sum(rg_data$ratio > CAP & on_ramp)
  cat(sprintf(paste0("%-9s weighting: %2d of %3d bins on the ramp exceed the %dx cap ",
                     "(max %.1fx); %3d bins in rings at n < %d drawn flat, ",
                     "ratios to %.1fx not shown\n"),
              wname, over, sum(on_ramp & rg_data$n > 0), CAP,
              max(rg_data$ratio[on_ramp]),
              sum(!on_ramp & rg_data$k > 0), SPARSE,
              max(rg_data$ratio[!on_ramp])))
  # All three composites are assembled in Illustrator from these panels; this
  # script writes panels only, so there is one source of truth per figure.
  draw_rings(rg_data, if (wname == "raw") "" else paste0("_norm_", wname))
  if (wname == "raw") counts <- distinct(rg_data, region, species, n)
}

# --- the key ----------------------------------------------------------------
# Drawn by hand because ggplot renders alpha scales with guide_legend, which
# steps the ramp into swatches; the mapping itself is continuous and the key
# should look like it. The species colours occupy the fill channel, so the ramp
# is shown in grey. Ring order and counts live in the per-region boxed key, so
# each panel stands alone and only the ramp needs placing once.
save_panel(key_ramp,  "season_key_intensity", 2.7, 0.62)

# --- three-month maps -------------------------------------------------------
# The nominal region boxes are round numbers; the animals occupy roughly half of
# each. Frames are cut to the 1st-99th percentile of the region's own records,
# so a few strays cannot undo the crop, plus a small margin.
crops <- obs %>%
  group_by(region) %>%
  summarise(cx0 = quantile(longitude, .01), cx1 = quantile(longitude, .99),
            cy0 = quantile(latitude,  .01), cy1 = quantile(latitude,  .99),
            .groups = "drop") %>%
  mutate(pad = pmax(cx1 - cx0, cy1 - cy0) * 0.04,
         cx0 = cx0 - pad, cx1 = cx1 + pad, cy0 = cy0 - pad, cy1 = cy1 + pad) %>%
  left_join(REGIONS, by = "region")

manifest <- list()
for (i in seq_len(nrow(crops))) {
  r <- crops[i, ]
  # Size each file to its own frame so no panel carries dead margin. coord_sf
  # stretches longitude by cos(latitude), which is what sets the width.
  #
  # MAP_SCALE shrinks the drawn map. Type stays at its absolute size so that
  # labels match the seasonality panels when the two are placed together, and
  # AXIS_ALLOW -- the strip the degree labels occupy -- is likewise not scaled.
  # The consequence is that the map area shrinks by MAP_SCALE while the panel
  # file shrinks by less.
  asp <- (r$cy1 - r$cy0) / ((r$cx1 - r$cx0) * cos(mean(c(r$cy0, r$cy1)) * pi / 180))
  h <- MAP_H * MAP_SCALE
  w <- max(1.25, min(3.4, MAP_H / asp)) * MAP_SCALE + AXIS_ALLOW
  for (q in names(QUARTERS)) {
    # Rare species last, so a common species cannot bury them.
    dat <- obs %>% filter(region == r$region, quarter == q) %>%
      add_count(species) %>% arrange(desc(n))
    p <- ggplot() +
      geom_sf(data = world, fill = "#D3D3D3", colour = NA) +
      geom_point(data = dat, aes(longitude, latitude, colour = species),
                 size = MAP_PT, alpha = 0.6, stroke = 0) +
      coord_sf(xlim = c(r$cx0, r$cx1), ylim = c(r$cy0, r$cy1), expand = FALSE) +
      scale_colour_manual(values = cols, guide = "none") +
      scale_x_continuous(breaks = scales::breaks_pretty(3)) +
      scale_y_continuous(breaks = scales::breaks_pretty(4)) +
      labs(title = q, x = NULL, y = NULL) +
      theme_minimal(base_size = 7) +
      theme(panel.grid = element_line(colour = "grey92", linewidth = 0.2),
            panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 0.3),
            plot.title = element_text(size = 7, hjust = 0.5),
            axis.text = element_text(size = 5.5))
    save_panel(p, sprintf("map_%s_%s", r$tag, QUARTERS[[q]]), w, h)
    manifest[[length(manifest) + 1]] <-
      data.frame(region = r$region, panel = q, n = nrow(dat))
  }
}

write.table(counts %>% arrange(region, species),
            file.path(OUT, "counts_by_region.tsv"),
            sep = "\t", row.names = FALSE, quote = FALSE)

cat("\nper-quarter counts:\n"); print(do.call(rbind, manifest), row.names = FALSE)
cat("\nrecords per species-region:\n"); print(as.data.frame(counts), row.names = FALSE)
cat("\ncells below n =", SPARSE, "(marked * in the panels):",
    paste(with(filter(counts, n > 0, n < SPARSE), paste0(region, "/", species, " n=", n)),
          collapse = "; "), "\n")
cat("\nwrote panels to", OUT, "\n")

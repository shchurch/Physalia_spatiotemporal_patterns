# Seasonality index per species and region (S1 Table).
#
# How concentrated are a species' observations within the calendar year? The
# index is the mean resultant length R of the observation dates treated as
# angles: 0 when records are spread evenly across the year, 1 when they all fall
# on a single day. Departure from an even spread is tested with a Rayleigh test.
#
# R is biased upward at small sample sizes -- a perfectly uniform set of 10
# records scores about 0.28, which is higher than the weakest real signal in this
# dataset. The Rayleigh test is unaffected (it is correctly calibrated at every
# n), but R itself is not comparable across rings of different size. Rather than
# drop the small rings, the table reports the value an evenly spread set of the
# same size would score, so the reader can see the floor each R sits above.
#
# Writes two files, both under manuscript/ so the supporting-information files
# for submission sit together:
#   seasonality_index.tsv        the machine-readable table (S1 Table)
#   seasonality_index_table.md   a markdown fragment included by
#                                Supplementary_Figures.qmd, so the rendered table
#                                cannot drift from the data without re-running
#                                this script. The manuscript build stays free of
#                                any R dependency.
# Run from the repository root.

source("analysis/read.data.R")
library(circular)

set.seed(1)

REGIONS <- tibble::tribble(
  ~region,                    ~x0,  ~x1,  ~y0,  ~y1,
  "E Australia, New Zealand",  140,  180,  -50,  -10,
  "Southern Africa",             5,   45,  -40,    0,
  "Caribbean, N America",     -100,  -60,    0,   40,
  "E South America",           -60,  -20,  -40,    0
)
SPP <- c("physalis", "megalista", "minuta", "utriculus")
MIN_N <- 10   # below this a ring is not worth reporting at all

# Expected R for an evenly spread set of n dates. Simulated rather than taken
# from the asymptotic form, which is poor at the sample sizes that matter here.
null_R <- function(n, reps = 2000)
  mean(replicate(reps, as.numeric(rho.circular(circular(runif(n, 0, 2 * pi))))))

rows <- list()
for (i in seq_len(nrow(REGIONS))) {
  r <- REGIONS[i, ]
  for (sp in SPP) {
    d <- final_results %>%
      filter(species == sp, !is.na(latitude), !is.na(longitude), !is.na(yd),
             longitude >= r$x0, longitude <= r$x1,
             latitude  >= r$y0, latitude  <= r$y1)
    if (nrow(d) < MIN_N) next
    th   <- circular(d$yd / 365 * 2 * pi, units = "radians")
    peak <- round((as.numeric(median(circular(d$yd / 365 * 360,
              units = "degrees", modulo = "2pi"))) %% 360) / 360 * 365)
    rows[[length(rows) + 1]] <- data.frame(
      region            = r$region,
      species           = sp,
      n                 = nrow(d),
      seasonality_index = round(as.numeric(rho.circular(th)), 2),
      expected_if_even  = round(null_R(nrow(d)), 2),
      peak_date         = format(as.Date(peak - 1, origin = "2023-01-01"), "%d %b"),
      rayleigh_p        = signif(rayleigh.test(th)$p.value, 3)
    )
  }
}

out <- do.call(rbind, rows)
out$region  <- factor(out$region, levels = REGIONS$region)
out$species <- factor(out$species, levels = SPP)
out <- out[order(out$region, out$species), ]

write.table(out, "manuscript/seasonality_index.tsv", sep = "\t",
            row.names = FALSE, quote = FALSE)

# LaTeX fragment for the supplement. Written as a raw LaTeX block rather than a
# pipe table so the column widths and rules can be set: with a plain markdown
# table pandoc wraps the region names over four lines and the table becomes hard
# to read. Region is printed once per block so the four regions read as groups.
esc <- function(x) gsub("&", "\\\\&", x)
tex <- c("```{=latex}",
  "\\begin{table}[H]",
  "\\centering\\footnotesize",
  "\\caption{\\textbf{Seasonality index by species and region.} \\emph{Peak} is the",
  "circular median date; see Methods for the index and null $R$.}",
  "\\begin{tabular}{|p{3.3cm}|p{3.1cm}|r|r|r|l|l|}",
  "\\hline",
  paste("\\textbf{Region} & \\textbf{Species} & \\textbf{n} &",
        "\\textbf{$R$} & \\textbf{Null $R$} & \\textbf{Peak} & \\textbf{Rayleigh $p$} \\\\"),
  "\\hline")
last <- ""
for (i in seq_len(nrow(out))) {
  r  <- out[i, ]
  rg <- if (as.character(r$region) == last) "" else as.character(r$region)
  # "New Zealand" spelled out wraps the column and opens a gap mid-group.
  rg <- sub("New Zealand", "NZ", rg, fixed = TRUE)
  if (rg != "" && i > 1) tex <- c(tex, "\\hline")
  last <- as.character(r$region)
  sp <- if (as.character(r$species) == "utriculus") "utriculus/mikazuki" else as.character(r$species)
  pv <- if (r$rayleigh_p < 1e-4) "$<$0.0001" else format(r$rayleigh_p, digits = 2, scientific = TRUE)
  tex <- c(tex, sprintf("%s & \\textit{P. %s} & %d & %.2f & %.2f & %s & %s \\\\",
      esc(rg), sp, r$n, r$seasonality_index, r$expected_if_even, r$peak_date, pv))
}
tex <- c(tex, "\\hline", "\\end{tabular}", "\\end{table}", "```")
writeLines(tex, "manuscript/seasonality_index_table.md")

print(out, row.names = FALSE)
cat("\nwrote manuscript/seasonality_index.tsv and manuscript/seasonality_index_table.md\n")

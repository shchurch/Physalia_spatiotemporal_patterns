#!/usr/bin/env Rscript
# Supplemental tree figures, coloured by ocean of origin.
#
# Follows the convention of the gene-tree figures in the previous manuscript
# (https://github.com/shchurch/Physalia_population_genomics): root on the
# Rhizophysa MRCA where outgroups
# are present and midpoint-root otherwise, colour tips by ocean, label tips with
# specimen and locality, and print bootstrap support at nodes.
#
# Run from this directory:
#   Rscript plot_trees.R

# ggtree/phytools may live in an renv library rather than the system library.
# Look in this repository first, then in the folder containing it, so the
# script works both from a standalone clone and from the original working
# folder. Override with PHYSALIA_RENV if the library is somewhere else.
for (root in c(Sys.getenv("PHYSALIA_RENV"), "..", "../..")) {
  if (!nzchar(root)) next
  for (lib in Sys.glob(file.path(root, "renv", "library", "*", "*", "*"))) {
    if (dir.exists(file.path(lib, "ggtree"))) .libPaths(c(lib, .libPaths()))
  }
}

suppressPackageStartupMessages({
  library(ape)
  library(ggplot2)
  library(ggtree)
  library(phytools)
  library(phangorn)
  library(dplyr)
})

here <- tryCatch(dirname(normalizePath(sub("^--file=", "",
         grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), error = function(e) ".")
if (is.na(here) || !nzchar(here)) here <- "."
# This script lives in plots/; everything it reads and writes -- the tip tables,
# gene_trees/, mitogenome_tree/, figures/ -- sits one level up in phylogenetics/.
setwd(normalizePath(file.path(here, "..")))

outdir <- file.path("figures")
dir.create(outdir, showWarnings = FALSE)

# Ocean palette carried over verbatim from the previous manuscript's figures so
# the two papers can be read side by side. Gulf of Mexico and NW Atlantic share
# #DAA520 intentionally.
#
# S Atlantic is the one addition: the previous vocabulary has no South Atlantic
# category, and the Saint Helena specimens are the range extension, so without
# it they would be uncoloured.
ocean_cols <- c(
  "Central Pacific"    = "#E78AC3",
  "E Indian"           = "#00008B",
  "Gulf of California" = "#800000",
  "Gulf of Mexico"     = "#DAA520",
  "NE Atlantic"        = "#006400",
  "NW Atlantic"        = "#DAA520",
  "NW Pacific"         = "#DC143C",
  "S Atlantic"         = "#9B59B6",
  "SE Pacific"         = "#FC8D62",
  "SW Atlantic"        = "#452a00",
  "SW Pacific"         = "#686f80",
  "W Indian"           = "#66C2A5"
)

# Tips that appeared in the previous manuscript's trees, used to mark which are
# new here. Squares = new to this study, circles = also in the previous trees.
#
# The tip sets are recorded in previous_study_tips.tsv rather than read from the
# previous study's treefiles, so that this script depends only on files in this
# repository. Those treefiles are available from
# https://github.com/shchurch/Physalia_population_genomics
# and the labels here were extracted from them, normalised as normalise() below.
# Labels in the file are already normalised; only the version suffix is dropped
# here, matching what the old code did after reading each treefile.
PREV_TIPS <- local({
  x <- read.delim("previous_study_tips.tsv", header = TRUE, stringsAsFactors = FALSE)
  split(sub("\\.\\d+$", "", x$tip), x$tree)
})

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

meta <- read.delim("tip_metadata.tsv", header = TRUE, stringsAsFactors = FALSE)

# Tip labels carry MAFFT's _R_ direction marker, a trailing _rc, or a
# _mtgenome_<contig> suffix. Reduce to the key used in tip_metadata.tsv.
normalise <- function(x) {
  x <- sub("^_R_", "", x)
  x <- sub("_rc$", "", x)
  sub("_(mtgenome|mito)_.*$", "", x)
}

rhizophysa <- c("AY937377.1", "GQ120038.1", "GQ120039.1", "GQ120040.1", "GQ120041.1",
                "AY935286.1", "AY935309.1", "AY937351.1", "AY937327.1",
                "KT809335.1", "NC_080942.1", "OQ957206.1", "NC_080941.1", "OQ957199.1",
                "PX372070.1", "PX372075.1", "PX372076.1", "PX372077.1",
                "PX372078.1", "PX372079.1")

plot_tree <- function(tree_file, name, height = 20, width = 11) {
  if (!file.exists(tree_file)) {
    message("  skipping ", name, " -- ", tree_file, " not found"); return(invisible(NULL))
  }
  tr <- read.tree(tree_file)
  tr$tip.label <- normalise(tr$tip.label)

  # Root on the outgroup where we have one; midpoint-root otherwise (ITS has no
  # Rhizophysa sequence on GenBank).
  og <- which(tr$tip.label %in% rhizophysa)
  tr <- tryCatch({
    if (length(og) > 1) {
      nd <- getMRCA(tr, tr$tip.label[og])
      phytools::reroot(tr, nd, position = 0.5 * tr$edge.length[which(tr$edge[, 2] == nd)])
    } else if (length(og) == 1) {
      ape::root(tr, outgroup = tr$tip.label[og], resolve.root = TRUE)
    } else {
      phytools::midpoint.root(tr)
    }
  }, error = function(e) { message("  rooting failed for ", name, ", using midpoint"); phytools::midpoint.root(tr) })

  # Which tips also appeared in the previous manuscript's tree for this locus.
  prev_tips <- if (is.null(PREV_TIPS[[name]])) character(0) else PREV_TIPS[[name]]
  is_prev <- function(x) sub("\\.\\d+$", "", x) %in% prev_tips

  d <- meta %>%
    filter(seq %in% tr$tip.label) %>%
    mutate(label2 = ifelse(nzchar(location), paste(seq, location, sep = " : "), seq),
           ocean = ifelse(nzchar(ocean), ocean, NA),
           status = ifelse(is_prev(seq), "In previous study", "New to this study"))

  n_og <- length(og)
  n_new <- sum(d$status == "New to this study")
  message(sprintf("  %-28s %4d tips, %2d outgroup, %3d new, rooted %s",
                  name, length(tr$tip.label), n_og, n_new,
                  if (n_og > 0) "on Rhizophysa" else "at midpoint"))

  p <- ggtree(tr, size = 0.3)
  max_edge <- max(nodeHeights(tr))

  pdf(file.path(outdir, paste0("tree_", name, "_ocean.pdf")),
      height = height, width = width, useDingbats = FALSE)
  print(
    p %<+% d +
      geom_text2(aes(label = label, subset = !is.na(as.numeric(label)) & as.numeric(label) >= 70),
                 hjust = 1.2, size = 1.5, colour = "grey40") +
      geom_tiplab(aes(label = label2), hjust = -0.06, size = 2.0) +
      geom_tippoint(aes(colour = ocean, shape = status), size = 2.0,
                    position = position_nudge(x = max_edge * 6e-3)) +
      xlim(c(0, max_edge + (max_edge * 0.75))) +
      scale_colour_manual(values = ocean_cols, na.value = "grey70",
                          name = "Ocean of origin", na.translate = FALSE) +
      scale_shape_manual(values = c("In previous study" = 16, "New to this study" = 15),
                         name = "Sampling") +
      geom_treescale(x = 0, y = 0.98 * max(p$data$y, na.rm = TRUE), fontsize = 2.5) +
      theme(legend.position = c(0.12, 0.86),
            legend.key.size = unit(0.45, "cm"),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9))
  )
  dev.off()
  invisible(NULL)
}

message("writing figures to ", outdir, "/")
plot_tree("gene_trees/16S.aln.fasta.contree", "16S",  height = 20)
plot_tree("gene_trees/18S.aln.fasta.contree", "18S",  height = 19)
plot_tree("gene_trees/CO1.aln.fasta.contree", "CO1",  height = 26)
plot_tree("gene_trees/ITS.aln.fasta.contree", "ITS",  height = 24)
plot_tree("mitogenome_tree/submitted_rooted.contree", "mitogenome_submitted_rooted", height = 18)
plot_tree("mitogenome_tree/submitted.contree", "mitogenome_submitted", height = 18)
plot_tree("mitogenome_tree/identification.contree", "mitogenome_identification", height = 21)

# ---------------------------------------------------------------------------
# Collapsed identification tree -- the supplemental figure.
#
# The full 199-tip tree is legible only at 11x21 inches, which a page cannot
# hold: scaled to the text block it is roughly 12 inches tall and the bottom is
# cut off. Collapsing clades that carry no information the figure is making a
# claim about brings it to one page at full size.
#
# A clade collapses only when all three hold:
#   * its maximum internal depth is under COLLAPSE_THR
#   * every tip in it is the same species
#   * every tip in it is on the same side of new-to-this-study
# The species rule keeps a summary label from spanning two species. The
# provenance rule keeps the colour honest, since colour is what distinguishes
# previously identified specimens from what this study adds.
# ---------------------------------------------------------------------------

COLLAPSE_THR <- 0.02   # substitutions/site; max depth within a collapsed clade
COLLAPSE_XPAD <- 4.6   # x-range as a multiple of tree depth, i.e. label room

# Species palette from analysis/read.data.R, so the tree reads with the
# seasonality and classification figures. P. mikazuki is the previous study's B2
# lineage and takes dark red; it is not in that palette, which predates the name.
species_cols <- c("P. megalista" = "purple",
                  "P. minuta"    = "dodgerblue",
                  "P. utriculus" = "dark orange",
                  "P. physalis"  = "dark cyan",
                  "P. mikazuki"  = "dark red",
                  "Rhizophysa (outgroup)" = "grey40",
                  "New to this study"     = "grey65")

# The collecting sheet records two open-ocean station groups as bare codes and
# never defines them. Read off their coordinates in data/sample_ids.tsv: CSPG is
# the northern pair (lat -33.2..-32.8, lon -151.9..-150.7), SSPG the southern
# sixteen (lat -42.1..-37.7, lon -163.6..-152.4), both on the S321 track.
LOC_EXPAND <- c("CSPG" = "Central South Pacific Gyre",
                "SSPG" = "Southern South Pacific Gyre")

plot_collapsed_tree <- function(tree_file, name, thr = COLLAPSE_THR,
                                height = 8.7, width = 7.5) {
  if (!file.exists(tree_file)) {
    message("  skipping ", name, " -- ", tree_file, " not found"); return(invisible(NULL))
  }
  tr <- read.tree(tree_file)
  tr$tip.label <- normalise(tr$tip.label)

  # identification.aln.fa carries YPM-IZ-111760 and YPM-IZ-110972 twice each --
  # two GetOrganelle path variants of one assembly, one reverse-complemented by
  # MAFFT. 201 tips for 199 samples. Each pair is sister at ~5e-4, so dropping
  # the second copy changes the topology not at all and makes the count match
  # the 199 the manuscript reports.
  dupes <- which(duplicated(tr$tip.label))
  if (length(dupes)) {
    message("  dropping ", length(dupes), " duplicate tip(s): ",
            paste(tr$tip.label[dupes], collapse = ", "))
    tr <- drop.tip(tr, dupes)
  }

  og <- which(tr$tip.label %in% rhizophysa)
  tr <- tryCatch({
    if (length(og) > 1) {
      nd <- getMRCA(tr, tr$tip.label[og])
      phytools::reroot(tr, nd, position = 0.5 * tr$edge.length[which(tr$edge[, 2] == nd)])
    } else if (length(og) == 1) {
      ape::root(tr, outgroup = tr$tip.label[og], resolve.root = TRUE)
    } else phytools::midpoint.root(tr)
  }, error = function(e) phytools::midpoint.root(tr))
  OG <- tr$tip.label[tr$tip.label %in% rhizophysa]
  n <- Ntip(tr)

  loc <- setNames(ifelse(meta$location %in% names(LOC_EXPAND),
                         LOC_EXPAND[meta$location], meta$location), meta$seq)
  sp  <- setNames(meta$species, meta$seq)
  ont <- read.delim("ont_species_assignments.tsv", stringsAsFactors = FALSE)
  sp2 <- setNames(ont$species, ont$sample)
  for (o in OG) sp[o] <- "Rhizophysa (outgroup)"

  sp_of <- function(tips) {
    s <- ifelse(!is.na(sp[tips]) & nzchar(sp[tips]), sp[tips], sp2[tips])
    ifelse(is.na(s) | !nzchar(s), NA_character_, s)
  }
  species_of <- function(tips) { s <- sp_of(tips); unique(s[!is.na(s)]) }

  # YPM-IZ-111760, YPM-IZ-110972 and YPM-IZ-104465 have no species in
  # tip_metadata.tsv -- they are the three left out of the GenBank submission,
  # and the gap looks like a side effect of that. Resolve them the way the
  # methods resolve the ONT samples: majority species of the smallest clade
  # containing the sample and at least one identified sample.
  for (t in tr$tip.label[is.na(sp_of(tr$tip.label))]) {
    nd <- match(t, tr$tip.label)
    repeat {
      nd <- tr$edge[tr$edge[, 2] == nd, 1]
      if (!length(nd)) break
      s <- sp_of(setdiff(tr$tip.label[unlist(Descendants(tr, nd, "tips"))], t))
      s <- s[!is.na(s)]
      if (length(s)) {
        sp[t] <- names(sort(table(s), decreasing = TRUE))[1]
        message("  resolved ", t, " -> ", sp[t], " by clade placement")
        break
      }
    }
  }

  # Not `%||%`: that helper tests is.na() on its argument, which is an error on
  # a vector of tip labels rather than a scalar.
  prev <- PREV_TIPS[[name]]
  if (is.null(prev)) prev <- character(0)
  prev <- union(prev, OG)   # outgroups are external sequences, not new material
  is_prev <- function(x) sub("\\.\\d+$", "", x) %in% prev

  D <- dist.nodes(tr)
  depth <- node.depth.edgelength(tr)
  maxdepth <- sapply((n + 1):(n + tr$Nnode),
                     function(x) max(D[x, unlist(Descendants(tr, x, "tips"))]))
  names(maxdepth) <- (n + 1):(n + tr$Nnode)

  homogeneous <- function(tips) {
    length(species_of(tips)) <= 1 && length(unique(is_prev(tips))) == 1
  }
  units <- list()
  walk <- function(nd) {
    if (nd <= n) { units[[length(units) + 1]] <<- nd; return(invisible()) }
    tips <- tr$tip.label[unlist(Descendants(tr, nd, "tips"))]
    if (maxdepth[as.character(nd)] <= thr && homogeneous(tips)) {
      units[[length(units) + 1]] <<- nd; return(invisible())
    }
    for (ch in tr$edge[tr$edge[, 1] == nd, 2]) walk(ch)
  }
  walk(n + 1)

  info <- lapply(units, function(nd) {
    tips <- if (nd <= n) tr$tip.label[nd] else tr$tip.label[unlist(Descendants(tr, nd, "tips"))]
    locs <- unique(loc[tips]); locs <- sort(locs[!is.na(locs) & nzchar(locs)])
    spp  <- species_of(tips)
    list(rep = tips[1], n = length(tips), locs = locs,
         species = if (length(spp)) spp[1] else NA_character_,
         n_new = sum(!is_prev(tips)),
         depth = mean(depth[match(tips, tr$tip.label)]))
  })
  stopifnot(!any(sapply(units, function(nd) {
    tips <- if (nd <= n) tr$tip.label[nd] else tr$tip.label[unlist(Descendants(tr, nd, "tips"))]
    length(species_of(tips)) > 1
  })))

  reps <- sapply(info, `[[`, "rep")
  tr2  <- keep.tip(tr, reps)
  # Place each collapsed tip at the mean root-to-tip depth of the tips it stands
  # for, rather than at the representative's own depth.
  d2 <- node.depth.edgelength(tr2)
  for (u in info) {
    e <- which(tr2$edge[, 2] == match(u$rep, tr2$tip.label))
    tr2$edge.length[e] <- max(u$depth - d2[tr2$edge[e, 1]], 1e-6)
  }
  tr2 <- ladderize(tr2)   # after the edge lengths: ladderize renumbers the edges

  short_sp <- function(x) ifelse(is.na(x), "unassigned", sub("^Physalia ", "P. ", x))
  # Species is carried by colour, so a collapsed label needs only the count and
  # the localities -- which also keeps every tip label plain text.
  lab <- sapply(info, function(u) {
    if (u$n == 1) sprintf("%s : %s", u$rep, if (length(u$locs)) u$locs[1] else "")
    else sprintf("n = %d   %s", u$n, paste(u$locs, collapse = ", "))
  })

  # Join on the representative tip, not on the label text: two collapsed groups
  # can legitimately produce the same summary and the join would drop one.
  d <- data.frame(label = reps, label2 = unname(lab),
                  species = short_sp(sapply(info, function(u) u$species)),
                  n_new = sapply(info, `[[`, "n_new"), stringsAsFactors = FALSE)
  d$key <- ifelse(d$n_new > 0, "New to this study", d$species)
  stopifnot(setequal(d$label, tr2$tip.label), !anyDuplicated(d$label))

  # A labeller function, not a plain vector: scale_*_manual applies `labels` in
  # the scale's own alphabetical order, which pairs each colour with the wrong
  # name.
  key_labels <- function(x) parse(text = ifelse(
    x == "Rhizophysa (outgroup)", 'italic("Rhizophysa")*" (outgroup)"',
    ifelse(x == "New to this study", '"New to this study"',
           paste0('italic("', x, '")'))))

  message(sprintf("  %-30s %3d tips -> %3d display units", name, n, length(info)))

  p <- ggtree(tr2, size = 0.35)
  maxe <- max(node.depth.edgelength(tr2))
  pdf(file.path(outdir, paste0("tree_", name, "_collapsed.pdf")),
      height = height, width = width, useDingbats = FALSE)
  print(
    p %<+% d +
      geom_text2(aes(label = label,
                     subset = !is.na(as.numeric(label)) & as.numeric(label) >= 70),
                 hjust = 1.15, vjust = -0.4, size = 1.6, colour = "grey45") +
      geom_tippoint(aes(colour = key), size = 1.7,
                    position = position_nudge(x = maxe * 5e-3)) +
      # show.legend = FALSE: a text geom inheriting colour stamps an "a" glyph
      # into every key of the colour legend.
      geom_tiplab(aes(label = label2, colour = key), hjust = -0.035, size = 2.1,
                  show.legend = FALSE) +
      scale_colour_manual(values = species_cols, name = NULL,
                          na.value = "grey50", labels = key_labels) +
      xlim(c(0, maxe * COLLAPSE_XPAD)) +
      geom_treescale(x = 0, y = 1, fontsize = 2.2, offset = 0.4) +
      # Outside the panel: the tree is a narrow comb on the left and the labels
      # run the full width, so an inset legend lands on the tip text.
      theme(legend.position = "bottom", legend.box = "horizontal",
            legend.margin = margin(0, 0, 0, 0),
            legend.key.size = unit(0.32, "cm"),
            legend.text = element_text(size = 6.5))
  )
  dev.off()
  invisible(NULL)
}

plot_collapsed_tree("mitogenome_tree/identification.contree", "mitogenome_identification")
message("done")

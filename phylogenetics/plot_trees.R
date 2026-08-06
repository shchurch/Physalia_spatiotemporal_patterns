#!/usr/bin/env Rscript
# Supplemental tree figures, coloured by ocean of origin.
#
# Follows the convention of the gene-tree figures in the previous manuscript
# (physalia/R_code/genetrees.qmd): root on the Rhizophysa MRCA where outgroups
# are present and midpoint-root otherwise, colour tips by ocean, label tips with
# specimen and locality, and print bootstrap support at nodes.
#
# Run from this directory:
#   Rscript plot_trees.R

# ggtree/phytools are installed in the renv library of the working folder that
# contains this clone, not in the system library. Add it if it is there.
for (lib in Sys.glob(file.path("~/Downloads/Physalia_spatiotemporal",
                               "renv", "library", "*", "*", "*"))) {
  if (dir.exists(file.path(lib, "ggtree"))) .libPaths(c(lib, .libPaths()))
}

suppressPackageStartupMessages({
  library(ape)
  library(ggplot2)
  library(ggtree)
  library(phytools)
  library(dplyr)
})

here <- tryCatch(dirname(normalizePath(sub("^--file=", "",
         grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), error = function(e) ".")
if (is.na(here) || !nzchar(here)) here <- "."
setwd(here)

outdir <- file.path("figures")
dir.create(outdir, showWarnings = FALSE)

# Ocean palette carried over verbatim from the previous manuscript's figures so
# the two papers can be read side by side. Gulf of Mexico and NW Atlantic share
# #DAA520 intentionally.
#
# SE Atlantic is the one addition: the previous vocabulary has no South Atlantic
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
  "SE Atlantic"        = "#9B59B6",
  "SE Pacific"         = "#FC8D62",
  "SW Atlantic"        = "#452a00",
  "SW Pacific"         = "#686f80",
  "W Indian"           = "#66C2A5"
)

# Trees published in the previous manuscript, used to mark which tips are new
# here. Squares = new to this study, circles = also in the previous trees.
PREV_TREES <- c(
  "16S" = "~/Downloads/physalia/results/iqtree/16S.aln.fasta.treefile",
  "18S" = "~/Downloads/physalia/results/iqtree/18S.aln.fasta.treefile",
  "CO1" = "~/Downloads/physalia/results/iqtree/CO1.aln.fasta.treefile",
  "ITS" = "~/Downloads/physalia/results/iqtree/ITS.aln.fasta.treefile",
  "mitogenome_submitted"      = "~/Downloads/physalia/results/iqtree/mito.aln.fa.treefile",
  "mitogenome_submitted_rooted" = "~/Downloads/physalia/results/iqtree/mito.aln.fa.treefile",
  "mitogenome_identification" = "~/Downloads/physalia/results/iqtree/mito.aln.fa.treefile"
)

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
  prev_tips <- character(0)
  pf <- path.expand(PREV_TREES[[name]] %||% "")
  if (nzchar(pf) && file.exists(pf)) {
    prev_tips <- normalise(read.tree(pf)$tip.label)
    prev_tips <- sub("\\.\\d+$", "", prev_tips)
  }
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
message("done")

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(stringr)
theme_set(theme_classic())
library(dplyr)
library(ggplot2)
library(RColorBrewer)

  # ---- Plot ----
  gene_colors <- c(
    "cox" = "#77a4b9ff",
    "cob" = "#92eff1ff",
    "rrn" = "#04589cff",
    "trn" = "#4DAF4A",
    "nad" = "#654d89ff",
    "tRNA" = "#610706ff",
    "atp" = "#cf68e7ff",
    "cyt" = "#7300b0ff",
    "mtgenome" = "black"
  )

# ---- Main directory ----
main_dir <- file.path("/Users/samuelchurch/Downloads/Physalia_spatiotemporal/Physalia_annotated", "individual_genomes")

# Find all subdirectories (one per genome ID)
genome_dirs <- list.dirs(main_dir, recursive = FALSE)

super_mtgenome <- data.frame()

read_gff <- function(file_path) {
  gff <- read.delim(file_path, comment.char = "#", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  if (ncol(gff) < 9) stop(paste("Malformed GFF:", file_path))
  colnames(gff)[1:9] <- c("scaffold", "source", "feature", "start", "end", "score", "strand", "phase", "attributes")
  
  # Keep relevant rows
  gff <- gff %>% filter(feature %in% c("gene", "tRNA", "ncRNA_gene"))
  
  gff <- gff %>%
    mutate(
      # Extract name robustly
      gene = case_when(
        grepl("Name=", attributes) ~ sub(".*Name=([^;]+).*", "\\1", attributes),
        grepl("gene_id=", attributes) ~ sub(".*gene_id=([^;]+).*", "\\1", attributes),
        grepl("ID=", attributes) ~ sub(".*ID=([^;]+).*", "\\1", attributes),
        TRUE ~ NA_character_
      ),
      # Simplify names: remove suffixes or long encodings
      gene = gsub("_\\d+$", "", gene),         # drop _0, _1 suffixes
      gene = gsub("-[A-Z]+$", "", gene),       # trnT-TCA -> trnT
      gene = gsub(";.*", "", gene),            # remove anything trailing semicolon
      gene_type = case_when(
        grepl("^cox", gene, ignore.case = TRUE) ~ "cox",
        grepl("^cob", gene, ignore.case = TRUE) ~ "cob",
        grepl("^nad", gene, ignore.case = TRUE) ~ "nad",
        grepl("^atp", gene, ignore.case = TRUE) ~ "atp",
        grepl("^cyt", gene, ignore.case = TRUE) ~ "cyt",
        grepl("^rrn", gene, ignore.case = TRUE) ~ "rrn",
        grepl("^trn", gene, ignore.case = TRUE) ~ "tRNA",
        TRUE ~ "other"
      ),
      file_name = basename(file_path)
    ) %>%
    select(scaffold, start, end, gene, score, strand, file_name, gene_type)
  
  return(gff)
}


read_fasta <- function(file_path) {
  lines <- readLines(file_path)
  headers <- grep("^>", lines, value = TRUE)
  seq_indices <- which(grepl("^>", lines))
  
  seq_list <- mapply(function(start, end) {
    paste(lines[start:end], collapse = "")
  }, seq_indices + 1, c(seq_indices[-1] - 1, length(lines)), SIMPLIFY = TRUE)
  
  data.frame(
    scaffold = sub("^>", "", headers),
    sequence = seq_list,
    file_name = basename(file_path),
    gene = "mtgenome",
    gene_type = "mtgenome",
    strand = "mtgenome",
    stringsAsFactors = FALSE
  )
}

# ---- Loop over each genome folder ----
for (dir in genome_dirs) {
  dir_name <- basename(dir)
  gff_file <- list.files(dir, pattern = "\\.gff$", full.names = TRUE)
  fasta_file <- list.files(dir, pattern = "\\.fasta$", full.names = TRUE)
  
  if (length(gff_file) == 0 || length(fasta_file) == 0) {
    cat(paste("Skipping", dir_name, "- missing GFF or FASTA\n"))
    next
  }
  
  cat("Processing:", dir_name, "\n")
  output_file <- file.path(dir, paste0(dir_name, "_plot.pdf"))
  
  bed <- read_gff(gff_file)
  fasta <- read_fasta(fasta_file) %>%
    mutate(start = 0, end = nchar(sequence))
  
  mtgenome <- bind_rows(bed, fasta)
  
  # Flip orientation if cox2 strand is "-"
  if (any(mtgenome$gene == "cox2")) {
    strand_val <- mtgenome %>% filter(gene == "cox2") %>% pull(strand) %>% unique()
    if (strand_val == "-") {
      mtgenome <- mtgenome %>%
        mutate(strand = ifelse(strand == "-", "+", "-"),
               tmp_start = start,
               start = -end,
               end = -tmp_start) %>%
        select(-tmp_start)
    }
  }
  
  # Adjust positions relative to cox2
  base_values <- mtgenome %>%
    filter(gene == "cox2") %>%
    select(scaffold, base_value = start)
  
  if (nrow(base_values) == 0) {
    base_values <- data.frame(scaffold = unique(mtgenome$scaffold), base_value = 0)
  }
  
  mtgenome <- mtgenome %>%
    left_join(base_values, by = "scaffold") %>%
    mutate(start = start - base_value, end = end - base_value) %>%
    select(-base_value) %>%
    mutate(scaffold_id = dense_rank(scaffold),
           ymin = ifelse(strand == "-", scaffold_id - 0.5, scaffold_id),
           ymax = ifelse(strand == "+", scaffold_id + 0.5, scaffold_id))
  
  super_mtgenome <- bind_rows(super_mtgenome, mtgenome %>% mutate(dir_name = dir_name))
  
  missing_colors <- setdiff(unique(mtgenome$gene_type), names(gene_colors))
  if (length(missing_colors) > 0)
    gene_colors[missing_colors] <- brewer.pal(max(3, length(missing_colors)), "Set3")
  
  pdf(file = output_file, width = 12, height = 3, useDingbats = FALSE)
  g1 <- ggplot(mtgenome %>% filter(gene != "mtgenome"), aes(
    y = ymin, ymin = ymin, ymax = ymax, xmin = start, xmax = end,
    group = interaction(file_name, gene),
    label = gene,
    fill = gene_type
  )) +
    geom_rect(alpha = 1, color = "black", lwd = 0.05) +
    geom_text(aes(x = (start + end) / 2, y = ymin + 0.25, label = gene),
              angle = 90, color = "black", size = 3) +
    geom_segment(data = mtgenome %>% filter(gene == "mtgenome"),
                 aes(x = start, xend = end, y = scaffold_id),
                 color = "black", linewidth = 1) +
    scale_y_continuous(limits = c(0, max(mtgenome$scaffold_id, na.rm = TRUE) + 1)) +
    scale_fill_manual(values = gene_colors) +
    theme(legend.position = "none",
          axis.text = element_text(size = 8)) +
    xlab("mtgenome") + ylab("")
  
  print(g1)
  dev.off()
  
  cat("Plot saved to:", output_file, "\n")
}

cat("\nAll genomes processed.\n")

mtgenome <- super_mtgenome %>% mutate(scaffold_id = dense_rank(dir_name),
            ymin = ifelse(strand == "-", scaffold_id - 0.25, scaffold_id),
            ymax = ifelse(strand == "+", scaffold_id + 0.25, scaffold_id))

output_file <- paste0("super_mtgenome_plot.pdf")

# Ensure all gene types have colors assigned
unique_gene_types <- unique(mtgenome$gene_type)
missing_colors <- setdiff(unique_gene_types, names(gene_colors))
gene_colors[missing_colors] <- brewer.pal(pmax(3,length(missing_colors)), "Set3")

# Create the plot and save as a PDF
pdf(file = output_file, width = 8, height = 1 * max(mtgenome$scaffold_id), useDingbats = FALSE)

g1 <- ggplot(mtgenome %>% filter(gene != "mtgenome"), aes(
y = ymin, ymin = ymin, ymax = ymax, xmin = start, xmax = end,
group = interaction(file_name, gene),
label = gene,
fill = gene_type
)) + 
geom_rect(alpha = 1, color = "black", lwd = 0.05) + 
geom_text(data = mtgenome %>% filter(gene == "mtgenome"),aes(x = (start + end) / 2, y = ymax + 0.55, label = gsub("(.*)_results_files","\\1",dir_name)), angle = 0, color = "black", size = 3) +  # Add text labels
geom_segment(data = mtgenome %>% filter(gene == "mtgenome"), aes(
    x = start, xend = end, y = scaffold_id
), color = "black", linewidth = 0.5) +
scale_y_continuous(limits = c(0, max(mtgenome$scaffold_id, na.rm = TRUE) + 1)) +
#scale_x_continuous(limits = c(-25000,25000)) +
scale_fill_manual(values = gene_colors) +  # Apply custom color scheme
theme(
    legend.position = "none",
    axis.text = element_text(size = 3)) + xlab("mtgenome") + ylab("")

print(g1)
dev.off()

# Print output file location
cat("Plot saved to:", output_file, "\n")
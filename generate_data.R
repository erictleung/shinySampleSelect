# Create data to demonstrate tool
# Date: 2020-10-15

# Load environment and data -----------------------------------------------

library(here)
library(phyloseq)
library(dplyr)
data(GlobalPatterns)


# Functions ---------------------------------------------------------------

#' Update sample names
#'
#' This function modifies the sample names slightly so that this new phyloseq
#' object can be merged with the original phyloseq object
#'
#' @param ps phyloseq-class object
#'
#' @return phyloseq-class object
#'
#' @example
#' new_ps <- update_sample_names(ps)
update_sample_names <- function(ps, suffix = "Rarefy") {
  new_ps <- ps
  sample_names(new_ps) <- paste0(sample_names(ps), suffix)
  new_ps
}


# Data processing ---------------------------------------------------------
# Create a phyloseq object that can illustrate program

# Agglomerate to higher taxonomic level to make ordination easier
gp_genus <- tax_glom(GlobalPatterns, "Genus")

# Rarefy some samples
soils <-
  gp_genus %>%
  subset_samples(SampleType == "Soil") %>%
  rarefy_even_depth(sample.size = 100000)

# Make minor updates to new objects for easier merging
new_soils <- update_sample_names(soils)

# Final phyloseq object processing
final_gp <- merge_phyloseq(gp_genus, new_soils)

# Check final
final_gp


# Save out object ---------------------------------------------------------
# Save out for quick access and use

save.image(here("data", "sample_data.RData"))

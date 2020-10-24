# Environment setup -------------------------------------------------------

# Utility libraries
library(here)

# Data wrangling
library(dplyr)

# Plotting
library(ggplot2)
library(gghighlight)

# Microbiome
library(phyloseq)

# Shiny interactive
library(shiny)


# Load data ---------------------------------------------------------------
# Here you can load your own data and rename it as `ps`. Otherwise, here we will
# source the code in `generate_data.R` to help create some data from built-in
# datasets to demonstrate this Shiny application.
# source(here("generate_data.R"))


# Data preparation --------------------------------------------------------

# Get sample abundance
psm_filt <- psmelt(ps)
sample_abund <-
  ps %>%
  select(Sample, Abundance, sample_origin, subject_id, visit) %>%
  group_by(Sample, subject_id) %>%
  summarise(Total = sum(Abundance)) %>%
  ungroup()

# Calculate distance measures
# Change these values as needed if no phylogenetic tree is available
ord_filt <- ordinate(ps, "PCoA", "unifrac")
ord_filt_w <- ordinate(ps, "PCoA", "wunifrac")
ord_filt_b <- ordinate(ps, "PCoA", "bray")

# The key values here (e.g., unifrac) will be the keys used in the Shiny backend
ords <- list(unifrac = ord_filt,
             wunifrac = ord_filt_w,
             bray = ord_filt_b)


# Shiny application -------------------------------------------------------

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    titlePanel("Highlights all samples from given subject"),

    # Allow user to type in subject Id of interest
    textInput(
      inputId = "text",
      label = h3("Subject ID"),

      # Get random subject to visit initially
      value = psm_filt %>%
        filter(host_species == "Human") %>%
        distinct(subject_id) %>%
        sample_n(1) %>%
        pull()
    ),

    # Allow user to switch between unweighted and weighted UniFrac
    radioButtons(
      inputId = "radio",
      label = h3("Distance measure"),
      choices = list(
        "Unweighted UniFrac" = "unifrac",
        "Weighted UniFrac" = "wunifrac",
        "Bray-Curtis" = "bray"
      ),
      selected = "unifrac"
    ),

    # Render table with list of highlighted subjects and sample sums
    h3("Sample ID and sample sums"),
    dataTableOutput("table")
  ),
  mainPanel(# Ordination plot
    plotOutput("plot", height = "800px"))
))

server <- function(input, output, session) {
  # Ordination plot
  output$plot <- renderPlot({
    ps %>%
      plot_ordination(ords[[input$radio]],
                      type = "samples",
                      color = "sample_origin") +
      gghighlight(subject_id == input$text,
                  label_key = sequence_id)
  })

  # Subject sample list and sample sum table
  output$table <-
    renderDataTable({
      sample_abund %>%
        dplyr::filter(subject_id == input$text) %>%
        dplyr::select(-subject_id) %>%
        dplyr::arrange(desc(Total))
    }, options = list(dom = 't'))
}

shinyApp(ui, server)

## global.R  –  loaded ONCE at app startup, shared across all sessions
## (Shiny sources this file before ui.R / server.R, in the global environment)

# ── Packages ──────────────────────────────────────────────────────────────────
library(shiny)
library(magrittr)
library(SHAPforxgboost)

# ── Analysis function scripts ─────────────────────────────────────────────────
# Sourced once here so all helper functions are available to every session
# without re-parsing these files on every new connection.
source('www/functions/dataCheck-page.R')
source('www/functions/lipidNameConversion.R')
source('www/functions/utils-dataCheck.R')
source('www/functions/char.tab.url.R')
source('www/functions/dataCheck-otherpage.R')
source('www/functions/dataSummary.R')
source('www/functions/reactome_network.R')
source('www/functions/get_reactome_path.R')
source('www/functions/lipid_related_gene_network.R')
source('www/functions/function_code.R')
source('www/functions/annotation_human.R')

# ── Network mapping tables (lazy-loaded) ──────────────────────────────────────
# These ~9 MB of RDS files are only needed when a user visits the Network tab.
# We defer the disk reads until first access so app cold-start is not penalised.
# All sessions share the same in-memory copies once loaded (copy-on-modify semantics).

.net_cache <- new.env(parent = emptyenv())
.net_cache$loaded <- FALSE

.load_net_data <- function() {
  if (!.net_cache$loaded) {
    .net_cache$NET_lipid_gene_path     <- readRDS('www/mapping_table/lipid_gene_path.rds')
    .net_cache$NET_all_interaction     <- readRDS('www/mapping_table/reactome_all_interaction.rds')
    .net_cache$NET_all_node            <- readRDS('www/mapping_table/reactome_all_node.rds')
    .net_cache$NET_complex             <- readRDS('www/mapping_table/reactome_complex.rds')
    .net_cache$NET_edge                <- readRDS('www/mapping_table/reactome_edge.rds')
    .net_cache$NET_node                <- readRDS('www/mapping_table/reactome_node.rds')
    .net_cache$NET_node_id_for_complex <- readRDS('www/mapping_table/reactome_node_id_for_complex.rds')
    .net_cache$NET_uniprot             <- readRDS('www/mapping_table/uniprot.rds')
    .net_cache$loaded <- TRUE
  }
  invisible(.net_cache)
}

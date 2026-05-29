library(shiny)
library(SHAPforxgboost)

## ── Global options (set once at startup, shared across all sessions) ──────────
options(shiny.maxRequestSize = 30 * 1024^2)   # allow up to 30 MB file uploads

## ── External analysis scripts (keep absolute paths as-is) ────────────────────
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

shiny::shinyServer(function(input, output, session) {

  #### shinyhelper control ####
  shinyhelper::observe_helpers()

  ## ── Merged reactive variables from all modules ────────────────────────────
  variables <- shiny::reactiveValues(

    ## ── DataCheck ──────────────────────────────────────────────────────────
    Check.type             = "Profiling",
    Data.check.progress    = htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/2) Check upload data format.</h4></div>'),
    Check.Ngroups          = NULL,
    Check.abundance        = NULL,
    Check.group.info       = NULL,
    Check.cond.tab         = NULL,
    Check.adj.tab          = NULL,
    Check.web              = NULL,

    ## ── Profiling ──────────────────────────────────────────────────────────
    PRO.exp.user.col1        = NULL,
    PRO.lipid.char.user.col1 = NULL,
    PRO.pca.result           = NULL,
    PRO.tsne.result          = NULL,
    PRO.umap.result          = NULL,
    PRO.corr.heatmap.result  = NULL,

    ## ── Differential Expression ────────────────────────────────────────────
    DE.data.check.progress               = htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/3) Check data frame format.</h4></div>'),
    DE.SE                                = NULL,
    DE.SE.list                           = NULL,
    DE.raw.abundance                     = NULL,
    DE.group.info.check                  = NULL,
    DE.group.info                        = NULL,
    DE.Ngroup                            = NULL,
    DE.ref.group                         = NULL,
    DE.exclude.ref.group                 = NULL,
    DE.all.group.name                    = NULL,
    DE.group.compare.message             = NULL,
    DE.check.step1                       = NULL,
    DE.check.step2                       = NULL,
    DE.processed.SE                      = NULL,
    DE.processed.SE.list                 = NULL,
    DE.processed.abundance               = NULL,
    DE.processed.lipid.char.tab          = NULL,
    DE.processed.plot                    = NULL,
    DE.data.summary.div                  = NULL,
    DE.species.lipid.char                = NULL,
    DE.processed.plot.download.log       = 1,
    DE.class.char.twoWayAnova            = NULL,
    deSp.se                              = NULL,
    deSp.plot                            = NULL,
    DE.species.tab.all                   = NULL,
    DE.species.dotchart.download.log     = 1,
    DE.species.maplot.download.log       = 1,
    DE.species.boxplot.download.log      = 1,
    DE.species.lipid.boxplot             = NULL,
    DE.species.enrichment.download.log   = 1,
    DE.species.network.download.log      = 1,
    DE.species.pca.result                = NULL,
    DE.species.dim.redu.pca.download.log     = 1,
    DE.species.dim.redu.pca.topN.download.log = 1,
    DE.species.plsda.result              = NULL,
    DE.species.dim.redu.plsda.download.log = 1,
    DE.species.tsne.result               = NULL,
    DE.species.dim.redu.tsne.download.log = 1,
    DE.species.umap.result               = NULL,
    DE.species.hclustering               = NULL,
    DE.species.clustering.download.log   = 1,
    DE.species.char.association          = NULL,
    DE.species.lipid.char.download.log   = 1,
    DE.class.lipid.char                  = NULL,
    DE.class.char                        = NULL,
    DE.class.char.tab                    = NULL,
    DE.class.sub.char                    = NULL,
    DE.class.sub.char.table              = NULL,
    DE.class.sub.char.plot               = NULL,
    DE.class.pca.result                  = NULL,
    DE.class.dim.redu.pca.download.log   = 1,
    DE.class.dim.redu.pca.topN.download.log = 1,
    DE.class.plsda.result                = NULL,
    DE.class.dim.redu.plsda.download.log = 1,
    DE.class.umap.result                 = NULL,
    DE.class.dim.redu.tsne.download.log  = 1,
    DE.class.tsne.result                 = NULL,
    DE.class.hclustering                 = NULL,
    DE.class.clustering.download.log     = 1,
    DE.class.twoCharHeatmap              = NULL,
    DE.class.twoChar.total.download.log  = 1,
    DE.class.twoChar.each.download.log   = 1,
    DE.class.twoCharHeatmap.total.char   = NULL,
    DE.class.twoChar.total.charFeature.download.log = 1,
    DE.class.twoCharHeatmap.each.char    = NULL,
    DE.class.twoChar.each.charFeature.download.log  = 1,
    DE.class.twoCharHeatmap.total.box    = NULL,
    DE.class.twoChar.total.boxplot.download.log = 1,
    DE.class.twoCharHeatmap.each.box     = NULL,
    DE.class.twoChar.each.boxplot.download.log  = 1,

    ## ── Enrichment ─────────────────────────────────────────────────────────
    deSp_se                          = NULL,
    nGroups                          = NULL,
    ORA.result                       = NULL,
    ORA.each.result                  = NULL,
    Enrichment.lsea.all.result       = NULL,
    Enrichment.lsea.each.result      = NULL,
    Enrichment.ORA.all.download.log  = 1,
    Enrichment.ORA.each.download.log = 1,
    Enrichment.LSEA.all.download.log = 1,
    Enrichment.LSEA.each.download.log = 1,

    ## ── Machine Learning ───────────────────────────────────────────────────
    ML.data.check.progress       = htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/3) Check upload data format.</h4></div>'),
    ML.check.step1               = NULL,
    ML.check.step2               = NULL,
    ML.raw.abundance             = NULL,
    ML.cond.tab                  = NULL,
    ML.processed.SE              = NULL,
    ML.processed.SE.list         = NULL,
    ML.processed.abundance       = NULL,
    ML.processed.lipid.char.tab  = NULL,
    ML.data.summary.div          = NULL,
    ML.model.SE                  = NULL,
    ML.ROC.result                = NULL,
    ML.PR.result                 = NULL,
    ML.ROC.PR.all.download.log   = 1,
    ML.ROC.PR.feature.download.log = 1,
    ML.evaluation.result         = NULL,
    ML.evalution.download.log    = 1,
    ML.probability.result        = NULL,
    ML.probability.download.log  = 1,
    ML.feature.result            = NULL,
    ML.feature.download.log      = 1,
    ML.shap.se                   = NULL,
    ML.shap.dependence.list      = NULL,
    ML.shap.plot.result          = NULL,
    ML.shap.download.log         = 1,
    ML.shap.force.plot.result    = NULL,
    ML.SHAP.force.download.log   = 1,
    ML.shap.dependence.plot.result = NULL,
    ML.SHAP.dependence.download.log = 1,
    ML.network.result            = NULL,
    ML.network.download.log      = 1,

    ## ── Correlation ────────────────────────────────────────────────────────
    CORR.data.check.progress         = htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/2) Check upload data format.</h4></div>'),
    CORR.SE                          = NULL,
    CORR.SE.list                     = NULL,
    CORR.raw.abundance               = NULL,
    CORR.condition.col               = NULL,
    CORR.raw.cond.tab                = NULL,
    CORR.adjusted.col                = NULL,
    CORR.raw.adj.tab                 = NULL,
    CORR.check.step1                 = NULL,
    CORR.processed.plot.download.log = NULL,
    CORR.species.corr.result         = NULL,
    CORR.species.corr.download.log   = 0,
    CORR.species.linear.result       = NULL,
    CORR.species.linear.download.log = 0,
    CORR.class.corr.result           = NULL,
    CORR.class.corr.download.log     = 0,
    CORR.class.linear.result         = NULL,
    CORR.class.linear.download.log   = 0,

    ## ── Network ────────────────────────────────────────────────────────────
    NET.pathwayActivity.deSp.se  = NULL,
    NET.pathwayActivity.check    = NULL,
    NET.pathwayActivity          = NULL,
    NET.pathwayActivity.plot     = NULL,
    NET.lipidReaction.deSp.se    = NULL,
    NET.lipidReaction.check      = NULL,
    NET.lipidReaction            = NULL,
    NET.lipidReaction.plot       = NULL,
    NET.gatom.deSp.se            = NULL,
    NET.gatom.check              = NULL,
    NET.gatom                    = NULL,
    NET.gatom.plot               = NULL,

    ## ── ID Conversion ──────────────────────────────────────────────────────
    IDconversion.progress        = htmltools::HTML('<div style="font-size: 0px;"><h2>Upload progress</h2><h4>(1/2) Check upload data format.</h4></div>'),
    IDconversion.lipidList       = NULL,
    lipid_annotation_table       = NULL,
    nonParseableLipid            = NULL,

    ## ── DataCheck preload (set by .reupload() when errors occur in other tabs) ──
    Check.preload.type           = NULL   ## "Profiling" | "DE" | "ML" | "Correlation"

  ) ## reactiveValues

  ## ── Source all module server logic ────────────────────────────────────────

  ################################
  ######   DataCheck Page   ######
  ################################
  source(file = "server_DataCheck.R", local = TRUE, encoding = "UTF-8")

  ################################
  ######   Profiling Page   ######
  ################################
  source(file = "server_Profiling.R", local = TRUE, encoding = "UTF-8")

  #############################################
  ######   Differential Expression Page  ######
  #############################################
  source(file = "server_DE.R", local = TRUE, encoding = "UTF-8")

  ################################
  ######   Enrichment Page  ######
  ################################
  source(file = "server_Enrichment.R", local = TRUE, encoding = "UTF-8")

  #######################################
  ######   Machine Learning Page   ######
  #######################################
  source(file = "server_ML.R", local = TRUE, encoding = "UTF-8")

  ##################################
  ######   Correlation Page   ######
  ##################################
  source(file = "server_Correlation.R", local = TRUE, encoding = "UTF-8")

  ##############################
  ######   Network Page   ######
  ##############################
  source(file = "server_Network.R", local = TRUE, encoding = "UTF-8")

  ####################################
  ######   ID Conversion Page   ######
  ####################################
  source(file = "server_IDconversion.R", local = TRUE, encoding = "UTF-8")

}) ## shinyServer

library(shiny)
library(shinyWidgets)
# library(ggvis)
# library(shinycssloaders)
# library(highcharter)
# 
# 
# library(pacman)
# library(GGally)
# library(cowplot)
# library(broom)
# library(Epi)
# library(jsonlite)
library(shinyjs)
source("waterfall.source.r")
source("CIBERSORT.R")
library(dplyr)
library(plyr)
library(ggvis)
library(survival)
library(shiny)
library(shinycssloaders)
library(highcharter)


library(data.table)
library(pacman)
library(plotly)
library(GGally)
library(cowplot)
library(broom)
library(Epi)
library(stringr)
library(varhandle)
library(jsonlite)
library(deconstructSigs)
library(DT)
library(Seurat)
library(ggplot2)
library(gridExtra)
library(grid)

  database.test <- readRDS("finish2.rds")
 #database.test <- readRDS("finishe2.TCGA.rds")
 all.mutations <- readRDS("all.mutations.simple.simple.rds")
 #load("start.tablebubian.RData")
 start.table <- readRDS("start.table.rds")
 Moshe_genes <- readRDS("Moshe_genes.rds")
 Krieg_panel_1_genes <- readRDS("Krieg_panel_1_genes.rds")
 Krieg_panel_2_genes <- readRDS("Krieg_panel_2_genes.rds")
 Krieg_panel_3_genes <- readRDS("Krieg_panel_3_genes.rds")
 each.cell <- readRDS("each.cell.rds")
# seurat_Moshe <- readRDS("Moshe.nocounts.rds")
# seurat_Krieg_panel_1 <- readRDS("Krieg_panel_1.nocounts.rds")
# seurat_Krieg_panel_2 <- readRDS("Krieg_panel_2.nocounts.rds")
# seurat_Krieg_panel_3 <- readRDS("Krieg_panel_3.nocounts.rds")
# marker_Moshe <- readRDS("Moshe_markers.rds")
# marker_Krieg_panel_1 <- readRDS("Krieg_panel_1_markers.rds")
# marker_Krieg_panel_2 <- readRDS("Krieg_panel_2_markers.rds")
# marker_Krieg_panel_3 <- readRDS("Krieg_panel_3_markers.rds")
# #load("test.RData")




library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(stringr)   
library(data.table)
library(network)
library(igraph)



#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#----------------------------------------------module----------------------------------------


NumberUI <- function(id){
  ns <- NS(id)
  textInput(ns("obs"), "NODE_entry:")
}

Number <- function(input, output, session){}


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#----------------------------------------------UI--------------------------------------------


# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Topological Relationships of Spaces"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs -------------------------------------------------------------------
    sidebarPanel(
      
      
      downloadButton("downloadData", "Download Sample"),
      br(""),
      # Input: Select a file ---- (1)
      fileInput("file1", "Upload Edgelist CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      #-------------------------------------------------------------
      
      # Input Layout 
      radioButtons("lay", "Layout",
                   c("Tree" = "tree",
                     "Radial Tree" = "rtree",
                     "Kamada Kawaii" = "kk",
                     "Fruchterman-Reingold" = "fr")),
      
      #Input Root Node
      textInput("root", "Root Node: "),
      
      
      # Input Centrality
      radioButtons("cent", "Centrality Measure:",
                   c("None" = "none",
                     "Betweenness" = "btw",
                     "Closeness" = "cls",
                     "Degree" = "deg",
                     "Eigenvector" = "eig")),
      
      # Input Vertex Size
      numericInput('nodesize', 'Node Size (%)', 100, min = 5, max = 2000),
      
      # Input label Size
      numericInput('labelsize', 'Label Size (%)', 100, min = 1, max = 200),
      
      #-------------------------------------------------------------      
      br(),
      
      #Input Colour Nodes
      numericInput("numInputs", "No. of Node Categories: ", 1 , min = 1 , max = 6),
      uiOutput("inputGroup"),
      
      br(),
      
      #Input Colour Edges
      textInput("edgeblock", "EDGE_entry (non-zero columns): ", "Staircase"),
      
      #------------------------------------------------------------- 
      
      
    ),
    
    # Main panel for displaying outputs -----------------------------------------------------------------  
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("JGraph Plot", plotOutput("plot")),
                  tabPanel("NodeList", tableOutput("node")),
                  tabPanel("EdgeList", tableOutput("edge"))
      )
      
    )
  )
)



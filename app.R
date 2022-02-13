#my_packages = c("shiny", "dplyr","tidyverse","ggplot2","reshape2","stringr","data.table","network","igraph","googlesheets4")

#install_if_missing = function(p) {
#  if (p %in% rownames(installed.packages()) == FALSE) {
#    install.packages(p)
#  }
#}

#invisible(sapply(my_packages, install_if_missing))

library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(stringr)   
library(data.table)
library(network)
library(igraph)
#library(googlesheets4)


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




#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#----------------------------------------------server----------------------------------------


# Define server logic for random distribution app ----
server <- function(input, output, session) {
  
  # observe changes in "numInputs", and create corresponding number of inputs
  observeEvent(input$numInputs, {
    
    LL <- vector("list", input$numInputs)
    output$inputGroup = renderUI({
      input_list <- lapply(1:input$numInputs, function(i) {
        inputName <- paste0("input", i)
        LL[[i]]<- list(NumberUI(inputName)) 
      })
    })
    
    #run module
    lapply(1:input$numInputs, function(i) {
      inputName <- paste0("input", i)
      callModule(Number, inputName)
    })
  })
  
  # this is to display all the input values which I want, this doesn't work
  #output$test <- renderPrint({
  #  paste(lapply(1:input$numInputs, function(i) {
  #    inputName <- paste0("input", i, "-obs")
  #    input[[inputName]]
  #  }))
  #})

  
  node_block <- reactive({
    paste(lapply(1:input$numInputs, function(i) {
      inputName <- paste0("input", i, "-obs")
      mylist <- input[[inputName]]
    }))
  })

  output$test <- renderPrint({
    paste0(length(node_block()) - sum(node_block()== ""))
  })
  
  
  #1
  #----edgelist-----------------------------------------
  
  edgelist <- reactive({
    req(input$file1)
    return(read.csv(input$file1$datapath))
    })

  
  
  #2
  #-----graph stuff-----------------------------------------

  
  g <- reactive({
    
    g1 = graph_from_data_frame(d = edgelist(),directed = FALSE)
    
  
    # ----Edge
    E(g1)$color <- 'black'

    if(input$edgeblock %in% colnames(edgelist())){ #test if input exist as a column
      
      #change edge colour
      command <- paste0("E(g1)$color[E(g1)$",input$edgeblock," != 0] <- 'orange'")
      eval(parse(text = command))
    }
    
    
    #----node
    #color palette
    color = c("red","mediumpurple3","deepskyblue3","orange","hotpink2","olivedrab3")
    
    #Node Colors
    no = length(node_block()) - sum(node_block()== "")
  
    V(g1)$color<-"lightgrey"
    if (no > 0) {
      for (i in 1:no){
        index = grep(node_block()[i],V(g1)$name)
        V(g1)[index]$color<-color[i]
      }
    }
    
    return(g1)
  })  
  
  
  
  #3
  #-----layout stuff---------------
  
  l <- reactive({
    
    if (input$root %in% centrality()$Name){
      layout <- switch(input$lay,
                       tree = layout_as_tree(g(), root = input$root, flip.y = TRUE, circular = FALSE),
                       rtree = layout_as_tree(g(), root = input$root, flip.y = TRUE, circular = TRUE) ,
                       kk = layout_with_kk(g()),
                       fr = layout_with_fr(g()))
    }
    else{
      layout <- switch(input$lay,
                       tree = layout_as_tree(g(), flip.y = TRUE, circular = FALSE),
                       rtree = layout_as_tree(g(), flip.y = TRUE, circular = TRUE) ,
                       kk = layout_with_kk(g()),
                       fr = layout_with_fr(g()))
    }
    
    layout
    #layout_as_tree(g, flip.y = TRUE, root = input$root, circular = TRUE)
  })
  
  size <- reactive({
    if (input$cent != "none"){
    
      maxsize <- 8 /100 *input$nodesize #calculate scaling from vertex size 3
      
      max.btw <- max(centrality()$Betweenness)
      max.cls <- max(centrality()$Closeness)
      max.deg <- max(centrality()$Degree)
      max.eig <- max(centrality()$Eigenvector)
      
      scale.btw <- maxsize/(max.btw+0.03)
      scale.cls <- maxsize/(max.cls^3)
      scale.deg <- maxsize/max.deg
      scale.eig <- maxsize/(max.eig^0.8)
      
      #nsize.btw = setNames((centrality()$Betweenness+0.03)*100 ,centrality()$name)
      
      nsize.btw <-setNames((centrality()$Betweenness+0.03)*scale.btw ,centrality()$name)
      nsize.cls <-setNames( ((centrality()$Closeness)^3)*scale.cls ,centrality()$name)
      nsize.deg <-setNames(centrality()$Degree*scale.deg ,centrality()$name)
      nsize.eig <-setNames( ((centrality()$Eigenvector)^0.8)*scale.eig ,centrality()$name)
      
      
      s <- switch(input$cent,
                     btw = nsize.btw,
                     cls = nsize.cls,
                     deg = nsize.deg,
                     eig = nsize.eig)


      return(s)
    }
    else{
      return(5 /100 *input$nodesize)
    }
    
  })
    
  
  
  #4
  #-----final plot formula---------------
  
  p <- reactive({
    plot(g(),layout = l(), vertex.label.cex = 1*input$labelsize/100, vertex.size = size(), vertex.label.color = "grey17") 
  })  
    
  
  
  
  #5
  #-----Centrality------------------------------------------------
  centrality <- reactive ({
    #extract centrality measures     
    btw = betweenness(g(), v = V(g()), directed = FALSE, weights = NULL,
                      nobigint = TRUE, normalized = TRUE)
    cls = closeness(g(), v = V(g()), weights = NULL, normalized = TRUE)
    deg = degree(g())
    eig = evcent(g())$vector
    
    #create dataframe
    btw_df = as.data.frame(btw)
    cls_df = as.data.frame(cls)
    deg_df = as.data.frame(deg)
    eig_df = as.data.frame(eig)
    
    #bind dataframes
    cent_df = setNames(cbind(rownames(btw_df), btw_df, cls_df, deg_df, eig_df, row.names = NULL), 
                       c("Name", "Betweenness", "Closeness","Degree","Eigenvector"))
    return(cent_df)
  })
  
  
  
  #6  !!! OUTPUT VISUALS
  #------------------------------------------------------------Plot    
  
  output$plot <- renderPlot({
    req(input$file1)
    p()
  },  width = 1280, height = 960)
  
  #------------------------------------------------------------NodeList  
  
  # Generate a summary of the data ----
  output$node <- renderTable({
    centrality()
  })
  
  #------------------------------------------------------------EdgeList
  # Generate an HTML table view of the data ----
  output$edge <- renderTable({
    edgelist()
  })

  
}



#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#----------------------------------------------Create----------------------------------------
shinyApp(ui, server)

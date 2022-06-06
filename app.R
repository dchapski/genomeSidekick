library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(ggrepel)
library(plotly)
library(magrittr)
library(htmlwidgets)
library(gprofiler2)
library(data.table)
library(stringi)
library(httr)
library(jsonlite)
library(tidyverse)
library(easyPubMed)

# Define UI
ui <- shinyUI(
  
  navbarPage("genomeSidekick: A data analysis application",
             tags$a(href="https://github.com/dchapski/genomeSidekick", "Test data can be found in test_data directory here"),
             
  # Define file upload page  
  tabPanel("Data File Upload (RNA Seq)",
    h2("RNA-seq Data Uplaod"),
    p("Your data file must be in .csv format. For instructions on how to convert an Excel or .txt file into a .csv file, 
    	consult the documentation."),
    fileInput('RNA_upload', 'Choose file to upload', accept=c('text/csv', 'text/comma-separated-values', '.csv')),
    radioButtons("separator_RNA", "Separator: ", choiceValues=c("\t", ",", ";"),
                 selected=",", inline=TRUE, choiceNames=c("Tab", "Comma", "Semicolon")),
    DT::dataTableOutput("sample_table")
  ),

  #define interactive volcano plot page 
  tabPanel("Interactive Volcano Plot (RNA Seq)",
           shinyjs::useShinyjs(),
           pageWithSidebar(
             headerPanel('Volcano Plot'),
             sidebarPanel(
               
               #inputs for the plot
               selectInput('names', 'Gene Names', ""),
               selectInput('x', 'Log2FoldChange', ""),
               selectInput('y', 'Padj', ""),
               sliderInput('FCCutoff', label="Choose Log2FoldChange Threshold", 
                           min=0, max=5, value=0.6, step=0.1),
               sliderInput('PadjCutoff', label="Choose Padj Threshold", 
                           min=0, max=0.1, value=0.05, step=0.001),
               actionButton(inputId="go", label="Submit")
             ),
             mainPanel(
               h3("Genome Browser View"),

               #link to UCSC genome browser
               "To view your data in a genome browser, visit the ",
               a(href="https://genome.ucsc.edu/", "UCSC Genome Browser"), " and upload your processed data file.",

               h3("Click on point to search in NCBI/Uniprot"),
               
               #database used for gene search
               selectInput('SearchDB', 'Choose Your Database for Searching', 
                           c("NCBI", "Uniprot", "Human Protein Atlas"), selected="NCBI"),
               selectizeInput ("InputGene", "Input Gene's Name to Search on Plot", "",
                                    multiple=TRUE),
               
               strong("Most Recent Papers from "),
               a(href="https://pubmed.ncbi.nlm.nih.gov/", "PubMed"),
               strong(":"),

               DT::dataTableOutput("results_table"),
               actionButton(inputId="reset", label="Reset"),

               plotlyOutput('volplot'),
               plotOutput('volplot_static')
             )
           )
           ),
  
  #define Top Differentially Expressed Genes page
  tabPanel("Top Differentially Expressed Genes",
  		   h2("Differentially Expressed Genes Data Tables"),
           'You may download the tables displayed below as a CSV (comma-separated values) file or a TSV (tab-separated values) file.',
           selectInput("Download", "Choose a dataset:",
                       choices=c("UpDEG", "DownDEG")),
           radioButtons("filetype", "File type:",
                        choices=c("csv", "tsv")),
           #download Top Differentially Expressed Genes
           downloadButton("downloadData", "Download"),
           h2("Upregulated Genes", align="center"),
           DT::dataTableOutput("UpDEG"),
           h2("Downregulated Genes", align="center"),
           DT::dataTableOutput("DownDEG")
           ),
  #define Data File Upload (ATAC Seq) page
  tabPanel("Data File Upload (ATAC Seq)",
    h2("ATAC-seq Data Upload"),
    p("Your data file must be in .csv format. For instructions on how to convert an Excel or .txt file into a .csv file, 
    	consult the documentation."),
    p("Please note: you can also upload ChIP-seq data here and perform the same visualization with the volcano plot as long as 
    	the table contains the necessary columns (gene name, p-value, and log2FoldChange)."),
    fileInput('ATAC_upload', 'Choose file to upload', accept=c('text/csv', 'text/comma-separated-values', '.csv')),
    radioButtons("separator_ATAC", "Separator: ", choiceValues=c("\t", ",", ";"),
                 selected=",", inline=TRUE, choiceNames=c("Tab", "Comma", "Semicolon")),
    DT::dataTableOutput("sample_table_ATAC")
  ),
  
  #define Interactive Volcano Plot (ATAC Seq) page
  tabPanel("Interactive Volcano Plot (ATAC Seq)",
           shinyjs::useShinyjs(),
           pageWithSidebar(
             headerPanel('Volcano Plot'),
             sidebarPanel(
               #define inputs
               selectInput('names_ATAC', 'Gene Names', ""),
               selectInput('x_ATAC', 'Log2FoldChange', ""),
               selectInput('y_ATAC', 'Padj', ""),
               sliderInput('FCCutoff_ATAC', label="Choose Log2FoldChange Threshold", 
                           min=0, max=5, value=0.6, step=0.1),
               sliderInput('PadjCutoff_ATAC', label = "Choose Padj Threshold", 
                           min=0, max=0.1, value=0.05, step=0.001),
               actionButton(inputId="go_ATAC", label="Submit")
             ),
             mainPanel(
               h3("Genome Browser View"),

               #link to UCSC genome browser
               "To view your data in a genome browser, visit the ",
               a(href="https://genome.ucsc.edu/", "UCSC Genome Browser"), " and upload your processed data file.",

               h3("Click on point to search in NCBI/Uniprot"),
               
               #database used for gene search
               selectInput('SearchDB_ATAC', 'Choose Your Database for Searching', 
                           c("NCBI", "Uniprot", "Human Protein Atlas"), selected="NCBI"),
               selectizeInput ("InputGene_ATAC", "Input Gene's Name to Search on Plot", "",
                               multiple=TRUE),

               strong("Most Recent Papers from "),
               a(href="https://pubmed.ncbi.nlm.nih.gov/", "PubMed"),
               strong(":"),

               DT::dataTableOutput("results_ATAC_table"),
               actionButton(inputId = "reset_ATAC", label="Reset"),
               #div(style="display:inline-block",actionButton(inputId = "searchGene", label = "Search")),
               plotlyOutput('volplot_ATAC'),
               plotOutput('volplot_static_ATAC')
             )
           )
  ),
  
  #define Integrate RNA and ATAC Seq page
  tabPanel("Integrate RNA and ATAC Seq",
    radioButtons("basegraph", "Base graph: ", choiceValues=c("RNA", "ATAC"),
                 selected="", inline=TRUE, choiceNames=c("RNA Seq", "ATAC Seq")),
    h3("Click on point to search in NCBI/Uniprot"),
    selectInput('SearchDB_Integrate', 'Choose Your Database for Searching', 
                c("NCBI", "Uniprot", "Human Protein Atlas"), selected="NCBI"),
    plotlyOutput('volplot_Integrate')
  ),
  
  #define g:Profiler GO Analysis page
  tabPanel("g:Profiler GO Analysis",
           pageWithSidebar(
             headerPanel('g:Profiler Parameters'),
             sidebarPanel(
               #define inputs
               selectInput('gPInput', 'Choose Input', 
                           c("Upregulated Genes", "Downregulated Genes", "Both"), 
                           selected="Upregulated Genes"),
               selectInput('organism', 'Choose Organism', 
                           c("Human", "Mouse", "Rat"), 
                           selected="Human"),
               checkboxInput("orderedQuery", 
                             label="Ordered Query", 
                             value=TRUE),
               bsTooltip(id="orderedQuery", 
                         title="Input is ordered by importance in decreasing manner",
                         placement="right", trigger="hover"),
               selectInput('StatsDomain', 'Statistical Domain Scope', 
                           c("Only annotated genes", "All known genes"), 
                           selected="Only annotated genes"),
               bsTooltip(id="StatsDomain", 
                         title="Choose effective genomic landscape for stats testing",
                         placement="right", trigger="hover"),
               selectInput('Threshold', 'Significance Threshold', 
                           c("g:SCS Threshold", "Bonferroni correction", "FDR"), 
                           selected="g:SCS Threshold"),
               bsTooltip(id="Threshold", 
                         title="Multiple testing correction method",
                         placement="right", trigger="hover"),
               selectInput('sortBy', 'Top Significant Genes Based on', 
                           c("-Log10PValue", "Log2FoldChange"), 
                           selected="Log2FoldChange"),
               sliderInput('topGenes', label="Number of Top Genes Included", 
                           min=0, max=500, value=50, step=10),
               actionButton(inputId="GPgo", label="Submit")
           ),
           mainPanel(
             uiOutput("tab"),
             plotlyOutput('gProfile_plot'),
             plotOutput('gProfile_table')
           )
           )
  )
)) 

# Define server logic
server <- shinyServer(function(input, output, session) {
  #RNA Seq data table
  df_RNA_Seq <- reactive({
    inFile <- input$RNA_upload
    if(is.null(inFile)) return(NULL)
    #read file
    df <- read.csv(inFile$datapath, header=TRUE, sep=input$separator_RNA)
    updateSelectInput(session, inputId='x', label='Log2FoldChange',
                      choices=colnames(df), selected="log2FC.TAC3w.over.CTRL")
    updateSelectInput(session, inputId='y', label='Padj',
                      choices=colnames(df), selected="padj.TAC3w.over.CTRL")
    updateSelectInput(session, inputId='names', label='Gene Names',
                      choices=colnames(df), selected="gene_name")
    return(df)
  })
  
  output$sample_table <- DT::renderDataTable({
    DT::datatable(df_RNA_Seq())
  })
  
  #RNA seq Differentially expressed genes
  df_DEG <- eventReactive (input$go,{
    df <- df_RNA_Seq()
    df$diffexpressed <- "NO"
    df <- within(df, diffexpressed[get(input$x) > as.numeric(input$FCCutoff) & 
                                   get(input$y) < as.numeric(input$PadjCutoff)] <- "UP")
    df <- within(df, diffexpressed[get(input$x) < -as.numeric(input$FCCutoff) & 
                                   get(input$y) < as.numeric(input$PadjCutoff)] <- "DOWN")
    # write down the name of genes beside the points
    df$delabel <- NA
    df <- within(df, delabel <- get(input$names))
    updateSelectizeInput(session, inputId="InputGene", 
                         label="Input Gene's Name to Search on Plot",
                         choices=df$delabel, selected=NULL, server=TRUE)
    return(df)
  })
  
  #search function in volcano plot
  rv <- reactiveValues(a=0)
  toListen1 <- reactive({
    list(input$go, input$reset, input$InputGene)
  })
  toListen2 <- reactive({
    list(input$go, input$reset)
  })
  
  observeEvent(toListen2(),{
    shinyjs::reset("InputGene") 
  })
  
df_DEG_Plot <- eventReactive(toListen1(), {
    if(is.null(input$InputGene)){
      df <- df_DEG()
      df$size <- 2
    }
    else {
      df <- df_DEG()
      df$diffexpressed <- "Non-Target"
      df <- within(df, diffexpressed[match(input$InputGene, delabel)] <- "Target")
      df$size <- 2
      df$size[which(df$diffexpressed == "Target")] <- 8
      df$size <- as.numeric(df$size)
    }
    return(df)
  })

#Generate Upregulated DEG table on abs(log2FoldChange)   
 df_UpDEG <- reactive({
    df <- df_DEG()
    df <- df[df$diffexpressed == "UP",]
    df <- df %>%
      select(paste0(input$names), paste0(input$x), paste0(input$y),everything())
    df <- select(df, -c(diffexpressed, delabel))
    df <- df[order(-df[, 2]),]
    return(df)
  })
  
 output$UpDEG <- DT::renderDataTable({
   DT::datatable(df_UpDEG())
 })

 #Generate Downregulated DEG table on abs(log2FoldChange)
 df_DownDEG <- reactive({
   df <- df_DEG()
   df <- df[df$diffexpressed == "DOWN",]
   df <- df %>%
     select(paste0(input$names), paste0(input$x), paste0(input$y),everything())
   df <- select(df, -c(diffexpressed, delabel))
   df <- df[order(df[,2]),]
   return(df)
 })
 
 #Generate DEG for both Up&Down, sort based on abs(log2FoldChange)
 df_BothDEG <- reactive({
   df <- df_DEG()
   df <- df[df$diffexpressed != "NO",]
   df <- df %>%
     select(paste0(input$names), paste0(input$x), paste0(input$y), everything())
   df <- select(df, -c(diffexpressed, delabel))
   df[, 2] <- abs(df[, 2])
   df <- df[order(-df[, 2]),]
   return(df)
 })

 output$DownDEG <- DT::renderDataTable({
   DT::datatable(df_DownDEG())
 })

#generate pubmed search results
 search_gene_PubMed <- eventReactive(toListen1(), {
    if(is.null(input$InputGene) == TRUE){
      results <- NULL
    } else {
      my.queries <- unlist(strsplit(input$InputGene, split=' '))
      results <- NULL
      
      # Define function to get XML, convert result to list
      getXmlAndList <- function(x, maxResults=5) {
        abstract.xml <- fetch_pubmed_data(x, retmax=maxResults)
        articles_to_list(pubmed_data=abstract.xml)
      }
      
      # Define function to return desired columns from 1st result of article_to_df()
      returnPaperData <- function(x, maxResults=5) {
        article_to_df(x)[1, c("pmid", "year", "title", "firstname", "lastname", "abstract")]
      }
      
      for(i in my.queries) {
        my.entrez.id <- get_pubmed_ids(i)
        null.df <- data.frame(pmid=NA, year=NA, title=NA, firstname=NA, lastname=NA, abstract=NA, query_gene=i)
        if(length(my.entrez.id$IdList) == 0) {
          results <- rbind(results, null.df)
        } else {
          pm.list <- getXmlAndList(my.entrez.id, maxResults=5)
          pm.df <- do.call(rbind, lapply(pm.list, returnPaperData))
          pm.df$query_gene <- i
          results <- rbind(results, pm.df)
        } # end if statement
      } # end i loop

      return(results)
    } # End else from if statement under # generate pubmed search results
	})

output$results_table <- DT::renderDataTable({
    DT::datatable(search_gene_PubMed())
  })
 
 #Generate volcano plot for RNA-Seq
  output$volplot <- renderPlotly({
    df <- df_DEG_Plot()
    mycolors <- c("blue", "grey", "red") # Point colors
    if(is.null(input$InputGene)){
      volplot <- plot_ly(data=df, x=~df[, input$x], y=~-log10(df[, input$y]), 
                       color=~diffexpressed, colors=mycolors, hoverinfo='text',
                       mode="markers", marker=list(size=4), 
                       #hovering text
                       text=~paste("Gene_name:", delabel, "<br>",
                                   "-Log10Padj:", -log10(df[, input$y]), "<br>",
                                   "Log2FoldChange:", df[, input$x]))
    } else {
      volplot <- plot_ly(data=df, x=~df[, input$x], y=~-log10(df[, input$y]), 
                       color=~diffexpressed, colors=mycolors, hoverinfo='text',
                       mode="markers" , size=~size, 
                       #hovering text
                       text=~paste("Gene_name:", delabel, "<br>",
                                   "-Log10Padj:", -log10(df[, input$y]), "<br>",
                                   "Log2FoldChange:", df[, input$x]))
    }

  #format axes
    f1 <- list(size=18, color="black")
    x <- list(title="Log2FoldChange", titlefont=f1, tickfont=f1)
    y <- list(title="-log10(Padj)", titlefont=f1, tickfont=f1)
    l <- list(title=list(text='<b> Click to Hide Group </b>'),
              font=list(font=list(size=100, color="#000")))
    
    volplot <- volplot %>% layout(xaxis=x, yaxis=y, legend=l)
  
  #add NCBI click action
    DBAddress <- paste0("https://www.ncbi.nlm.nih.gov/search/all/?term=", df$delabel)
    if(input$SearchDB == "Human Protein Atlas") {
      DBAddress <- paste0("https://www.proteinatlas.org/search/", df$delabel)
    } else if(input$SearchDB == "Uniprot") {
      DBAddress <- paste0("https://www.uniprot.org/uniprot/?query=", df$delabel, "&sort=score")
    }
   volplot <- volplot %>% add_markers(customdata=DBAddress) 
   onRender(
     volplot, "
  function(el) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      window.open(url);
    });
  }
")

    
  })

  #static volplot for RNA-Seq
  output$volplot_static <- renderPlot({
    df <- df_DEG()
    mycolors <- c("blue", "red") # Point colors
    df_sub <- df[df$diffexpressed != "NO", ]
    x_max <- max(abs(df_sub[, input$x]))
    
    volplot <- ggplot(df_sub, aes(x=df_sub[, input$x], y=-log10(df_sub[, input$y]), color=diffexpressed)) + 
      geom_point() + ylim(0, NA) + xlim(-x_max, x_max) + theme_minimal() +
      geom_text_repel(data=df_sub, aes(label=delabel), size=3.5, 
                      max.overlaps=20, min.segment.length=0, box.padding= 0.5) + 
      scale_colour_manual(values=mycolors) +
      labs(x="Log2FoldChange", y="-log10(Padj)") +
      theme(axis.text.x=element_text(face="bold", size=12),
            axis.text.y=element_text(face="bold", size=12),
            legend.text=element_text(size = 12),
            legend.title=element_blank(),
            axis.title=element_text(face="bold", size=16))

    volplot
  })
  
  #Download DEG table
  datasetInput <- reactive({
    if(input$Download == "UpDEG") {
      return(df_UpDEG())
    } else {
      return(df_DownDEG())}
  })
  output$downloadData <- downloadHandler(
    filename=function() {
      paste(input$Download, input$filetype, sep=".")
    },
    content=function(file) {
      sep <- switch(input$filetype, "csv"=",", "tsv"="\t")
      # Write to a file specified by the 'file' argument
      write.table(datasetInput(), file, sep=sep, row.names=FALSE)
    }
  )
  
  #ATAC Seq data table
  df_ATAC_Seq <- reactive({
    inFile <- input$ATAC_upload
    if(is.null(inFile)) return(NULL)
    
    df <- read.csv(inFile$datapath, header=TRUE, sep=input$separator_ATAC)
    updateSelectInput(session, inputId='x_ATAC', label='Log2FoldChange', 
                      choices=colnames(df), selected="Fold.TAC3w")
    updateSelectInput(session, inputId='y_ATAC', label='Padj', 
                      choices=colnames(df), selected="FDR.TAC3w")
    updateSelectInput(session, inputId='names_ATAC', label='Gene Names',
                      choices=colnames(df), selected="gene_name")
    return(df)
  })
  
  output$sample_table_ATAC <- DT::renderDataTable({
    DT::datatable(df_ATAC_Seq())
  })
  
  #ATAC seq Differentially expressed genes
  df_DEG_ATAC <- eventReactive(input$go_ATAC, {
    df <- df_ATAC_Seq()
    df$diffexpressed <- "NO"
    df <- within(df, diffexpressed[get(input$x_ATAC) > as.numeric(input$FCCutoff_ATAC) & 
                                     get(input$y_ATAC) < as.numeric(input$PadjCutoff_ATAC)] <- "UP")
    df <- within(df, diffexpressed[get(input$x_ATAC) < -as.numeric(input$FCCutoff_ATAC) & 
                                     get(input$y_ATAC) < as.numeric(input$PadjCutoff_ATAC)] <- "DOWN")
    # write down the name of genes beside the points
    df$delabel <- NA
    df <- within(df, delabel <- get(input$names_ATAC))
    updateSelectizeInput(session, inputId="InputGene_ATAC", 
                         label="Input Gene's Name to Search on Plot",
                         choices=df$delabel, selected=NULL, server=TRUE)
    return(df)
  })
  
  #search function in volcano plot (ATAC)
  rv <- reactiveValues(a=0)
  toListen3 <- reactive({
    list(input$go_ATAC, input$reset_ATAC, input$InputGene_ATAC)
  })
  toListen4 <- reactive({
    list(input$go_ATAC, input$reset_ATAC)
  })
  
  observeEvent(toListen4(),{
    shinyjs::reset("InputGene_ATAC") 
  })
  
  df_DEG_Plot_ATAC <- eventReactive(toListen3(), {
    if(is.null(input$InputGene_ATAC)) {
      df <- df_DEG_ATAC()
      df$size <- 2
    } else {
      df <- df_DEG_ATAC()
      df$diffexpressed <- "Non-Target"
      df <- within(df, diffexpressed[match(input$InputGene_ATAC, delabel)] <- "Target")
      df$size <- 2
      df$size[which(df$diffexpressed == "Target")] <- 8
      df$size <- as.numeric(df$size)
    }
    return(df)
  })
  
  #generate pubmed search results
  search_gene_PubMed_ATAC <- eventReactive(toListen3(), {
    if(is.null(input$InputGene_ATAC)) {
      results_ATAC <- NULL
    } else {
	  my.queries <- unlist(strsplit(input$InputGene_ATAC, split=' '))
	  results_ATAC <- NULL

	  # Define function to get XML, convert result to list
	  getXmlAndList <- function(x, maxResults=5) {
	    abstract.xml <- fetch_pubmed_data(x, retmax=maxResults)
	    articles_to_list(pubmed_data=abstract.xml)
	  }

	  # Define function to return desired columns from 1st result of article_to_df()
	  returnPaperData <- function(x, maxResults=5) {
	    article_to_df(x)[1, c("pmid", "year", "title", "firstname", "lastname", "abstract")]
	  }

	for(i in my.queries) {
	  my.entrez.id <- get_pubmed_ids(i)
	  null.df <- data.frame(pmid=NA, year=NA, title=NA, firstname=NA, lastname=NA, abstract=NA, query_gene=i)
	  if(length(my.entrez.id$IdList) == 0) {
	    results_ATAC <- rbind(results_ATAC, null.df)
	  } else {
	    pm.list <- getXmlAndList(my.entrez.id, maxResults=5)
	    pm.df <- do.call(rbind, lapply(pm.list, returnPaperData))
	    pm.df$query_gene <- i
	    results_ATAC <- rbind(results_ATAC, pm.df)
	  } # end if statement
	} # end i loop

	return(results_ATAC)
	}
	})

output$results_ATAC_table <- DT::renderDataTable({
  DT::datatable(search_gene_PubMed_ATAC())
  })
 
  #Generate Upregulated DEG table ATAC, sort based on abs(log2FoldChange) 
  df_UpDEG_ATAC <- reactive({
    df <- df_DEG_ATAC()
    df <- df[df$diffexpressed == "UP", ]
    df <- df %>%
      select(paste0(input$names_ATAC), paste0(input$x_ATAC), paste0(input$y_ATAC), everything())
    df <- select(df, -c(diffexpressed, delabel))
    df <- df[order(-df[,2]), ]
    return(df)
  })
  
  output$UpDEG_ATAC <- DT::renderDataTable({
    DT::datatable(df_UpDEG_ATAC())
  })
  
  #Generate Downregulated DEG table ATAC, sort based on abs(log2FoldChange)
  df_DownDEG_ATAC <- reactive({
    df <- df_DEG_ATAC()
    df <- df[df$diffexpressed == "DOWN", ]
    df <- df %>%
      select(paste0(input$names_ATAC), paste0(input$x_ATAC), paste0(input$y_ATAC),everything())
    df <- select(df, -c(diffexpressed, delabel))
    df <- df[order(df[, 2]),]
    return(df)
  })
  
  #Generate DEG for both Up&Down, sort based on abs(log2FoldChange)
  df_BothDEG_ATAC <- reactive({
    df <- df_DEG_ATAC()
    df <- df[df$diffexpressed != "NO",]
    df <- df %>%
      select(paste0(input$names_ATAC), paste0(input$x_ATAC), paste0(input$y_ATAC), everything())
    df <- select(df, -c(diffexpressed, delabel))
    df[, 2] <- abs(df[, 2])
    df <- df[order(-df[, 2]),]
    return(df)
  })
  
  output$DownDEG_ATAC <- DT::renderDataTable({
    DT::datatable(df_DownDEG_ATAC())
  })
  
  #Generate volcano plot for ATAC
  output$volplot_ATAC <- renderPlotly({
    df <- df_DEG_Plot_ATAC()
    mycolors <- c("blue", "grey", "red") # Point colors
    if(is.null(input$InputGene_ATAC)) {
      volplot <- plot_ly(data=df, x=~df[,input$x_ATAC], y=~-log10(df[,input$y_ATAC]), 
                       color=~diffexpressed, colors=mycolors, hoverinfo='text',
                       mode="markers", marker=list(size=4),
                       #hovering text
                       text=~paste("Gene_name:", delabel, "<br>",
                                   "-Log10Padj:", -log10(df[, input$y_ATAC]), "<br>",
                                   "Log2FoldChange:", df[, input$x_ATAC]))
    } else {
      volplot <- plot_ly(data=df, x=~df[,input$x_ATAC], y=~-log10(df[,input$y_ATAC]), 
                       color=~diffexpressed, colors=mycolors, hoverinfo='text',
                       mode="markers" , size=~size, 
                       #hovering text
                       text=~paste("Gene_name:", delabel, "<br>",
                                   "-Log10Padj:", -log10(df[, input$y_ATAC]), "<br>",
                                   "Log2FoldChange:", df[, input$x_ATAC]))
    }

    #format axes
    f1 <- list(size=18, color="black")
    x <- list(title="Log2FoldChange", titlefont=f1, tickfont=f1)
    y <- list(title="-log10(Padj)", titlefont=f1, tickfont=f1)
    l <- list(title=list(text='<b> Click to Hide Group </b>'), 
              font=list(font=list(size=100, color="#000")))
    
    volplot <- volplot %>% layout(xaxis=x, yaxis=y, legend=l)
    
    #add NCBI click action
    DBAddress <- paste0("https://www.ncbi.nlm.nih.gov/search/all/?term=", df$delabel)
    if(input$SearchDB_ATAC == "Human Protein Atlas") {
      DBAddress <- paste0("https://www.proteinatlas.org/search/", df$delabel)
    } else if(input$SearchDB_ATAC == "Uniprot") {
      DBAddress <- paste0("https://www.uniprot.org/uniprot/?query=", df$delabel, "&sort=score")
    }
    volplot <- volplot %>%
      add_markers(customdata=DBAddress) 
    onRender(
      volplot, "
  function(el) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      window.open(url);
    });
  }
")
    
  })
  
  #static volplot for ATAC
  output$volplot_static_ATAC <- renderPlot({
    df <- df_DEG_ATAC()
    mycolors <- c("blue", "red") # Point colors
    df_sub <- df[df$diffexpressed != "NO", ]
    x_max <- max(abs(df_sub[, input$x_ATAC]))
    
    volplot <- ggplot(df_sub, aes(x=df_sub[, input$x_ATAC], y=-log10(df_sub[, input$y_ATAC]), color=diffexpressed)) + 
      geom_point() + ylim(0, NA) + xlim(-x_max, x_max) + theme_minimal() +
      geom_text_repel(data=df_sub, aes(label=delabel), size=3.5, 
                      max.overlaps=20, min.segment.length=0, box.padding=0.5) + 
      scale_colour_manual(values=mycolors) +
      labs(x="Log2FoldChange", y="-log10(Padj)") +
      theme(axis.text.x=element_text(face="bold", size=12),
            axis.text.y=element_text(face="bold", size=12),
            legend.text=element_text(size=12),
            legend.title=element_blank(),
            axis.title=element_text(face="bold", size=16))
    
    volplot
  })
  
  #Integrate RNA Seq and ATAC Seq
  toListen5 <- reactive({
    list(input$reset_Integrate, input$InputGene_Integrate, input$basegraph)
  })
  toListen6 <- reactive({
    list(input$reset_Integrate)
  })
  observeEvent(toListen6(),{
    shinyjs::reset("InputGene_Integrate") 
  })
  #Define the base plot Integrating RNA Seq and ATAC Seq
  df_DEG_Integrate <- eventReactive(input$basegraph, {
    if(input$basegraph == "RNA") {
      df_base <- df_DEG()
      df_mask <- df_DEG_ATAC()
      log2FC <- input$x_ATAC
      Padj <- input$y_ATAC
    } else if(input$basegraph == "ATAC") {
      df_base <- df_DEG_ATAC()
      df_mask <- df_DEG()
      log2FC <- input$x
      Padj <- input$y
    }
    
    df_base_sub <- df_base[df_base$diffexpressed != "NO", ]
    df_mask_sub <- df_mask[df_mask$diffexpressed != "NO", ]
    
    df_base_sub <- subset(df_base_sub, delabel %in% df_mask_sub$delabel)
    df_mask_sub <- subset(df_mask_sub, delabel %in% df_base_sub$delabel)
    
    df_mask_sub$Log2FC <- df_mask_sub[, log2FC]
    df_mask_sub$Minus_Log10Padj <- -log10(df_mask_sub[, Padj])
    df_mask_sub <- df_mask_sub %>% select(delabel, Log2FC, Minus_Log10Padj)
    
    df_base_sub <- left_join(df_base_sub, df_mask_sub, by="delabel")
    
    return(df_base_sub)
  })

  df_DEG_Plot_Integrate <- eventReactive(toListen5(), {
    if(is.null(input$InputGene_Integrate) == TRUE) {
      df <- df_DEG_Integrate()
    } else {
      df <- df_DEG_Integrate()
      df$diffexpressed <- "Non-Target"
      df <- within(df, diffexpressed[match(input$InputGene_Integrate, delabel)] <- "Target")
    }
    
    return(df)
  })

  #interactive volcanoplot for Integrating RNA Seq and ATAC Seq
  output$volplot_Integrate <- renderPlotly({
    df <- df_DEG_Plot_Integrate()
    if(input$basegraph == "RNA") {
      log2FC <- input$x
      Padj <- input$y
      t <- "Size and Color of the dots correspond to -log10Padj and log2FoldChange respectively from the ATAC Data."
    } else { 
      log2FC <- input$x_ATAC
      Padj <- input$y_ATAC
      t <- "Size and Color of the dots correspond to -log10Padj and log2FoldChange respectively from the RNA Data."
    }
    ## Change point color
    mycolors <- c("blue", "grey", "red")
    volplot <- plot_ly(data=df, x=~df[, log2FC], y=~-log10(df[,Padj]), 
                       color=~Log2FC, hoverinfo='text', mode="markers", 
                       size=~Minus_Log10Padj, 
                       #hovering text
                       text=~paste("Gene_name:", delabel, "<br>",
                                   "-Log10Padj:", -log10(df[, Padj]), "<br>",
                                   "Log2FoldChange:", df[, log2FC]))
    #format axes
    f1 <- list(size=18, color="black")
    if(input$basegraph == "RNA") {
      x <- list(title="log2FoldChange from RNA Data", titlefont=f1, tickfont=f1)
      y <- list(title="-log10(Padj) from RNA Data", titlefont=f1, tickfont=f1)
    } else {
      x <- list(title="log2FoldChange from ATAC Data", titlefont=f1, tickfont=f1)
      y <- list(title="-log10(Padj) from ATAC Data", titlefont=f1, tickfont=f1)
    }
    
    volplot <- volplot %>% layout(title=t, xaxis=x, yaxis=y)
    
    #add NCBI click action
    DBAddress <- paste0("https://www.ncbi.nlm.nih.gov/search/all/?term=", df$delabel)
    if(input$SearchDB_Integrate == "Human Protein Atlas") {
      DBAddress <- paste0("https://www.proteinatlas.org/search/", df$delabel)
    } else if(input$SearchDB_Integrate == "Uniprot") {
      DBAddress <- paste0("https://www.uniprot.org/uniprot/?query=", df$delabel, "&sort=score")
    }
    
    volplot <- volplot %>% add_markers(customdata=DBAddress)
    
    onRender(
      volplot, "
  function(el) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      window.open(url);
    });
  }
")
    
  })
 
  #gProfiler analysis
  #Generate input Gene list
  tolisten3 <- reactive({
    list(input$gPInput, input$sortBy, input$topGenes, input$GPgo)
  })
  gProfiler_input <- eventReactive(tolisten3(), {
    
    if(input$gPInput == "Upregulated Genes") {
      df_input <- df_UpDEG()
    } else if(input$gPInput == "Downregulated Genes") {
      df_input <- df_DownDEG()
    } else {
      df_input <- df_BothDEG()
    }
    
    if(input$sortBy == "Log2FoldChange") {
      input <- df_input[[input$names]][1:input$topGenes]
    } else {
      df_input <- df_input[order(df_input[,3]),]
      input <- df_input[[input$names]][1:input$topGenes]
    }
    return(input)
  })
  
  #gProfiler object
  gprofiler_object <- eventReactive(input$GPgo,{
    geneList <- gProfiler_input()
    Species <- switch(input$organism, 
                      "Human"="hsapiens", "Mouse"="mmusculus", "Rat"="rnorvegicus")
    method <- switch(input$Threshold, 
                     "g:SCS Threshold"="g_SCS", 
                     "Bonferroni correction"="bonferroni", "FDR"="fdr")
    domain <- switch(input$StatsDomain, 
                     "Only annotated genes"="annotated", "All known genes"="known")
    gostres <- gost(query=geneList, 
                    organism=Species, ordered_query=input$orderedQuery, 
                    multi_query=FALSE, significant=TRUE, exclude_iea=FALSE, 
                    measure_underrepresentation=FALSE, evcodes=FALSE, 
                    user_threshold=0.05, correction_method=method, 
                    domain_scope=domain, custom_bg=NULL, 
                    numeric_ns="", sources=NULL, as_short_link=FALSE)
    return(gostres)   
    })
  
  #Generate gprofile plot
  output$gProfile_plot <- renderPlotly({
    gostres <- gprofiler_object()
    p <- gostplot(gostres, capped=TRUE, interactive=TRUE)
  }
  )
  output$gProfile_table <- renderPlot({
    gostres1 <- gprofiler_object()
    gostres1$result <- gostres1$result[order(gostres1$result$p_value), ]
    p <- publish_gosttable(gostres1, highlight_terms = gostres1$result[c(1:50), ],
                      use_colors=TRUE,
                      show_columns=c("source", "term_name", "term_size", "intersection_size"),
                      filename=NULL, ggplot=TRUE)
    plot(p)
  })
  
  gostres_url <- eventReactive(gprofiler_object(),{
    input <- gProfiler_input()
    link <- gost(query=input, as_short_link=TRUE)
    url <- a("Click here", href=paste0(link))
    return(url)
  })
  
  output$tab <- renderUI({
    url <- gostres_url()
    tagList("For more detailed results :", url, "(remember to check Ordered query on the website)")
  })
})

# Run the application
shinyApp(ui=ui, server=server)

library(shiny)
library(ggplot2)
library(gplots)
library(rcartocolor)
library(dplyr)
library(formattable)

# Define the color palette for the heatmap
my_color <- colorpanel(40, "#009680", "white", "#BE4F96")

annot <- readRDS("annot.RDS") #read annotation data
expr <- readRDS("expr.RDS") #read expression data
exampleIds <- readRDS("exampleIds.RDS") #list of example gene IDs

################## Define server logic for the Shiny app ##############
function(session, input, output) {
  updateSelectizeInput(
    session,
    "gene_select",
    choices = c("", expr$GeneID),
    server = TRUE,
    selected = "Sobic.001G106000"
  )
  
  #hide download button
  shinyjs::hide("download_multi")
  
  
  ######## SINGLE GENE
  # Render gene info; cell specificity and annotation
  output$gene_feature_info <- renderUI({
    req(input$gene_slc_btn)
    
    gene <- NULL
    if (!is.null(input$gene_select) && input$gene_select != "") {
      gene <- input$gene_select
    }
    
    if (!is.null(gene)) {
      gene_feature_info <- get_gene_feature_info(gene)
      feature_info <- paste(gene_feature_info, collapse = "<br/>")
      
      HTML(paste(feature_info, sep = "<br/>"))
    }
  })
  
  # Plot gene expression
  observeEvent(input$gene_slc_btn, {
    output$expression_plot <- renderPlot({
      req(input$gene_slc_btn)
      
      if (!is.null(input$gene_select) && input$gene_select != "") {
        gene <- input$gene_select
        plot_gene_expression(gene)
      }
    })
  })
  
  
  ####### MULTIPLE GENES
  # Plot heatmap of multiple genes
  observeEvent(input$plot_btn, {
    if (!is.null(input$gene_list) && input$gene_list != "") {
      gene_list <- strsplit(input$gene_list, "\n")[[1]]
      
      #expression
      output$heatmap_plot <- renderPlot({
        gene_expr <- expr %>% subset(GeneID %in% gene_list)
        plot_heatmap(gene_expr)
      })
      
      dat_frame <-
        annot %>% subset(GeneID %in% gene_list, c(1:4, 12, 14, 15, 17, 11))
      #dat_frame$Gene <- rownames(smel_data[gene_list,])
      
      thedata <- reactive(dat_frame)
      output$multi_dto <- renderDataTable({
        thedata()
      })
      
      
      #show download button
      shinyjs::show("download_multi")
      
      #download data
      output$download_multi <- downloadHandler(
        filename = function() {
          "sorghum_data.csv"
        },
        content = function(fname) {
          write.csv(thedata(), fname)
        }
      )
    }
    else{
      showModal(
        modalDialog(
          title = NULL,
          "Please provide a list of genes.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })
  
  
  #clear text field
  observeEvent(input$clear_txt, {
    updateTextAreaInput(session, "gene_list", value = "")
    output$multi_dto <- NULL
    output$heatmap_plot <- NULL
    #hide download button
    shinyjs::hide("download_multi")
  })
  #load example Ids in the text field
  observeEvent(input$load_exmpl, {
    updateTextAreaInput(session, "gene_list", value = gsub(", ", "\n", toString(exampleIds[-1])))
  })
  
  
  # Helper function to get gene feature info
  get_gene_feature_info <- function(gene) {
    gene_record <- annot %>% filter(GeneID %in% c(gene))
    
    
    feature_info <-
      paste(
        "<b>GeneID: </b>",
        gene_record[gene, 1],
        "<br/>",
        "<b>Tau: </b>",
        ifelse(gene_record[gene, 2] != "", gene_record[gene, 2] , "NA"),
        "<br/>",
        "<b>Cell-type specificity: </b>",
        ifelse(gene_record[gene, 3] != "", gene_record[gene, 3] , "NA"),
        "<br/><br/>",
        "<u>Annotation</u>",
        "<br/>",
        "Primary transcript Id: ",
        gene_record[gene, 4],
        " <br/>",
        "Best-arabidopsis-hit: ",
        gene_record[gene, 14],
        " (",
        gene_record[gene, 12],
        ") <br/>",
        "Best-rice-hit: ",
        gene_record[gene, 17],
        " (",
        gene_record[gene, 15],
        ") <br/>"
      )
    
    return(feature_info)
  }
  
  
  # Helper function to plot gene expression barplots
  plot_gene_expression <- function(gene) {
    gene_expr <- expr %>% subset(GeneID %in% c(gene))
    
    if (input$logTransformed == TRUE) {
      gene_expr <-
        data.frame(t(log2(gene_expr[, c(2:21)] + 1))) #log-transformed TPM
      yLabel = "log2(TPM+1)"
    }
    else{
      gene_expr <- data.frame(t(gene_expr[, c(2:21)])) #TPM
      yLabel = "TPM (TMM normalized)"
    }
    
    colnames(gene_expr) <- "expr"
    gene_expr$sampleCol <-
      c(
        rep("#FFFF00", 4),
        rep("#00FF00", 4),
        rep("#FF9933", 4),
        rep("#FF6666", 4),
        rep("#66CCCC", 4)
      )
    
    
    ggplot(data = gene_expr, aes(
      x = rownames(gene_expr),
      y = expr,
      fill = sampleCol
    )) +
      xlab('Samples') +
      ylab(yLabel) +
      geom_bar(stat = "identity") +
      geom_text(aes(
        label = formattable(expr, format = "f", digits = 2),
        vjust = 0
      )) +
      scale_x_discrete(limits = rownames(gene_expr)) +
      guides(fill = "none") +
      theme_classic(
        base_size = 18,
        base_line_size = 0.2,
        base_family = "aerial"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, ),
        line = element_blank()
      )
  }
  
  # Helper function to plot heatmap
  plot_heatmap <- function(gene_expr) {
    rownames(gene_expr) <- gene_expr$GeneID
    gene_expr <- data.frame(gene_expr[, c(2:21)])
    
    #gene_expr.filt <- gene_expr[rowSums(gene_expr > 0 ) >= 2,] #plot expr for genes with >0 in atleast 4 samples
    
    if (nrow(gene_expr) > 1) {
      heatmap.2(
        as.matrix(gene_expr),
        dendrogram = "row",
        scale = "row",
        Colv = FALSE,
        colCol = c(
          rep("#E69F00", 4),
          rep("#56B4E9", 4),
          rep("#009E73", 4),
          rep("#0072B2", 4),
          rep("#D55E00", 4)
        ),
        Rowv = TRUE,
        key = TRUE,
        col = my_color,
        density.info = "none",
        key.title = NA,
        trace = "none",
        lwid = c(2, 10),
        lhei = c(2, 9),
        cexCol = 1.1,
        srtCol = 55,
        #labRow = FALSE,
        margins = c(10, 12)
      )
    }
    else{
      return(0)
      
    }
  }
  
  
}
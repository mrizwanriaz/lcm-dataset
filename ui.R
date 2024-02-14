library(shiny)
library(shinyjs)


################# Define UI for the Shiny app ################
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(# Note the wrapping of the string in HTML()
    tags$style(
      HTML(
        "
                    .well{
                    background-color:#fcf9f0;
                    border-top:0px;
                    border-radius:0px;
                    border-color:#f0dfaf;
                    }
                      .nav-tabs>li>a {
                        color:black;
                      }
                      .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
                      font-weight:600;
                    color:black;
                    background-color:#fcf9f0;
                      }
                    .nav-tabs>li.active {
                        border-botom:0px;
                      }


      "
      )
    )),
  
  titlePanel(
    fluidRow(
      "Laser Capture Microdissection (LCM) derived stem cell-type transcriptome",
      tags$h4(
        tags$i("Sorghum bicolor"),
        "cv. Wray; Vegetative stage (74 days after emergence)"
      ),
      tags$h5(
        tags$a(
          href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE218642",
          target = "_BLANK",
          "[GEO: GSE218642]",
          style = "display:inline;"
        ),
        tags$a(
          href = "https://github.com/mrizwanriaz/lcm-dataset",
          target = "_BLANK",
          "[GitHub]",
          style = "display:inline;padding-left:10px;"
        )
      ),
      style = "padding-left:10px;"
    ),
    windowTitle = "LCM Dataset"
  ),
  tags$br(),
  
  tabsetPanel(
    tabPanel(
      "Single Gene",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            inputId = "gene_select",
            label = "Select a Gene:",
            multiple = FALSE,
            choices = character(0)
          )
          ,
          fluidRow(
            column(
              6,
              checkboxInput(
                "logTransformed",
                "log-transformed?",
                value = FALSE,
                width = NULL
              ),
              style = "display:inline;"
            ),
            column(
              6,
              actionButton(
                inputId = "gene_slc_btn",
                label = "Plot",
                icon("chart-simple"),
                style = "color: #fff; background-color: #4A9759; border-color: ##4A9759;float:right"
              )
            ),
          ),
          width = 3
        ),
        
        mainPanel(fluidRow(column(
          12, div(uiOutput(outputId = "gene_feature_info"), style = "margin-top:20px;")
        )),
        fluidRow(column(
          12, div(plotOutput(outputId = "expression_plot"), style = "margin-top:60px;")
        )))
      )
    ),
    
    
    tabPanel(
      "Multiple Genes",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          textAreaInput(
            inputId = "gene_list",
            label = "Enter Gene IDs (one per line):",
            value = "",
            rows = 5
          ),
          fluidRow(column(
            12,
            
            actionButton(
              inputId = "clear_txt",
              label = "Clear",
              icon("broom"),
              style = "color: #fff; background-color: #8E9590; border-color: #8E9590;"
            ),
            actionButton(
              inputId = "load_exmpl",
              label = "Load Example IDs",
              icon("pencil"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            ),
            
            actionButton(
              inputId = "plot_btn",
              label = "Plot",
              icon("chart-simple"),
              style = "color: #fff;background-color: #4A9759; border-color: #4A9759;display:inline-block;float:right;"
            )
          )),
          width = 3
        ),
        mainPanel(fluidRow(
          column(12, plotOutput(outputId = "heatmap_plot"), style = "min-height:420px;")
        ),
        fluidRow(
          column(
            12,
            #conditionalPanel(
            #  condition = "input.gene_list != ''",
            downloadButton(outputId = 'download_multi', "Export Table as CSV", style =
                             "margin-top:40px;margin-bottom:10px;color:#fff;background-color:#2CB49E;float:right;"),
            #),
            div(dataTableOutput('multi_dto'), style =
                  "margin-top:10px;"),
            style = "padding:5px"
          )
        ),)
      )
      
    )
  )
)
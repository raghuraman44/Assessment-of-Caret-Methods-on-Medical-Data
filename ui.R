shinyUI(fluidPage(
  theme = shinytheme("simplex"),
  # Application title
  titlePanel("Assignment 3 - Raghuraman Srinivasan"),
  tabsetPanel(
    tabPanel(strong("Data"),
             verbatimTextOutput(outputId = "DataSummary"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel(strong("EDA"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr")
    ),
    tabPanel(strong("Split"),
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.75),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel(strong("Available methods"),
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel(strong("Methods"),
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             helpText("The preprocessing steps and their order are important. ", 
                      "See:", a("Documentation", href="https://www.rdocumentation.org/packages/recipes/versions/0.1.13")),
             
             tabsetPanel(type = "pills",
               tabPanel(strong("NULL Model"),
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               tabPanel(strong("GLMnet Model"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel(strong("PLS Model"),
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel(strong("Rpart Model"),
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c()), 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               ),
               
               # maintenance point ------------------------------------------------------------------------------
               # add further tabs (with controls) here
               
               tabPanel(strong("RandomForest Model"),
                        verbatimTextOutput(outputId = "RFModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RFPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c()), 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RFGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RFGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RFMetrics"),
                        hr(),
                        plotOutput(outputId = "RFModelPlots"),
                        verbatimTextOutput(outputId = "RFRecipe"),
               ),
               
               tabPanel(strong("PlsRglm Model"),
                        verbatimTextOutput(outputId = "PlsRglmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsRglmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c()), 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsRglmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsRglmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsRglmMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsRglmModelPlots"),
                        verbatimTextOutput(outputId = "PlsRglmRecipe"),
               ),
               
               tabPanel(strong("Boosted GLM Model"),
                        verbatimTextOutput(outputId = "GlmbModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmbPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c()), 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "GlmbGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GlmbGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmbMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmbModelPlots"),
                        verbatimTextOutput(outputId = "GlmbRecipe"),
               ),
               
               tabPanel(strong("GBM Model"),
                        verbatimTextOutput(outputId = "GbmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GbmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c()), 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "GbmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GbmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GbmMetrics"),
                        hr(),
                        plotOutput(outputId = "GbmModelPlots"),
                        verbatimTextOutput(outputId = "GbmRecipe"),
               ),
               
               # tabPanel(strong("BayesGLM Model"), #Takes Longer Time
               #          verbatimTextOutput(outputId = "BayesGlmModelSummary0"),
               #          fluidRow(
               #            column(width = 4, 
               #                   selectizeInput(inputId = "BayesGlmPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing", 
               #                                  choices = ppchoices,  
               #                                  multiple = TRUE, 
               #                                  selected = c()), 
               #            ),
               #            column(width = 1, 
               #                   actionButton(inputId = "BayesGlmGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "BayesGlmGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "BayesGlmMetrics"),
               #          hr(),
               #          plotOutput(outputId = "BayesGlmModelPlots"),
               #          verbatimTextOutput(outputId = "BayesGlmRecipe"),
               # ),
             
             tabPanel(strong("XGB Model"),
                      verbatimTextOutput(outputId = "XgbModelSummary0"),
                      fluidRow(
                        column(width = 4, 
                               selectizeInput(inputId = "XgbPreprocess", # name this control <Method>Preprocess
                                              label = "Pre-processing", 
                                              choices = ppchoices,  
                                              multiple = TRUE, 
                                              selected = c()), 
                        ),
                        column(width = 1, 
                               actionButton(inputId = "XgbGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "XgbGo", title = "This will train or retrain your model")
                        )
                      ),
                      hr(),
                      h3("Resampled performance:"),
                      tableOutput(outputId = "XgbMetrics"),
                      hr(),
                      plotOutput(outputId = "XgbModelPlots"),
                      verbatimTextOutput(outputId = "XgbRecipe"),
             ),
             
             tabPanel(strong("ANN Model"),
                      verbatimTextOutput(outputId = "AnnModelSummary0"),
                      fluidRow(
                        column(width = 4, 
                               selectizeInput(inputId = "AnnPreprocess", # name this control <Method>Preprocess
                                              label = "Pre-processing", 
                                              choices = ppchoices,  
                                              multiple = TRUE, 
                                              selected = c()), 
                        ),
                        column(width = 1, 
                               actionButton(inputId = "AnnGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "AnnGo", title = "This will train or retrain your model")
                        )
                      ),
                      hr(),
                      h3("Resampled performance:"),
                      tableOutput(outputId = "AnnMetrics"),
                      hr(),
                      plotOutput(outputId = "AnnModelPlots"),
                      verbatimTextOutput(outputId = "AnnRecipe"),
             ),
             
             tabPanel(strong("SVM Linear Model"),
                      verbatimTextOutput(outputId = "SvmModelSummary0"),
                      fluidRow(
                        column(width = 4, 
                               selectizeInput(inputId = "SvmPreprocess", # name this control <Method>Preprocess
                                              label = "Pre-processing", 
                                              choices = ppchoices,  
                                              multiple = TRUE, 
                                              selected = c()), 
                        ),
                        column(width = 1, 
                               actionButton(inputId = "SvmGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "SvmGo", title = "This will train or retrain your model")
                        )
                      ),
                      hr(),
                      h3("Resampled performance:"),
                      tableOutput(outputId = "SvmMetrics"),
                      hr(),
                      plotOutput(outputId = "SvmModelPlots"),
                      verbatimTextOutput(outputId = "SvmRecipe"),
             ),
             
             tabPanel(strong("SVM Radial Model"),
                      verbatimTextOutput(outputId = "SvmrModelSummary0"),
                      fluidRow(
                        column(width = 4, 
                               selectizeInput(inputId = "SvmrPreprocess", # name this control <Method>Preprocess
                                              label = "Pre-processing", 
                                              choices = ppchoices,  
                                              multiple = TRUE, 
                                              selected = c()), 
                        ),
                        column(width = 1, 
                               actionButton(inputId = "SvmrGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "SvmrGo", title = "This will train or retrain your model")
                        )
                      ),
                      hr(),
                      h3("Resampled performance:"),
                      tableOutput(outputId = "SvmrMetrics"),
                      hr(),
                      plotOutput(outputId = "SvmrModelPlots"),
                      verbatimTextOutput(outputId = "SvmrRecipe"),
             )
    )
    ),
    tabPanel(strong("Model Selection"),
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
    ),
    tabPanel(strong("Performance"),
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width=4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width=2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width=2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE ),
    )
  )
))

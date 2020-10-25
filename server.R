shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    m <- readRDS(file = rdsfile)  
    models[[name]] <- m
    
    # try to update the preprocessing steps with the ones that were used
    inpId <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), "Preprocess")
    steps <- m$recipe$steps
    seld <- c()
    for (step in steps) {
      s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
      seld <- c(seld, s)
    }
    if (length(seld) > 0 && seld[1] == "date") { 
      seld <- seld[2:length(seld)] #discard initial date step 
      seld <- seld[seld != "rm"] #discard rm step
    }
    updateSelectizeInput(session = session, inputId = inpId, choices = ppchoices, selected = seld)
    if (length(seld) > 0) {
      showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 3)
    }
  }
  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "MedicalData.csv", row.names = "ID", stringsAsFactors = TRUE)
    d$TreatmentDate <- as.Date(d$TreatmentDate)
    d$year <- year(ymd(d$TreatmentDate))
    d$month <- month(ymd(d$TreatmentDate)) 
    d$day <- day(ymd(d$TreatmentDate))
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode="list", length = n+1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 15, min = 1000, max = 5000)))
    }
    seeds[[n+1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  # observeEvent NullGo ----
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent NullGo ----
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output NullRecipe
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------

  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$GlmnetPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent GlmnetGo ----
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                              trControl = getTrControl(), tuneLength=15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output GlmnetModelSummary (text) ----
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })
  
  # output GlmnetMetrics (table) ----
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output GlmnetModelPlots (plot) ----
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  # output GlmnetRecipe (print) ----
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output GlmnetModelSummary2 (print) ----
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------

  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$PlsPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent PlsGo ----
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                              trControl = getTrControl(), tuneLength = 20)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output PlsModelSummary0 (text) ----
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  # output PlsMetrics (table) ----
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output PlsModelPlots (plot) ----
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output PlsRecipe (print) ----
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output PlsModelSummary2 (print) ----
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------

  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$RpartPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent RpartGo ----
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 20)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output RpartModelSummary0 (print) ----
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  # output RpartMetrics (table) ----
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output RpartRecipe (print) ----
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output RpartModelPlots (plot) ----
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output RpartModelTree (plot) ----
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  
  # METHOD * Random Forest ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getRFRecipe ----
  getRFRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$RFPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent RFGo ----
  observeEvent(
    input$RFGo,
    {
      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRFRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output RFModelSummary0 (print) ----
  output$RFModelSummary0 <- renderText({
    description("rf")
  })
  
  # output RFMetrics (table) ----
  output$RFMetrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })
  
  # output RFRecipe (print) ----
  output$RFRecipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })  
  
  # output RFModelPlots (plot) ----
  output$RFModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })
  
  # METHOD * PlsRglm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getPlsRglmRecipe ----
  getPlsRglmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$PlsRglmPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent PlsRglmGo ----
  observeEvent(
    input$PlsRglmGo,
    {
      library(glmnet)
      method <- "plsRglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPlsRglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output PlsRglmModelSummary (text) ----
  output$PlsRglmModelSummary0 <- renderText({
    description("plsRglm")
  })
  
  # output PlsRglmMetrics (table) ----
  output$PlsRglmMetrics <- renderTable({
    req(models$plsRglm)
    models$plsRglm$results[ which.min(models$plsRglm$results[, "RMSE"]), ]
  })
  
  # output PlsRglmModelPlots (plot) ----
  output$PlsRglmModelPlots <- renderPlot({
    req(models$plsRglm)
    plot(models$plsRglm)
  })
  
  # output PlsRglmRecipe (print) ----
  output$PlsRglmRecipe <- renderPrint({
    req(models$plsRglm)
    models$plsRglm$recipe
  })  
  
  # output PlsRglmModelSummary2 (print) ----
  output$PlsRglmModelSummary2 <- renderPrint({
    req(models$plsRglm)
    print(models$plsRglm)
  })
  
  # METHOD * Boosting GLM ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getGlmbRecipe ----
  getGlmbRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$GlmbPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent GlmbGo ----
  observeEvent(
    input$GlmbGo,
    {
      method <- "glmboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGlmbRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 20)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output GlmbModelSummary0 (print) ----
  output$GlmbModelSummary0 <- renderText({
    description("glmboost")
  })
  
  # output GlmbMetrics (table) ----
  output$GlmbMetrics <- renderTable({
    req(models$glmboost)
    models$glmboost$results[ which.min(models$glmboost$results[, "RMSE"]), ]
  })
  
  # output GlmbRecipe (print) ----
  output$GlmbRecipe <- renderPrint({
    req(models$glmboost)
    models$glmboost$recipe
  })  
  
  # output GlmbModelPlots (plot) ----
  output$GlmbModelPlots <- renderPlot({
    req(models$glmboost)
    plot(models$glmboost)
  })
  
  # METHOD * Gradient Boosting Machine ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getGbmRecipe ----
  getGbmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$GbmPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent GbmGo ----
  observeEvent(
    input$GbmGo,
    {
      method <- "gbm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGbmRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                              trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output GbmModelSummary0 (print) ----
  output$GbmModelSummary0 <- renderText({
    description("gbm")
  })
  
  # output GbmMetrics (table) ----
  output$GbmMetrics <- renderTable({
    req(models$gbm)
    models$gbm$results[ which.min(models$gbm$results[, "RMSE"]), ]
  })
  
  # output GbmRecipe (print) ----
  output$GbmRecipe <- renderPrint({
    req(models$gbm)
    models$gbm$recipe
  })  
  
  # output GbmModelPlots (plot) ----
  output$GbmModelPlots <- renderPlot({
    req(models$gbm)
    plot(models$gbm)
  })
  
  # # METHOD * Bayes GLM -------Takes Longer Time--------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getBayesGlmRecipe ----
  # getBayesGlmRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     step_date(has_type("date"), features = "decimal") %>%
  #     steps(input$BayesGlmPreprocess) %>%
  #     step_rm(has_type("date"))
  # })
  # 
  # # observeEvent BayesGlmGo ----
  # observeEvent(
  #   input$BayesGlmGo,
  #   {
  #     method <- "bayesglm"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getBayesGlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     }, 
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output BayesGlmModelSummary0 (print) ----
  # output$BayesGlmModelSummary0 <- renderText({
  #   description("bayesglm")
  # })
  # 
  # # output BayesGlmMetrics (table) ----
  # output$BayesGlmMetrics <- renderTable({
  #   req(models$bayesglm)
  #   models$bayesglm$results[ which.min(models$bayesglm$results[, "RMSE"]), ]
  # })
  # 
  # # output BayesGlmRecipe (print) ----
  # output$BayesGlmRecipe <- renderPrint({
  #   req(models$bayesglm)
  #   models$bayesglm$recipe
  # })  
  # 
  # # output BayesGlmModelPlots (plot) ----
  # output$BayesGlmModelPlots <- renderPlot({
  #   req(models$bayesglm)
  #   plot(models$bayesglm)
  # })

  
  # METHOD * Extreme Gradient Boosting  ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getXgbRecipe ----
  getXgbRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$XgbPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent GbmGo ----
  observeEvent(
    input$XgbGo,
    {
      method <- "xgbLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getXgbRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                              trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output XgbModelSummary0 (print) ----
  output$XgbModelSummary0 <- renderText({
    description("xgbLinear")
  })
  
  # output XgbMetrics (table) ----
  output$XgbMetrics <- renderTable({
    req(models$xgbLinear)
    models$xgbLinear$results[ which.min(models$xgbLinear$results[, "RMSE"]), ]
  })
  
  # output XgbRecipe (print) ----
  output$XgbRecipe <- renderPrint({
    req(models$xgbLinear)
    models$xgbLinear$recipe
  })  
  
  # output XgbModelPlots (plot) ----
  output$XgbModelPlots <- renderPlot({
    req(models$xgbLinear)
    plot(models$xgbLinear)
  })
  
  # METHOD * ANN  ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getAnnRecipe ----
  getAnnRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$AnnPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent AnnGo ----
  observeEvent(
    input$AnnGo,
    {
      method <- "nnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getAnnRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                              trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output AnnModelSummary0 (print) ----
  output$AnnModelSummary0 <- renderText({
    description("nnet")
  })
  
  # output AnnMetrics (table) ----
  output$AnnMetrics <- renderTable({
    req(models$nnet)
    models$nnet$results[ which.min(models$nnet$results[, "RMSE"]), ]
  })
  
  # output AnnRecipe (print) ----
  output$AnnRecipe <- renderPrint({
    req(models$nnet)
    models$nnet$recipe
  })  
  
  # output AnnModelPlots (plot) ----
  output$AnnModelPlots <- renderPlot({
    req(models$nnet)
    plot(models$nnet)
  })
  
  # METHOD * SVM Linear  ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getSvmRecipe ----
  getSvmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$SvmPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent SvmGo ----
  observeEvent(
    input$SvmGo,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getSvmRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                              trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output SvmModelSummary0 (print) ----
  output$SvmModelSummary0 <- renderText({
    description("svmLinear")
  })
  
  # output SvmMetrics (table) ----
  output$SvmMetrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  # output SvmRecipe (print) ----
  output$SvmRecipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  # output SvmModelPlots (plot) ----
  output$SvmModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })
  
  # METHOD * SVM Radial  ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getSvmrRecipe ----
  getSvmrRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$SvmrPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent SvmrGo ----
  observeEvent(
    input$SvmrGo,
    {
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getSvmrRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                              trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output SvmrModelSummary0 (print) ----
  output$SvmrModelSummary0 <- renderText({
    description("svmRadial")
  })
  
  # output SvmMetrics (table) ----
  output$SvmrMetrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  # output SvmrRecipe (print) ----
  output$SvmrRecipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  # output SvmrModelPlots (plot) ----
  output$SvmrModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })

  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getResamples ----
  getResamples <- reactive({
    models <- reactiveValuesToList(models)
    results <- caret::resamples(models)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }

    #hide results worse than null model
    subset <- rep(TRUE, length(models))
    if (input$HideWorse & NullModel %in% names(models)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })

  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })

  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
    
})

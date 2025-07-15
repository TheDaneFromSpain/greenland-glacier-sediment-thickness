### Install packages if running for the first time:
# install.packages(c("shiny", "terra", "hdf5r"))

library(shiny)
library(terra)  # currently v1.7.65
library(hdf5r)  # incompatible with values() from terra

## Helpful how-to's and tips:
# PLEASE install Rstudio if planning to use R, turn on all options in Tools -> Global Options -> Code -> Display -> Syntax
#                                              and customize theme in Tools -> Global Options -> Appearance (I use Tomorrow Night Bright)
# control+Enter / command+Enter to execute. It will automatically advance to the next line. You can select the lines to be executed before executing.
# If dealing with working directory problems, use getwd() and setwd() to troubleshoot.
# Folder path can be relative (where ./ is the directory where the script is located) or absolute (like C:/Users/...).
# Functions and sections can be folded with a small arrow to the left or by Alt+L, Shift+Alt+L, Alt+O, Shift+Alt+O (in Rstudio)

## FOR EDITING THIS SCRIPT:
# Most meaningful changes are done in the load function, located in the beginning of the server function (you can use ctrl+F to find it, at the moment it is line 178)

GEBCO.rast <<- rast('./OrganizedData/GEBCO/GEBCO.tif'); names(GEBCO.rast) <- "GEBCO"

sigmoid <<- function(x){ return(1/(1+exp(-x))) }

# Define UI for application ----
ui <- fluidPage(
    titlePanel("Greenland seabed glacier sediments"),

    sidebarLayout(
        
        # Sidebar panel for inputs
        sidebarPanel(
            # style = "position:fixed;width:inherit;",
            
            h4("Section 1.1: Load data"),
            
            textInput(
                "ms_to_m_conversion",
                "Miliseconds to metre conversion rate (for GEUS data):",
                value = "0.74"
            ),
            
            checkboxGroupInput(
                "model_features",
                "Select features:",
                choices = c(
                    "depth", 
                    "slope",
                    "TRI",  # basically the same as slope
                    "roughness",  # basically the same as slope
                    # "aspect",  # directional, hardly relevant
                    "TPI",  # basically the same as slope
                    # "general_flowdir",  # also directional
                    "dist_shore", 
                    "dist_mainland",
                    "bidist_shelf",
                    # "dist_shore_shelf", 
                    # "dist_shelf_shore", 
                    # "dist_shelf_deep", 
                    "gravity",
                    "wdist_glaciers",
                    "flow_acc"
                ),
                inline = TRUE,
                # selected = "depth"
            ),
            
            selectInput(
                "training_data_selection",
                "Select training data:",
                choices = c(
                    "GEUS with sd",
                    "GEUS and GRADP, no sd",
                    "GRADP, no sd"
                )
            ),

            strong("Select prediction area (in Overview tab) -->"),
            
            conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                
                h5("Busy...", style="color:red")
            ),
            
            conditionalPanel(
                condition = "!$('html').hasClass('shiny-busy')",
                
                h5("Ready.", style="color:green")
            ),
            
            
            actionButton(
                "LOAD",
                "LOAD DATA"
            ),
            
            h5("Some sections can only be accessed after loading data. Changes won't be saved if you don't load them again."),
            
            hr(),
            
            h4("Section 1.2: Export data"),

            textInput(
                "low_res",
                "Divide dataset resolution by*:",
                value = "5"
            ),
            
            h5("*For big prediction areas it is recommended to divide resolution by more than 1."),
            
            checkboxInput(
                "descriptive_name",
                "Save dataset with descriptive name",
                value = TRUE
            ),
            
            conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                
                h5("Busy...", style="color:red")
            ),
            
            conditionalPanel(
                condition = "!$('html').hasClass('shiny-busy')",
                
                h5("Ready.", style="color:green")
            ),
            
            conditionalPanel(
                condition = "output.plot_thickness_UI",
            
                downloadButton(
                    "EXPORT",
                    "SAVE DATASET"
                )
            ),
            
            downloadButton(
                "LOAD_EXPORT",
                "SAVE DATASET DIRECTLY*"
            ),
            
            h5("*By loading in the background. Is processing even if it still says 'Ready.'"),
            
            uiOutput("plot_thickness_UI"),
            
            hr(),
            
            h4("Section 2: Analysis"),
            
            #- Correlation scatterplots
            uiOutput("correlation_scatterplots_UI"),
            
            #- Click features
            uiOutput("click_features_UI"),
            
            hr(),
            
            h4("Section 3: Linear regression"),
            
            #- Regression
            uiOutput("regression_UI"),
        ),

        # Main panel for displaying outputs
        mainPanel(
            
            uiOutput("feature_tabs_UI"),
            
            uiOutput("plot_prediction_UI")
            
        )
        
    )
)


# Define server logic ----
server <- function(input, output, session) {
    
    load <- function(){
        brush <- input$prediction_area
        if (!is.null(input$prediction_area)) {
            reactive$prediction_extent <- intersect(ext(brush$xmin, brush$xmax, brush$ymin, brush$ymax),
                                                    ext(-76.3833333333333, 0.3125, 57.7958333333333, 86.2958333333333))
        }
        
        model_features <<- input$model_features
        training_data_selection <- input$training_data_selection
        ms_to_m_conversion <- as.numeric(input$ms_to_m_conversion)
        prediction_extent <<- reactive$prediction_extent
        
        ### Load training data -----
        
        if (training_data_selection == "GEUS with sd"){
            #-> Only GEUS, with standard deviation
            GEUS.vect <- vect("./OrganizedData/GEUS/GEUS_thickness-sd-count-pointdens.shp")
            GEUS.vect[["thickness"]] <- GEUS.vect[["thickness"]] * ms_to_m_conversion
            GEUS.vect[["sd"]] <- GEUS.vect[["sd"]] * ms_to_m_conversion
            training_data.vect <<- GEUS.vect
            training_data_id <<- "GEUS"
            
        }else if (training_data_selection == "GEUS and GRADP, no sd"){
            #-> GEUS and GRADP, no standard deviation
            GEUS.vect <- vect("./OrganizedData/GEUS/GEUS_thickness-sd-count-pointdens.shp")[c("thickness", "count", "point_dens")]
            GEUS.vect[["thickness"]] <- GEUS.vect[["thickness"]] * ms_to_m_conversion
            GRADP_AU1.vect <- vect("./OrganizedData/GRADP/AU1_thickness-count-pointdens.shp")
            training_data.vect <<- rbind(GEUS.vect, GRADP_AU1.vect)
            training_data_id <<- "GEUSandGRADP"
            
        }else if (training_data_selection == "GRADP, no sd"){
            #-> Only GRADP, no standard deviation or counts
            GRADP_AU1.vect <- vect("./OrganizedData/GRADP/AU1_thickness-count-pointdens.shp")
            training_data.vect <<- GRADP_AU1.vect
            training_data_id <<- "GRADP"
            
        }else{
            message("Error: training data selection is outside programmed possibilities")
        }
        
        ### Load features -----
        
        if ("depth" %in% model_features){     # Bathymetry from GEBCO, in metres, negative.
            if (!exists("depth.rast")){
                depth.rast <- GEBCO.rast; names(depth.rast) <- "depth"
                depth.rast[depth.rast > 0] <- NA
            }else{
                window(depth.rast) <- NULL
            }
            depth.num <- extract(depth.rast, training_data.vect)$depth
            window(depth.rast) <- prediction_extent
        }
        if ("slope" %in% model_features){     # Value of slope, in degrees between 0 and 90. 
            if (!exists("slope.rast")){
                slope.rast <- trim(terra::terrain(GEBCO.rast, v = "slope"))
            }else{
                window(slope.rast) <- NULL
            }
            slope.num <- extract(slope.rast, training_data.vect)$slope
            window(slope.rast) <- prediction_extent
        }
        if ("aspect" %in% model_features){    # Direction of steepest decent, in degrees following a compass (0 = north, 90 = east, ...).
            if (!exists("aspect.rast")){
                aspect.rast <- trim(terra::terrain(GEBCO.rast, v = "aspect"))
                aspect.rast <- focal(aspect.rast, w = 3, mean, pad = T); names(aspect.rast) <- "aspect"
            }else{
                window(aspect.rast) <- NULL
            }
            aspect.num <- extract(aspect.rast, training_data.vect)$aspect
            window(aspect.rast) <- prediction_extent
        }
        if ("TPI" %in% model_features){   # Terrain Position Index. Positive values mean the cell is higher than its surroundings while negative values mean it is lower. 
            if (!exists("TPI.rast")){
                TPI.rast <- trim(terra::terrain(GEBCO.rast, v = "TPI"))
                # TPI.rast <- focal(depth.rast, w=5, fun=function(x){ x[13] - mean(x[-13]) })
                # TPI.rast <- focal(depth.rast, w=7, fun=function(x){ x[25] - mean(x[-25]) })
            }else{
                window(TPI.rast) <- NULL
            }
            TPI.num <- extract(TPI.rast, training_data.vect)$TPI
            window(TPI.rast) <- prediction_extent
        }
        if ("TRI" %in% model_features){   # Terrain Ruggedness Index. Indicates how jagged or flat the terrain of a country is on average. 
            if (!exists("TRI.rast")){
                TRI.rast <- trim(terra::terrain(GEBCO.rast, v = "TRI"))
            }else{
                window(TRI.rast) <- NULL
            }
            TRI.num <- extract(TRI.rast, training_data.vect)$TRI
            window(TRI.rast) <- prediction_extent
        }
        if ("roughness" %in% model_features){     # Roughness Index. Largest inter-cell difference of a central pixel and its surrounding cell. 
            if (!exists("roughness.rast")){
                roughness.rast <- trim(terra::terrain(GEBCO.rast, v = "roughness"))
            }else{
                window(roughness.rast) <- NULL
            }
            roughness.num <- extract(roughness.rast, training_data.vect)$roughness
            window(roughness.rast) <- prediction_extent
        }
        if ("general_flowdir" %in% model_features){   # Flow direction, coded in powers of two as shown in ?trim(terra::terrain. The Whitebox package can calculate an actual flow chart and has been used in Ice.
            if (!exists("general_flowdir.rast")){
                general_flowdir.rast <- terra::terrain(GEBCO.rast, v = "flowdir")
                general_flowdir.rast <- focal(general_flowdir.rast, w = 5, mean); names(general_flowdir.rast) <- "general_flowdir"
            }else{
                window(general_flowdir.rast) <- NULL
            }
            general_flowdir.num <- extract(general_flowdir.rast, training_data.vect)$general_flowdir
            window(general_flowdir.rast) <- prediction_extent
        }
        if ("gravity" %in% model_features){   # Bouguer gravity anomaly data, in mGal.
            if (!exists("gravity.rast")){
                gravity.rast <- rast("./OrganizedData/Gravity/Bouguer_anomaly.tif")
                names(gravity.rast) <- "gravity"
            }else{
                window(gravity.rast) <- NULL
            }
            gravity.num <- extract(gravity.rast, training_data.vect)$gravity
            window(gravity.rast) <- prediction_extent
        }
        if ("wdist_glaciers" %in% model_features){   # Glacier distance weighted with glacier sediment production, grouped by fjords
            if (!exists("wdist_glaciers.rast")){
                wdist_glaciers.rast <- rast("./OrganizedData/Glaciers/weighted_fjord_distances_min.tif")
                names(wdist_glaciers.rast) <- "wdist_glaciers"
            }else{
                window(wdist_glaciers.rast) <- NULL
            }
            wdist_glaciers.num <- extract(wdist_glaciers.rast, training_data.vect)$wdist_glaciers
            window(wdist_glaciers.rast) <- prediction_extent
        }
        if ("dist_shore" %in% model_features){   # Distance to shore
            if (!exists("dist_shore.rast")){
                dist_shore.rast <- rast("./OrganizedData/GEBCO/GEBCOdist_shore_frompolar.tif")
                names(dist_shore.rast) <- "dist_shore"
            }else{
                window(dist_shore.rast) <- NULL
            }
            dist_shore.num <- extract(dist_shore.rast, training_data.vect)$dist_shore
            window(dist_shore.rast) <- prediction_extent
        }
        if ("dist_mainland" %in% model_features){   # Distance to only greenland main island
            if (!exists("dist_mainland.rast")){
                dist_mainland.rast <- rast("./OrganizedData/GEBCO/GEBCOdist_mainland_frompolar.tif")
                names(dist_mainland.rast) <- "dist_mainland"
            }else{
                window(dist_mainland.rast) <- NULL
            }
            dist_mainland.num <- extract(dist_mainland.rast, training_data.vect)$dist_mainland
            window(dist_mainland.rast) <- prediction_extent
        }
        
        if ("bidist_shelf" %in% model_features){   # Distance to shelf, positive out to sea and negative inland
            if (!exists("bidist_shelf.rast")){
                bidist_shelf.rast <- rast("./OrganizedData/GEBCO/GEBCObidist_shelf_frompolar.tif")
                names(bidist_shelf.rast) <- "bidist_shelf"
            }else{
                window(bidist_shelf.rast) <- NULL
            }
            bidist_shelf.num <- extract(bidist_shelf.rast, training_data.vect)$bidist_shelf
            window(bidist_shelf.rast) <- prediction_extent
        }
        if ("flow_acc" %in% model_features){   # Flow accumulation made with whitebox package
            if (!exists("flow_acc.rast")){
                flow_acc.rast <- rast("./OrganizedData/GEBCO/GEBCOflow_accumulation.tif")
                names(flow_acc.rast) <- "flow_acc"
            }else{
                window(flow_acc.rast) <- NULL
            }
            flow_acc.num <- extract(flow_acc.rast, training_data.vect)$flow_acc
            window(flow_acc.rast) <- prediction_extent
        }
        
        if (is.null(model_features) == F){
            #- Data frame with training features
            training_features.df <<- as.data.frame(cbind(sapply(model_features, function(name){ get(paste0(name, ".num")) } )))
            
            #- Raster with prediction features, already with masked land cells
            
            prediction_features.rast <<- rast(lapply(model_features, function(name){ get(paste0(name, ".rast")) } ))
        }
    }
    
    reactive <- reactiveValues(training_extent = ext(-76.3833333333333, 0.3125, 57.7958333333333, 86.2958333333333),
                              prediction_extent = ext(-76.3833333333333, 0.3125, 57.7958333333333, 86.2958333333333))
    
    ### output$GEBCO #####
    
    output$feature_tabs_UI <- renderUI({
        
        tabsetPanel(
            tabPanel("Overview", plotOutput(outputId = "GEBCO",
                                            height = "800px",
                                            brush = brushOpts(
                                                id = "prediction_area",
                                                resetOnNew = FALSE       # change if necessary
                                            ))
                     )
        )

    })
    
    output$GEBCO <- renderPlot({
        plot(GEBCO.rast, main = "Drag to select prediction area (full area by default)", col = hcl.colors(256, "YlGnBu"))
        plot(reactive$training_extent, add = T, lty = "dotted")
        plot(reactive$prediction_extent, add = T, lty = "solid")
    })
    
    observeEvent(input$training_data_selection, {
        if (input$training_data_selection == "GEUS with sd"){
            reactive$training_extent <- ext(-61.7229166666667, -52.81875, 62.68125, 69.3854166666667)
        }else if (input$training_data_selection == "GEUS and GRADP, no sd"){
            reactive$training_extent <- ext(-61.7229166666667, -50.6812931760765, 62.6209798439444, 69.3854166666667)
        }else if (input$training_data_selection == "GRADP, no sd"){
            reactive$training_extent <- ext(-58.0424471740846, -50.6812931760765, 62.6209798439444, 68.1234424574555)
        }else{
            message("Error: training data selection is outside programmed possibilities")
        }
    }, ignoreInit = FALSE)  # change if necessary
    
    #####
    
    observeEvent(input$LOAD,{   #{ input$LOAD_EXPORT}
        
        load()
        
        ### output$feature_tabs #####
        
        output$feature_tabs_UI <- renderUI({
            
            featureTabs = c(list(tabPanel("Overview", plotOutput(outputId = "GEBCO",
                                       height = "800px",
                                       brush = brushOpts(
                                           id = "prediction_area",
                                           resetOnNew = FALSE       # change if necessary
                                       )))),lapply(model_features, function(name){
                tabPanel(name, renderPlot({
                    plot(prediction_features.rast[[name]], main = name)
                }, outputArgs = list(click = "click", height = "800px")))
            }))
            
            do.call(tabsetPanel, featureTabs)
            
        })
        
        output$plot_thickness_UI <- renderUI({
            
            tagList(
                
                downloadButton(
                    "plot_thickness",
                    "Save thickness data plot"
                )
                
            )
            
        })
        
        if (is.null(model_features) == F){
            
            output$click_features_UI <- renderUI({
                
                tagList(
                    
                    hr(),
                    
                    verbatimTextOutput("click_features")
                    
                )
                
            })
            
            output$click_features <- renderText({ "Click on feature maps in their respective tabs to display values" })
            
            #####
            
            ### output$correlation_scatterplots #####
            
            output$correlation_scatterplots_UI <- renderUI({
                
                tagList(
                    
                    selectInput(
                        "study_feature",
                        "Select correlation scatterplot y axis:",
                        choices = c("thickness",
                                    input$model_features)
                    ),
                    
                    downloadButton(
                        "make_correlation_scatterplots",
                        "SAVE SCATTERPLOTS"
                    ),
                    
                )
                
            })
            
            #####
            
            ### output$regression #####
            
            output$regression_UI <- renderUI({
                
                tagList(
                    
                    textInput(
                        "regression_formula",
                        "Input linear regression formula*:",
                        value = "gravity + log(dist_shore) * poly(depth,2)"
                    ),
                    
                    h5("*The app will crash if a non-loaded feature is included or an ilegal formula is used. All intermediate possible features are also included in the regression."),
                    
                    actionButton(
                        "regress",
                        "See regression model fit"
                    ),
                    
                    verbatimTextOutput("regression"),
                    
                )
                
            })
            
            #####
            
        }
        
    })
    
    observeEvent(input$click,{
        click <- input$click
        output$click_features <- renderPrint({ extract(prediction_features.rast, matrix(c(click$x, click$y), 1, 2)) })
    })
    
    output$make_correlation_scatterplots <- downloadHandler(
        filename = function() {
            study_feature <- input$study_feature
            model_matrix <- cbind(training_data.vect[["thickness"]], training_features.df)
            study_feature_pos <- which(names(model_matrix) == study_feature)
            paste0("CorrelationScatterplots_", training_data_id, "_", study_feature, "-", paste(names(model_matrix)[-study_feature_pos], collapse = "-"), ".png")
        },
        content = function(bsfile) {
        
        study_feature <- input$study_feature
        # study_feature <- "thickness"
        model_matrix <- cbind(training_data.vect[["thickness"]], training_features.df)

        sample_index <- sample(1:nrow(model_matrix), min(nrow(model_matrix) / 10, 1e5))
        study_feature_pos <- which(names(model_matrix) == study_feature)
        model_matrix_sample <- model_matrix[sample_index, ]
        # png("./corr.png", width = 1920, height = 1080)
        png(bsfile, width = 1920, height = 1080)
        fraction <- 50
        n <- length(names(model_matrix)[-study_feature_pos])
        if (n == 1){
            dims <- 1
        }else if (n > 1) {
            dims <- which.min(abs(1:n - sqrt(n)) ^ 2)
        }else{
            message("Error: n is less than 1 in correlation scatterplot exporter")
        }
        par(mfrow = c(dims, ceiling(n / dims)))
        diff_range_thickness <- diff(range(model_matrix_sample[, study_feature_pos]))
        for (name in names(model_matrix_sample)[-study_feature_pos]){
            corr <- cor(model_matrix_sample[, name], model_matrix_sample[, study_feature_pos])
            scattercols <- densCols(model_matrix_sample[, name], model_matrix_sample[, study_feature_pos], nbin = 256, colramp = colorRampPalette(hcl.colors(256, "rocket", rev = T)),
                                    bandwidth = c(diff(range(model_matrix_sample[, name])) / fraction, diff_range_thickness / fraction))
            plot(model_matrix_sample[, name], model_matrix_sample[, study_feature_pos], main = paste0("Correlation: ", corr),
                 xlab = name, ylab = study_feature, col = scattercols)
        }
        dev.off()
        }
    )
    

    observeEvent(input$regress,{
        
        model_matrix <- cbind(training_data.vect[["thickness"]], training_features.df)

        linear_model <- lm(formula = paste0("thickness ~ ", input$regression_formula), data = model_matrix, weights = 1/training_data.vect[["point_dens"]][,1])

        output$regression <- renderPrint({ summary(linear_model) })
        
        low_res <- as.numeric(input$low_res)
        prediction_features.rast_agg <- terra::aggregate(prediction_features.rast, fact = low_res)  # can multicore
        
        prediction <<- terra::predict(prediction_features.rast_agg, linear_model, na.rm = T, se.fit = T)
        # writeRaster(prediction, "./linear_model_prediction.tif")
        
        output$plot_prediction_UI <- renderUI({
            
            wellPanel(
                renderPlot({
                    par(mfrow = c(1,2))
                    plot(prediction[[1]], main = "prediction", col = hcl.colors(256, "viridis"))
                    plot(prediction[[2]], main = "st. error", col = hcl.colors(256, "plasma"))
                }, outputArgs = list(height = "800px"))
            )

        })
        
    })
    
    output$plot_thickness <- downloadHandler(
        filename = paste0(training_data_id, "_thickness-data.png"),
        content = function(filename) {
            png(filename, height = 1600, width = 1200)
            plot(crop(GEBCO.rast, ext(training_data.vect)), col = gray(0:256/256), legend = F) # plg = list(loc = "bottom")
            plot(training_data.vect, "thickness", type = "continuous", cex = .2, col = hcl.colors(256, "viridis"), add = T)
            dev.off()
        }
    )
    
    output$EXPORT <- downloadHandler(
        filename = function() {
            if (input$descriptive_name == T){
                low_res <- as.numeric(input$low_res)
                paste0("SedimentModelDataset_", low_res, "_", paste0(training_data_id, "-", paste(model_features, collapse = "-")), ".h5")
            }else{
                paste0("SedimentModelDataset.h5")
            }
        },
        content = function(bsfile) {
            low_res <<- as.numeric(input$low_res)
            
            if (low_res != 1){
                if (is.null(model_features) == F){
                    prediction_features.rast_agg <- terra::aggregate(prediction_features.rast, fact = low_res)  # can multicore
                }
                
                file.h5 <- H5File$new(bsfile, mode = "w")
                
                #- Write to hdf5 file
                file.h5$create_dataset("TrainingThickness", as.data.frame(training_data.vect))
                if (is.null(model_features) == F){
                    file.h5$create_dataset("xy-TrainingFeatures", cbind(crds(training_data.vect), training_features.df))
                    file.h5$create_dataset("xy-PredictionFeatures", cbind(crds(prediction_features.rast_agg), as.data.frame(prediction_features.rast_agg, na.rm = T)))
                }
                
                file.h5$close_all()
            }else{
                file.h5 <- H5File$new(bsfile, mode = "w")
                
                #- Write to hdf5 file
                file.h5$create_dataset("TrainingThickness", as.data.frame(training_data.vect))
                if (is.null(model_features) == F){
                    file.h5$create_dataset("xy-TrainingFeatures", cbind(crds(training_data.vect), training_features.df))
                    file.h5$create_dataset("xy-PredictionFeatures", cbind(crds(prediction_features.rast), as.data.frame(prediction_features.rast, na.rm = T)))
                }
                
                file.h5$close_all()
            }
        }
    )
    
    output$LOAD_EXPORT <- downloadHandler(
        filename = function() {
            
            load()
            
            if (input$descriptive_name == T){
                low_res <- as.numeric(input$low_res)
                paste0("SedimentModelDataset_", low_res, "_", paste0(training_data_id, "-", paste(model_features, collapse = "-")), ".h5")
            }else{
                paste0("SedimentModelDataset.h5")
            }
        },
        content = function(bsfile) {
            
            low_res <<- as.numeric(input$low_res)
            
            if (low_res != 1){
                if (is.null(model_features) == F){
                    prediction_features.rast_agg <- terra::aggregate(prediction_features.rast, fact = low_res)  # can multicore
                }
                
                file.h5 <- H5File$new(bsfile, mode = "w")
                
                #- Write to hdf5 file
                file.h5$create_dataset("TrainingThickness", as.data.frame(training_data.vect))
                if (is.null(model_features) == F){
                    file.h5$create_dataset("xy-TrainingFeatures", cbind(crds(training_data.vect), training_features.df))
                    file.h5$create_dataset("xy-PredictionFeatures", cbind(crds(prediction_features.rast_agg), as.data.frame(prediction_features.rast_agg, na.rm = T)))
                }
                
                file.h5$close_all()
            }else{
                file.h5 <- H5File$new(bsfile, mode = "w")
                
                #- Write to hdf5 file
                file.h5$create_dataset("TrainingThickness", as.data.frame(training_data.vect))
                if (is.null(model_features) == F){
                    file.h5$create_dataset("xy-TrainingFeatures", cbind(crds(training_data.vect), training_features.df))
                    file.h5$create_dataset("xy-PredictionFeatures", cbind(crds(prediction_features.rast), as.data.frame(prediction_features.rast, na.rm = T)))
                }
                
                file.h5$close_all()
            }
        }
    )
    
    session$onSessionEnded(function() {
        stopApp()
    })
    
}


### PLAYGROUND #################################################################
if (FALSE == TRUE){
    
    # for (i in 1:nlyr(prediction_features.rast)){
    #     png(paste0("./prediction_feature_NEW_",i,".png"), 1600, 1400)
    #     plot(prediction_features.rast[[i]], main = names(prediction_features.rast)[i])
    #     dev.off()
    # }
    
    # (obsolete)
    # window(GEBCO.rast) <- prediction_extent
    # prediction_features.rast <- mask(prediction_features.rast, GEBCO.rast < 0, maskvalue = F)
    # # rast("./OrganizedData/Glaciers/icevelfjord_mask.tif")     # mask with this if necessary to eliminate fjords from prediction (wdist_glaiciers)
    # window(GEBCO.rast) <- NULL
    
    prediction_features.rast <- terra::aggregate(prediction_features.rast, fact = 5)  # can multicore
    
    model_matrix <- cbind(training_data.vect[["thickness"]], training_features.df)
    # model_matrix_scaled <- as.data.frame(scale(model_matrix, center = F))
    
    ### local
    prediction_features.rast_local <- prediction_features.rast
    model_matrix_local <- model_matrix
    linmodel <- lm(thickness ~ depth + poly(gravity,3), data = model_matrix_local, weights = 1/training_data.vect[["point_dens"]][,1])
    # linmodel <- lm(thickness ~ log(dist_shore - bidist_shelf) + depth + gravity, data = model_matrix, weights = 1/training_data.vect[["point_dens"]][,1])
    summary(linmodel)
    prediction <- terra::predict(prediction_features.rast_local, linmodel, na.rm = T, se.fit = T)
    # writeRaster(prediction, "./PredictionRasters/linearization_logdbid.tif")
    par(mfrow=c(1,2))
    plot(prediction[[1]], main="prediction", col=hcl.colors(256, "viridis"))
    plot(prediction[[2]], main="st. error", col=hcl.colors(256, "plasma"))
    
    ### global
    prediction_features.rast_global <- mask(prediction_features.rast, prediction_features.rast[["bidist_shelf"]] < 1e5, maskvalue = F)
    model_matrix_global <- model_matrix
    linmodel <- lm(thickness ~ depth + poly(gravity,3), data = model_matrix, weights = 1/training_data.vect[["point_dens"]][,1])
    # linmodel <- lm(thickness ~ log(dist_shore - bidist_shelf) + depth + gravity, data = model_matrix, weights = 1/training_data.vect[["point_dens"]][,1])
    summary(linmodel)
    prediction <- terra::predict(prediction_features.rast, linmodel, na.rm = T, se.fit = T)
    # writeRaster(prediction, "./PredictionRasters/linearization_logdbid.tif")
    par(mfrow=c(1,2))
    plot(prediction[[1]], main="prediction", col=hcl.colors(256, "viridis"))
    plot(prediction[[2]], main="st. error", col=hcl.colors(256, "plasma"))
    
    
    png(paste0("./Predictions/best_linear_try-depth-gravity3_local.png"), height = 1600, width = 1400)
    par(mfrow=c(1,2))
    plot(prediction[[1]], main="prediction", col=hcl.colors(256, "viridis"))
    plot(prediction[[2]], main="st. error", col=hcl.colors(256, "plasma"))
    dev.off()
    
    
    
    ####### NON LINEAR SHENANIGANS
    
    # linear_model <- nls(formula = thickness ~ ., data = model_matrix, trace = T)
    
    linear_model <- glm(formula = "log(thickness) ~ bidist_shelf", data = model_matrix, family = gaussian, weights = 1/training_data.vect[["point_dens"]][,1])
    bidist_coef <- coef(linear_model)  # gives log(a) as intercept
    linear_model <- glm(formula = "log(thickness) ~ dist_shore", data = model_matrix, family = gaussian, weights = 1/training_data.vect[["point_dens"]][,1])
    dist_coef <- coef(linear_model)  # gives log(a) as intercept
    linear_model <- glm(formula = "log(thickness) ~ dist_shore + bidist_shelf", data = model_matrix, family = gaussian, weights = 1/training_data.vect[["point_dens"]][,1])
    multidist_coef <- coef(linear_model)  # gives log(a) as intercept
    
    linear_model <- glm(formula = "thickness ~ depth + gravity", data = model_matrix, family = gaussian, weights = 1/training_data.vect[["point_dens"]][,1])
    other_coef <- coef(linear_model)  # gives log(a) as intercept
    
    nls_formula <- "d*depth + g*gravity + multidist*exp(bidistB*bidist_shelf + distB*dist_shore)"
    model <- nls(formula = paste0("thickness ~ ", nls_formula), data = model_matrix,
                 start = list(#x = -500, y = 1, 
                              # intercept = other_coef["(Intercept)"],
                              d = other_coef["depth"],
                              g = other_coef["gravity"],
                              # g = 1, g2 = 1, d=.1,  ############# make them start at something instead of just 1
                              multidist = multidist_coef["(Intercept)"], 
                              distB = multidist_coef["dist_shore"],
                              bidistB = multidist_coef["bidist_shelf"]
                 ),
                 trace = T, weights = 1/training_data.vect[["point_dens"]][,1], control = list(maxiter = 100))
    summary(model)
    multimodel_formula <- "(1-sigmoid(y*dist_shore-x)) * (c*depth + alpha*exp(beta*bidist_shelf) ) + sigmoid(y*dist_shore-x) * (d*gravity)"   # x=2e5
    multimodel_formula <- "intercept + (1-sigmoid(-y*depth+x)) * ( g*gravity + distA*exp(distB*dist_shore) + bidistA*exp(bidistB*bidist_shelf)) + sigmoid(-y*depth+x) * ( g*gravity + d*depth ) "    # x=-500, 1-sigmoids inverted
    model <- nls(formula = paste0("thickness ~ ", multimodel_formula), data = model_matrix,
                 start = list(x = -500, y = 1, 
                              intercept = other_coef["(Intercept)"],
                              d = other_coef["depth"],
                              g = other_coef["gravity"],
                              # g = 1, g2 = 1, d=.1,  ############# make them start at something instead of just 1
                              distA = dist_coef["(Intercept)"], 
                              distB = dist_coef["dist_shore"],
                              bidistA = bidist_coef["(Intercept)"],
                              bidistB = bidist_coef["bidist_shelf"]
                 ),
                 trace = T, weights = 1/training_data.vect[["point_dens"]][,1], control = list(maxiter = 100))
    summary(model)
    # vcov(model)     # estimate of the scaled variance-covariance matrix, if high values then probably overfitted
    ??nls
    ??boot
    # library(car)
    model_boot <- Boot(model, function(x){ coef(model)["d"] + coef(model)["g"] })
    confint(model_boot)
    model_delta <- deltaMethod(model, "d + g + multidist * exp(bidistB * distB)")
    
    multimodel_formula <- " (1-sigmoid(-y*depth+x)) * ( distA*exp(distB*dist_shore) ) + sigmoid(-y*depth+x) * ( g*gravity ) "    # x=-500, 1-sigmoids inverted
    model <- nls(formula = paste0("thickness ~ ", multimodel_formula), data = model_matrix,
                 start = list(g = 1,
                              x=-500, y=1,
                              distA = dist_coef["(Intercept)"],
                              distB = dist_coef["dist_shore"]
                              # bidistA = bidist_coef["(Intercept)"],
                              # bidistB = bidist_coef["bidist_shelf"]
                 ),
                 trace = T, weights = 1/training_data.vect[["point_dens"]][,1])
    summary(model)

    
    
    ## Observations:
    # slope seems negligeble, at least when many features are included, and seems to be indicated as such by models that are better in quality
    # flow_acc same and makes artifacts
    
    # wdist_glaciers is no substitute for dist_shore
    
    
    prediction <- terra::predict(prediction_features.rast, model)   # type = "response", se.fit = T, interval = "confidence"
    png(paste0("./Predictions/",gsub('[*]', '', multimodel_formula), ".png"), height = 1600, width = 1400)
    plot(prediction, range = c(0, 1200), col=hcl.colors(256, "plasma"))
    dev.off()
    
}


# Run the application 
shinyApp(ui = ui, server = server)

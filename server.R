source("beta_plan_lib.R")
require(VGAM)
require(DT)

# Function to find a optimal  sampling plan
find_optimal_plan <- function(p1, alpha, p2, beta, SL, sigma = NA, theta = NA, 
                              distribution = c("Beta", "Normal"),
                              limtype = c("lower", "upper"), uom, uom_mapping,
                              me_adjust_enabled = NA, me_type = NA, 
                              r = NA, R = NA, sdtype) 
  {
  
  distribution <- "Beta"
  limtype <- match.arg(limtype)
  
  set.seed(123)
  if (distribution == "Beta") {
    LSL = USL = NULL
    if(limtype == "lower") {
      LSL = SL
    }
    else if (limtype == "upper") {
      USL = SL
    }
    
    plan <- optPlan(PRQ = p1, CRQ = p2, alpha = alpha, beta = beta, 
               distribution = "beta", theta = theta, 
               theta_type = "known", LSL = LSL, USL = USL)
    #plan <- find_beta_plan(p1, alpha, p2, beta, theta, SL, limtype,
    #                       grid_step = c(0.5, 0.02))
    if (is.null(plan))   return(NULL) 
    
    # plots <- plot_OC_curve_ggplot(plan$sample_size, plan$k, 
    #                               theta, SL, limtype, uom, uom_mapping, 
    #                               plan$PRQ, plan$PR, plan$CRQ, plan$CR)
    
    plots <- plot_OC_curve(plan, uom, uom_mapping)
    
    opt_table_data <- data.frame(
      # Ensures scientific notation
      SL = if (SL < 1e-4 || SL > 1e+4) 
        formatC(SL, format = "e", digits = 1) else SL,
      theta = if (plan$theta < 1e-4 || plan$theta > 1e+4) 
        formatC(plan$theta, format = "e", digits = 1) else plan$theta,
      m = plan$m, k = plan$k, 
      PR = plan$PR, PRQ = paste0(round(plan$PRQ * 100,3), "%"), 
      CR = plan$CR, CRQ = paste0(round(plan$CRQ * 100,3), "%")
    )
    
    opt_table_caption <- paste("Beta-based sampling plan with", sdtype, "theta.")
    
    #print(opt_table_data)
  }
  # else { # Follow Normal-based plan
  #   if (me_adjust_enabled) {
  #     plan <- find_normal_plan_mu_adjusted(p1, alpha, p2, beta, 
  #                                          sigma, SL, limtype, sdtype, R, r) 
  #   }
  #   else {
  #     plan <- find_normal_plan(p1, alpha, p2, beta, 
  #                              sigma, SL, limtype, sdtype)  
  #   }
  #   
  #   if (is.null(plan))   return(NULL) 
  #   
  #   plots <- plot_OC_curve_normal_ggplot(plan$n, plan$k,
  #                                        sigma, sdtype,
  #                                        SL, limtype, 
  #                                        plan$PRQ, plan$PR, plan$CRQ, plan$CR)
  #   
  #   opt_table_data <- data.frame(  #uom
  #     # Ensures scientific notation
  #     SL = if (plan$sl_value < 1e-4 || plan$sl_value > 1e+4) 
  #       formatC(plan$sl_value, format = "e", digits = 1) else plan$sl_value,
  #     sigma = if (plan$sigma_value < 1e-4 || plan$sigma_value > 1e+4) 
  #       formatC(plan$sigma_value, format = "e", digits = 1) else plan$sigma_value,
  #     n = plan$n, k = plan$k, 
  #     PR = plan$PR, PRQ = paste0(round(plan$PRQ * 100,3), "%"), 
  #     CR = plan$CR, CRQ = paste0(round(plan$CRQ * 100,3), "%")
  #   )
  #   opt_table_caption <- paste("Normal-based sampling plan with", sdtype, "sigma.")
  # }
  
  # if(me_adjust_enabled){
  #   opt_table_caption <- paste("Measurement errors-adjusted", opt_table_caption)
  # }
  # Create list data to be displayed at frontend
  return(list(plots = plots, 
              opt_table_data = opt_table_data, 
              opt_table_caption = opt_table_caption)
         )
}

shinyServer(function(input, output, session) { 
  
  # Check required packages
  required_pkg <- c("shiny", "shinyjs", "shinyWidgets", "VGAM", "AccSamplingDesign")
  
  missing_pkgs <- required_pkg[!sapply(required_pkg, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) > 0) {
    showModal(modalDialog(
      title = "Missing Package(s)",
      paste("R package(s) are required but not installed:", 
            paste(missing_pkgs, collapse = ", ")),
      easyClose = TRUE
    ))
    return()  # Stop app initialization if packages are missing
  }
  
  processing <- reactiveVal(FALSE)
  plan <- NULL
  
  observeEvent(input$find_plan, {
    shinyjs::show("loading_div")
    shinyjs::hide("find_plan")
    shinyjs::hide("planOptTable")
    shinyjs::hide("ocTabs")
    shinyjs::hide("found_plan") 
    processing(TRUE)
    
    # Start count processing time
    start_time <- Sys.time()

    #details of plan to be evaluated
    meval=input$meval
    keval=input$keval
    
    # Parameter reading and plan calculation remains the same
    SL= input$sl_value #divide by uom
    theta= input$theta_value
    sigma= input$sigma_value #not needed
    p1= input$PRQ/100
    p2= input$CRQ/100
    alpha= input$PR
    beta= input$CR
    #distribution = input$distribution
    #sdtype = tolower(input$sdtype) # This set sigma/theta known or unknown
    sdtype="Known"
    limtype= tolower(input$sl_type)
    # me_adjust_enabled = input$me_toggle
    # me_type = input$me_type
    # r = input$me_r
    # R = input$me_R
    
    uom_mapping <- dplyr::case_when(
      input$uom == "proportion" ~ 1,
      input$uom == "custom" ~ as.double(input$custom_uom_mapping),
      input$uom == "parts per hundred (%)" ~ 100,
      input$uom == "parts per million (ppm)" ~ 1e6,
      input$uom == "parts per billion (ppb)" ~ 1e9,
      )
    
    if (input$uom == 'custom'){
      uom = input$custom_uom_label
      uom_mapping = input$custom_uom_mapping
    } else {
      uom=input$uom
      }

    SL=SL/uom_mapping
    
    # Now find and optimal plan base on input parameters
    opt_plan <- find_optimal_plan(p1, alpha, p2, beta, SL,
                                  sigma, theta, 
                                  distribution, limtype,uom,uom_mapping,
                                  me_adjust_enabled, me_type,
                                  r, R, sdtype)
    
    # End count processing time
    elapsed_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
    
    if(is.null(opt_plan)) {
      # Inform the user via a modal dialog (or update a text output if preferred)
      showModal(modalDialog(
        title = "No Optimal Plan Found",
        paste("No optimal plan could be determined based on the input parameters after", 
              elapsed_time, "seconds."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
      # Reset the UI elements so the user can try again
      processing(FALSE)
      shinyjs::hide("loading_div")
      shinyjs::show("find_plan")
      shinyjs::hide("planOptTable")
      shinyjs::hide("ocTabs")
      shinyjs::hide("found_plan") 
    } 
    else {
      # Load data from plans
      plots <- opt_plan$plots
      opt_table_data <- opt_plan$opt_table_data 
      opt_table_caption <- opt_plan$opt_table_caption
      
      # Create separate outputs for each plot
      output$ocPlot1 <- renderPlot({ plots$p1 })
      output$ocPlot2 <- renderPlot({ plots$p2 })
      #output$ocPlot3 <- renderPlot({ plots$p3 })
      
      output$planOptTable <- renderTable({
        #print(opt_table_data)
        opt_table_data
      }, rownames = FALSE, digits = 3, caption = paste("An optimal plan was found after", elapsed_time, "seconds."))
      
      # Optimal plan found
      output$plan_message <- renderText({
        opt_table_caption
      })
      
      processing(FALSE)
      shinyjs::hide("loading_div")
      shinyjs::show("find_plan")
      shinyjs::show("planOptTable")
      shinyjs::show("ocTabs")
      shinyjs::show("found_plan")  # Show the message div
    }
  })
  
  
  output$dynamicTabs <- renderUI({
    tabs <- list(
      tabPanel("Acceptance Plan", 
               br(), 
               actionButton("find_plan", "Find Plan"),
               hidden(
                 div(id = "found_plan", style = "text-align: center; margin-top: 20px;",
                     h5(textOutput("plan_message"))
                 ),
                 br(),
                 div(id="planOptTable", 
                     style = "display: flex; justify-content: center;",
                     tableOutput("planOptTable")),
                 div(id = "ocTabs",
                     tabsetPanel(
                       tabPanel("Plot 1", plotOutput("ocPlot1")),
                       tabPanel("Plot 2", plotOutput("ocPlot2"))#,
                       #tabPanel("Plot 3", plotOutput("ocPlot3"))
                     )
                 ),
                 div(id = "loading_div",
                     h5("Finding an optimal plan ...", id = "loading_text"),
                     tags$div(class = "spinner-border", role = "status")
                 )
               )
      ),
      tabPanel("Risk settings",
        
               sliderInput("PR", "PR (Producer's Risk)", min = 0, max = 0.10, value = 0.05, step = 0.01),
               sliderInput("CR", "CR (Consumer's Risk)", min = 0, max = 0.20, value = 0.10, step = 0.01),
        ),
      
      tabPanel("Parameter Estimation", 
                br(), 
                fileInput("file", "Upload Dataset", 
                          accept = c(".csv", ".txt")),
               DTOutput("dataPreview"),
               br(),
               h4("Estimated value of theta"),
      
      textOutput("theta"))
    )
  
   do.call(tabsetPanel, c(id = "tabs", tabs))
   
  })
  
  output$dataPreview <- renderDT({
    req(input$file)
    df <- read.csv(input$file$datapath)
    datatable(df, options = list(pageLength = 10))
  })
  
  output$theta=renderText({
  
  req(input$file)
  df <- read.csv(input$file$datapath)
  
  uom_mapping <- dplyr::case_when(
    input$uom == "proportion" ~ 1,
    input$uom == "custom" ~ as.double(input$custom_uom_mapping),
    input$uom == "parts per hundred (%)" ~ 100,
    input$uom == "parts per million (ppm)" ~ 1e6,
    input$uom == "parts per billion (ppb)" ~ 1e9,
  )
  
  bdata <- data.frame(y = df[,1]/uom_mapping, shape1 = exp(0),
                                     shape2 = exp(1))
     
       fit1 <- vglm(y ~ 1, betaff, data = bdata, trace = FALSE)
       theta=Coef(fit1)[2] # Useful for intercept-only models
       theta
})
  
})
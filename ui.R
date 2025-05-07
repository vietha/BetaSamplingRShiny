
# Load libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)

shinyUI(fluidPage(
  titlePanel("Sampling Plans for Compositional Data"),
  useShinyjs(),  # Enable shinyjs
  
  sidebarLayout(
    sidebarPanel(
      
      # h4("Evaluated Plan"),
      # 
      # numericInput("meval", "Number of (Sub-)samples (m) ", value = 2, min = 0, max = 100, step = 1),
      # numericInput("keval", "Acceptability constant (k) ", value = 1.5, min = 0.5, max = 3, step = 0.05),
      
      h4("Designed Plan"),
      
      numericInput("PRQ", "PRQ (Producer's Risk Quality level %) ", value = 2.5, min = 0, max = 100, step = 0.5),
      #sliderInput("PR", "PR (Producer's Risk)", min = 0, max = 0.10, value = 0.05, step = 0.01),
      numericInput("CRQ", "CRQ (Consumer's Risk Quality level %) ", value = 10, min = 0, max = 100, step = 0.5),
      #sliderInput("CR", "CR (Consumer's Risk)", min = 0, max = 0.20, value = 0.10, step = 0.01),
      
      selectInput(
        "uom", "Units of Measurement:",
        c(
          "proportion",
          "parts per hundred (%)",
          "parts per million (ppm)",
          "parts per billion (ppb)",
          "parts per trillion (ppt)",
          "custom"
        )
      ),
      # if custom, define label and mapping
      conditionalPanel(
        condition = "input.uom == 'custom'",
        textInput("custom_uom_label", "Unit label", value = "microgram per 100g (μg/100g)"),
        numericInput("custom_uom_mapping", "Mapping from 0-1 scale to measurement scale:", value = 1e8)
      ),
      
      h4("Specification Limit (SL)"),
      #selectInput("sl_type", "Specification Limit (SL) Type", choices = c("Lower", "Upper")),
      numericInput("sl_value", label = NULL, value = 10e-06, min = 0, step = 0.01),
      radioButtons("sl_type", label = NULL, choices = c("Lower", "Upper"), inline = TRUE, selected = "Lower"),

      # h4("Assumption Distribution"),
      # selectInput("distribution", label = NULL, choices = c("Beta", "Normal")),
      # 
      # Conditional panels for Normal and Beta distributions
      # conditionalPanel(
      #   condition = "input.distribution == 'Normal'",
      #   #radioButtons("sdtype", "Sigma", choices = c("Known", "Unknown"), inline = TRUE, selected = "Known"),
      #   numericInput("sigma_value", "Sigma Value", value = 1, min = 0, step = 0.05)
      # ),
      # 
      # conditionalPanel(
      #   condition = "input.distribution == 'Beta'",
      #   #radioButtons("sdtype", "Theta", choices = c("Known", "Unknown"), inline = TRUE, selected = "Known"),
        numericInput("theta_value", "Precision Parameter", value = 4.4e+07, min = 0, step = 0.01),
      #),
      #radioButtons("sdtype", label = NULL, choices = c("Known", "Unknown"), inline = TRUE, selected = "Known"),
      
      # h4("Measurement Error Adjustment"),
      # switchInput("me_toggle", label = "Enable Adjustment", value = FALSE),
      # conditionalPanel(
      #   condition = "input.me_toggle == true",
      #   radioButtons("me_type", "Error Type", choices = c("Additive", "Multiplicative")),
      #   numericInput("me_r", "Repeatability (r)", value = 0.01, min = 0, step = 0.005),
      #   numericInput("me_R", "Reproducibility (R)", value = 0.03, min = 0, step = 0.005)
      # )
    ),
    
    mainPanel(
      uiOutput("dynamicTabs")
    )
  )
))


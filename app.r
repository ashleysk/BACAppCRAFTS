# Loading necessary packages to build UI. 
library(shiny)
library(bs4Dash) # Extra UI components. 
library(shinyWidgets)
library(bslib)
library(plotly) #For Widmark plot after user inputs values.

# Defining the UI components.
ui <- bs4DashPage(
  title = "CRAFTS Lab Blood Alcohol Content (BAC) Calculator Prototype",
  fullscreen = TRUE,
  
  # Creating a brief loading screen. 
  preloader = list(html = tagList(
    div("Loading CRAFTS BAC Calculator..."),
    br(), br(),
    div(style = "color: red; font-style: italic;",
        "Please note: This calculator is for educational purposes only and is not a valid
        resource for professional use (i.e. in courtrooms). Use with the awareness that mistakes may be present.")
  )),
  
  # Header creation with CRAFTS logo.
  header = bs4DashNavbar( 
    title = dashboardBrand(
      title = tags$span("CRAFTS Lab Blood Alcohol Content (BAC) Calculator - Prototype",
                        style = "font-size: 20px; white-space: normal"),
      color = "lightblue",
      image = "crafts.png"
    )
  ), # Contains title of entire project. 
  
  # Sidebar - For tool description, instructions & limitations...
  sidebar = bs4DashSidebar(
    status = "primary",
    width = "380px",
    
    
    sidebarMenu(
      tags$h6(style = "font-weight: bold;", "Method 1a: The Widmark Equation"),
      
      menuItem("(1) Tool Description", tabName = "desc"),
      menuItem("(2) Tool Instructions", tabName = "instruct"),
      menuItem("Widmark Calculator", tabName = "widm"),
      menuItem("(3) Widmark Limitations", tabName = "limit"),
      
      tags$h6(style = "font-weight: bold;", "Method 1b: Simulating Widmark with Ranges"),
      
      menuItem("Normal Curve Calculator", tabName = "desc2"),
      
      tags$h6(style = "font-weight: bold;", "Method 2: Pharmokinetic (PBPK) Models of Alcohol"),
      
      menuItem("(1) Tool Description", tabName = "desc3"),
      menuItem("(2) Tool Instructions", tabName = "instruct3"),
      menuItem("PBPK Calculator", tabName = "pbpk"),
      
      tags$h6(style = "font-weight: bold;", "Supplementary Information"),
      menuItem("Contact", tabName = "contact"),
      menuItem("References", tabName = "ref")
    )
  ),
  
  # Body
  body = bs4DashBody(
    tabItems(    
      bs4TabItem(     
        tabName = "desc",
        tags$h5(style = "font-weight: bold;", "(1) Description of Tool"),
        
        card(
          card_header(
            tags$h6(style = "font-weight: bold", "The Widmark Equation: What it Measures")
          ),
          card_body(
            tags$h6("    Welcome to the CRAFTS lab Blood Alcohol Content (BAC) calculator prototype!
                    The following section will briefly describe the components of the Widmark equation and its uses in both forensic and
                    non-forensic contexts."),
            br(), 
            tags$h6("    This equation is the most popular method for a simple blood alcohol content (BAC) calculation. After the
                   consumption of alcohol, BAC is the value used to estimate the approximate percentage of this subtance per each 100ml of an individual's blood [1].
                    Most tools used to report this value (i.e. professional grade brethalyzers used by police for impaired driving cases) report this value in a similar manner.
                    A simple interpretation using the Canadian legal limit (BAC = 0.08) is illustrated below: "),
            br(),
            tags$h6(style = "font-style: italic; color: red;", "'If a brethalyzer reads a BAC value of 0.08, this means that each 100ml of the
                    individual's blood is composed of approximately 0.08% alcohol.'"),
            br(),
            tags$h6("    While there are a few limitations to using the Widmark Equation as the primary estimator of BAC-- see section (3) for details--,
                    a detailed snapshot of the equation along with descriptions of each component are pictured below.")
          ),
          tags$div(
            style = "text-align: center;",
            tags$img(src = "WidmarkE.png", width = "900px")
          ),
          br(),
          tags$h6("The following section will describe this equation in further detail."),
          card_header(
            tags$h6(style = "font-weight: bold", "How Equation Components are Determined and Utilized")
          ),
          card_body(
            fluidRow(
              column(4, 
                     tags$h6(style = "font-weight: bold; color: green;", "Alcohol Consumed (g):"),
                     br(),
                     tags$img(src = "Standard Drink Volumes.png", width = "650px")),
              br(), br(),
              column(4, 
                     tags$h6(style = "font-weight: bold; color: darkred;", "Body Weight (g)"),
                     br(), 
                     tags$img(src = "weight.png", width = "500px")),
              br(), br(),
              column(4, 
                     tags$h6(style = "font-weight: bold; color: teal; ", "R-value (r):"),
                     br(),
                     tags$img(src = "gender.png", width = "500px"))
            )
          )
        )
      ),
      
      bs4TabItem(
        tabName = "instruct",
        tags$h5(style = "font-weight: bold;", "(2) Instructions For Use"),
        card( 
          card_header(
            tags$h6(style = "font-weight: bold;", "Widmark Calculator Steps")
          ),
          fluidRow(
            column(4,
                   tags$h6(style = "font-weight: bold;", "Step 1."),
                   tags$h6("Fill in each field with the required information. Use your best approximation if exact values are not known."),
                   br(), br(),br(), br(),
                   tags$h6(style = "font-weight: bold;", "Step 2."),
                   tags$h6("When all fields are completed, press the 'Calculate my BAC!' button for your approximate blood alcohol concentration (BAC) value."),
                   br(), br(),br(),
                   tags$h6(style = "font-weight: bold;", "Step 3."),
                   tags$h6("View the plot below containing supplementary information about the following:"),
                   br(),
                   tags$h6(style = "font-weight: bold; color: blue;", "Blue Line:"),
                   tags$h6(" BAC decline over time given a constant rate of 0.016/hr."),
                   br(),
                   tags$h6(style = "font-weight: bold; color: purple;", "Purple Line:"),
                   tags$h6(" The number of hours after your first drink at which your BAC will return to zero."),
                   br(),
                   tags$h6(style = "font-weight: bold; color: green;", "Green Line:"),
                   tags$h6(" Your current BAC content given the number of hours since your first drink."),
                   br(),
                   tags$h6(style = "font-weight: bold; color: red;", "Red Line:"),
                   tags$h6("The legal alcohiol limit of 0.08 (& how your current BAC compares).")
            ),
            column(6, 
                   img(src = 'Instructions1.png', width = 400),
                   br(), br(),
                   img(src = 'Instructions2.png', width = 600),
                   br(), br(), br(), br(), br(), br(), br(),
                   img(src = 'Instructions3.png', width = 800)
            )
          )
        )
      ),
      
      bs4TabItem(
        tabName = "widm",
        tags$h5(style = "font-weight: bold;", "Method 1: Widmark Equation Method"),
        card(
          tags$h5(style = "font-weight: bold;", "Gender"),
          tags$h6("Please select your sex:"),
          fluidRow(
            column(6,
                   selectInput("gender", "", choices = list("Female" = 1, 
                                                            "Male" = 2), 
                               selected = 1)),
            column(6,
                   tags$p(style = "font-style: italic;",
                          "*Note: This calculator assumes that the input corresponds to 
                          biological sex. The limitations of this method in calculating BAC for 
                          both transgender and intersex populations are outlined in (3)."))
          ),
          tags$h5(style = "font-weight: bold;", "Weight (lbs)"),
          
          tags$h6("Please select your approximate body weight (lbs):"),
          
          sliderInput("weightlbs", "", min = 70,
                      max = 500,
                      value = 150,
                      step = 10),
          
          tags$h5(style = "font-weight: bold;", "Hours Since First Drink"),
          tags$h6("Please input the approximate number of hours since your first drink:"),
          
          numericInput("hsfd", "",
                       min = 0,
                       max = 24,
                       value = 0,
                       step = 0.25),
          
          tags$h5(style = "font-weight: bold;", "Standard Drinks"),
          
          tags$h6("Please enter the approximate number of standard drinks consumed from the following list:"),
          
          fluidRow(
            column(4, numericInput("beerorCooler", "Beer/Cooler (341mL/~5% AbV)", min = 0, max = 24, value = 0, step = 0.25)),
            
            column(4, numericInput("wine", "Wine (142mL/~12% AbV)",
                                   min = 0,
                                   max = 24,
                                   value = 0,
                                   step = 0.25)),
            
            column(4, numericInput("liquor", "Liquor (43mL/~40% AbV)",
                                   min = 0,
                                   max = 24,
                                   value = 0,
                                   step = 0.25))
            
          ),
          actionButton("calculate", "Calculate my BAC!"),
          verbatimTextOutput("resultingBAC")
        ),
        plotlyOutput("BACplot")
      ),
      
      bs4TabItem(
        tabName = "limit",
        tags$h5(style = "font-weight: bold;", "(3) Limitations & Alternative Methods"),
        card(
          card_header(
            tags$h6(style = "font-weight: bold;", "Three Important Downfalls of Widmark")
          ),
          
          fluidRow(
            column(4, 
                   tags$h6(style = "font-weight: bold; color: darkred;", "1. Assumes A Gender Binary"),
                   br(),
                   tags$img(src = "binary.png", width = "600px")),
            br(), br(),
            column(4, 
                   tags$h6(style = "font-weight: bold; color: darkred;", "2. Lack of Consideration for Ethanol Metabolization"),
                   br(), 
                   tags$img(src = "metabolism.png", width = "600px")),
            br(), br(),
            column(4, 
                   tags$h6(style = "font-weight: bold; color: darkred; ", "3. Does Not Address Intervals Between Drinks"),
                   br(),
                   tags$img(src = "interval.png", width = "600px"))
          )
        )
      ),
      
      bs4TabItem(
        tabName = "desc2",
        tags$h5(style = "font-weight: bold;", "Adressing Widmark's Problems"),
        card(
          card_header(
            tags$h6(style = "font-style: italic;", "But, how precise is Widmark really?"),
      
          ),
          
          card_body(
            tags$h6("Lorem ipsum..."),
            
            uiOutput("UIsummary"), # User inputs
            plotlyOutput("BACsimPlot"), #Plot
            verbatimTextOutput("simStatistics") #Summary stats
          )
        )
      ),
      
      bs4TabItem(
        tabName = "desc3",
        tags$h5(style = "font-weight: bold;", "Method 2 Description"),
        card(
          card_header(
            tags$h6(style = "font-weight: bold;", "(Coming soon...)")
          )
        )
      ),
      
      bs4TabItem(
        tabName = "instruct3",
        tags$h5(style = "font-weight: bold;", "Method 2 Instructions"),
        card(
          card_header(
            tags$h6(style = "font-weight: bold;", "Instructions (Coming soon...)")
          )
        )
      ),
      
      bs4TabItem(
        tabName = "pbpk",
        tags$h5(style = "font-weight: bold;", "Method 2: Post-Mortem Correction"),
        card(
          tags$h5(style = "font-weight: bold;", "Coming soon...")
        )
      ),
      
      bs4TabItem(
        tabName = "contact",
        tags$h5(style = "font-weight: bold;", "Contact Information"),
        tags$h6("Name: Ashley King"),
        tags$h6("Title: Research Assistant in CRAFTS Laboratory - Trent University"),
        tags$h6("Email: asking@trentu.ca")
      )
    )
  ),  
  
  # Footer
  footer = bs4DashFooter(
    left = "CRAFTS Lab 2025",
    right = "Version 1.0"
  )
)  

# Server logic
server <- function(input, output, session) {
  
  Sys.sleep(4)
  
  r_value <- reactive({
    req(input$gender)
    if (input$gender == 1) return(0.55) else return(0.68)
  })
  
  weightinGrams <- reactive({
    input$weightlbs * 453.59237
  })
  
  BACdecline <- reactive({
    input$hsfd * 0.016
  })
  
  alcoholDose <- reactive({
    beer <- 341 * input$beerorCooler * 0.05 * 0.789
    wine <- 142 * input$wine * 0.12 * 0.789
    liquor <- 43 * input$liquor * 0.40 * 0.789
    beer + wine + liquor
  })
  
  observeEvent(input$calculate, {
    BAC <- ((alcoholDose() / (weightinGrams() * r_value())) * 100) - BACdecline()
    if (BAC < 0) BAC <- 0
    output$resultingBAC <- renderText({
      paste("Your estimated blood alcohol content (BAC) is:", round(BAC, 2), "%. See the resulting graph for further details.")
    })
  })
  
  bac_info <- eventReactive(input$calculate, {
    r <- r_value()
    alcohol_ingrams <- alcoholDose()
    weight_ingrams <- weightinGrams()
    hours <- seq(0, 12, by = 0.5)
    bactotal = ((alcohol_ingrams / (weight_ingrams * r)) * 100) - 0.016 * hours
    bactotal = pmax(bactotal, 0)
    data.frame(hours = hours, bac = bactotal)
  })
  
  output$BACplot <- renderPlotly({
    req(bac_info())
    bac_zero <- ((alcoholDose() / (weightinGrams() * r_value())) * 100) / 0.016
    initial_bac <- (alcoholDose() / (weightinGrams() * r_value())) * 100
    hours_legal_limit <- (initial_bac - 0.08) / 0.016
    
    p <- ggplot(bac_info(), aes(x = hours, y = bac)) +
      geom_line(aes(color = "BAC Decline Over Time"), linewidth = 1.5) +
      geom_point(aes(x = hours_legal_limit,
                     y = 0.08,
                     color = "Legal Limit"),
                 shape = 8,
                 size = 6) +
      
      geom_vline(aes(xintercept = input$hsfd, color = "Current BAC"),
                 linetype = "dashed", linewidth = 1.2) +
      
      geom_vline(aes(xintercept = bac_zero, color = "BAC Zero Point"),
                 linetype = "dashed", linewidth = 1.2) +
      
      labs(title = "Estimated BAC Changes Since First Drink",
           x = "Hours Since First Drink",
           y = "Blood Alcohol Content (BAC)") + 
      
      scale_color_manual(name = "Legend",
                         values = c("BAC Decline Over Time" = "blue",
                                    "Legal Limit" = "red",
                                    "Current BAC" = "green",
                                    "BAC Zero Point" = "purple")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
    
    ggplotly(p)
    
    
  })
  
  #For method 1b, simulating a normal curve using values that deviate 10% from what the user inputs. 
  bacSim = function(userInput, n = 100, percentRange = 0.10){ #Creating a function for this purpose. 100 values simulated.
    
    lowVal = userInput * (1 - percentRange)
    highVal = userInput * (1 + percentRange) #Defining the low and high values that numbers will be pulled from.
    
    #Ensuring that approx. 95% of the simulated values fall within 2 SD of the user input. 
    standardDev = (highVal - lowVal) / 4
    
    #Generating the randomized values from the distribution around the user input. 
    simulateCurve = rnorm(n, mean = userInput, sd = standardDev)
    
    #Capping the simulated values such that no numbers fall outside of the low and high numbers.
    #If a number does fall outside, it will be set to either the low or high value. 
    simulateCurve = pmax(pmin(simulateCurve, highVal), lowVal)
    return(simulateCurve)
    
    
  }
  
  #For Method 1b, creating a reactive() section that returns the simulated values for each of the 4 Widmark components.
  simulatedSections = reactive({
    
    list(
      alc = bacSim(alcoholDose()),
      bodyWeight = bacSim(input$weightlbs),
      hoursSince = bacSim(input$hsfd),
      r_val = bacSim(r_value())
      
    )
    
    
  })
  
  #Using these vals to calculate a BAC for each of the 100 simulated values for each component. 
  bacCalc = reactive({
    inputs = simulatedSections() #Grabbing inputs
    
    #Converting the weight from lbs to gs 
    weightGs = inputs$bodyWeight * 453.59237
    
    #Doing the BAC calculations
    BACs = ((inputs$alc / (weightGs * inputs$r_val)) * 100) - (inputs$hoursSince * 0.016)
    
    #Making sure 'negative' BACs are no higher than 0 
    BACs = pmax(BACs, 0)
    return(BACs)
    
    
  })
  
  #Creating a summary of what the user chose
  output$UIsummary <- renderUI({
    req(input$beerorCooler, input$wine, input$liquor, input$weightlbs, input$hsfd, input$gender)
    
    #Output gender name given what they user chooses instead of the numbers
    genderText <- ifelse(input$gender == 1, "Female", "Male")   
    
    tagList(
      tags$h5("Summary of Your Alcohol Consumption & Inputs:"),
      tags$ul(
        tags$li(strong("For your gender, you chose: "), " ", genderText),
        tags$li(strong("For your weight, you chose: "), " ",  input$weightlbs, "lbs"),
        tags$li(strong("The number of hours since your first drink: "), " ", input$hsfd),
        tags$li(strong("Your approximate number of beer/cooler drinks: "), " ", input$beerorCooler),
        tags$li(strong("Your approximate number of wine drinks: "), " ", input$wine),
        tags$li(strong("Your approximate number of hard liquor drinks"), " ", input$liquor)
        
        
      )
    )
  })
  
  #Using this calc to visualize a density plot of all of the simulated BAC values. 
  output$BACsimPlot = renderPlotly({
    
    req(bacCalc())
    
    #Creating data frame w/ simulated BACs
    df = data.frame(BACs = bacCalc())
    
    #Making plot 
    p = ggplot(df, aes(x = BACs)) +
      geom_density(fill = "darkgreen", alpha = 0.05)+
      labs(title = "BAC Distribution Within 10% Range of User Input",
           x = "BAC Value (%)",
           y = "Density")+
      theme_minimal()+
      theme( #Centering the title
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
    
    ggplotly(p)
    
  })
  
  
  #Placing some summary statistics below the plot for additional context
  output$simStatistics = renderText({
    req(bacCalc())
    
    BACvalues = bacCalc()
    
    paste0(
      "Median (Widmark-given) BAC:", round(median(BACvalues), 2), "\n",
      "95% Confidence Interval: [", round(quantile(BACvalues, 0.025), 2),
      ",", round(quantile(BACvalues, 0.975), 2), "]"
      
      
    )
    
    
  })
}

# Running the application
shinyApp(ui = ui, server = server)



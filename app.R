# Loook: A tool for visualizing and analyzing infant looking time data
# Author: Shari Liu
# Last edited: Dec 5, 2017

# required packages
library(boot)
library(lme4)
library(lmerTest)
library(psych)
library(shiny)
library(tidyverse)


# Define UI for application --------
ui <- fluidPage(
   
  # Application title
  titlePanel(paste(intToUtf8(0x1F50D),"Loook: A tool for visualizing and analyzing infant looking time data")),

    # Sidebar with input functions for dataset and variables
    sidebarLayout(
      
    sidebarPanel(
      
        # Dataset specification ---------
        h3("Step 1. Upload Data"),
        helpText("Upload your",
                 tags$a(href = "http://r4ds.had.co.nz/tidy-data.html", "tidy"),
                 "dataset in .csv format, or use the default dataset from ",
                 tags$a(href= "https://www.shariliu.com/hubfs/shariliu.com/papers_and_docs/liu_spelke_2017_cognition.pdf?t=1512485758167", "Liu & Spelke (2017), Exp. 1.")),
        
        # Input: Select default dataset, or upload personal dataset
        radioButtons("dataset", "Select a dataset",
                     c("default", "upload my own"), inline = TRUE),
        
        # Input: Upload dataset, only if user indicates
        conditionalPanel("input.dataset === 'upload my own'",
                         helpText("Make sure that looks are contained in column called 'look'"),
                         fileInput(inputId = "userdata",
                                   label = "Choose CSV file",
                                   multiple = FALSE,
                                   accept = c("/text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         selectize = FALSE),
        
        # Input: Specify the number of observations to view in summary panel
        numericInput("obs", "Number of observations to view:", 10),
        
        # Clarifying text
        helpText("Note: while the data view will show only the specified",
                 "number of observations, the summary will still be based",
                 "on the full dataset."),
        
        # Input: Defer the rendering until user clicks the button
        actionButton("updatedata", "Update dataset"),
        
        tags$hr(),
        
        # Variable specification --------------
        h3("Step 2. Specify Variables"),
        
        # Input: Select column with looking times 
        selectInput("look", 
                    label = "Variable containing looks",
                    choices = NULL,
                    selected = NULL),
        
        # Input: Select column with fixed effect
        selectInput("type", 
                    label = "Variable containing fixed effect of interest (e.g. expected/unexpected)",
                    choices = NULL,
                    selected = NULL),
        
        # Input: Select column with random effect 1 (subject ID)
        selectInput("subj", 
                    label = "Variable containing subject IDs",
                    choices = NULL,
                    selected = NULL),
        
        # Input: Select column with random effect 2 (optional)
        selectInput("rand", 
                    label = "Optional: variable containing additional random effect",
                    choices = NULL,
                    selected = NULL),
      
        # Input: Select transformation of data
        checkboxInput(inputId = "log",
                      label = "Log-transform looks?",
                      value = FALSE),
      
        # Input: Defer the rendering of output until user clicks
        actionButton("updatevars", "Update variables"),
        
        tags$hr(),
        
        # Info about how to cite program
        helpText("To cite: Liu, S. (2017). Loook: A tool for visualizing and analyzing infant looking time data. [Computer software]. Retrieved from https://shariliu.shinyapps.io/loook/")
    ),
      
    # Main panel with output functions --------------
    mainPanel(
      
      tabsetPanel(
        
        # 4 panels to summarize, visualize, analyse using parametric methods, and analyses using non-parametric methods
        # Includes helpful references for the user
        tabPanel("summarize",
                 h4("Obervations"),
                 tableOutput("dataTable"),
                 h4("Data Structure"),
                 verbatimTextOutput("structure"),
                 h4("Summary"),
                 helpText(tags$strong("References: "),
                          tags$a(href = "https://www.rdocumentation.org/packages/psych/versions/1.7.8/topics/describeBy", "describeBy")
                 ),
                 tableOutput("summary")),
        
        tabPanel("visualize",
                 h4("Density Plot of Looks"),
                 tags$p("If looking times appear ",
                          tags$a(href = "https://en.wikipedia.org/wiki/Log-normal_distribution", "lognormally distributed"),
                          " consider performing analyses on log-transformed data, or use nonparametric methods"),
                 helpText(tags$strong("References: "),
                 tags$a(href = "https://www.researchgate.net/profile/Denis_Tatone2/publication/292990919_Statistical_Treatment_of_Looking-Time_Data/links/56eaa73008aee3ae24a26088/Statistical-Treatment-of-Looking-Time-Data.pdf", "Csibra et al. (2016), "),
                 tags$a(href = "http://ggplot2.tidyverse.org/","ggplot2, "),
                 tags$a(href = "http://ggplot2.tidyverse.org/reference/geom_density.html", "geom_density")),
                 plotOutput("distPlot"),
                 h4("Violin and Boxplots of Looks"),
                 tags$p("Individual looks plotted in black. Means are plotted in white."),
                 helpText(tags$strong("References: "),
                 tags$a(href = "http://ggplot2.tidyverse.org/reference/geom_violin.html", "geom_violin, "),
                 tags$a(href = "http://ggplot2.tidyverse.org/reference/geom_boxplot.html", "geom_boxplot, "),
                 tags$a(href = "http://ggplot2.tidyverse.org/reference/facet_wrap.html", "facet_wrap")),
                 plotOutput("boxVioPlot")),
        
        tabPanel("lme4 analysis",
                 h3("Mixed effects models"),
                 helpText(tags$strong("References: "),
                          tags$a(href = "https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf", "Bates et al. (2015), "),
                 tags$a(href = "http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html", "GLMM FAQ, "),
                 tags$a(href = "https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/anova", "anova")),
                 tags$p("Linear mixed effects models are extensions of linear models, except that they model both fixed and random effects. ",
                    "Fixed effects describe predictors where all possible levels are included in the model. In infant data, this can describe what we manipulated (e.g. expected vs unexpected).",
                    "Random effects describe predictors where levels were drawn from a population. In infant data, this can describe subject variables (e.g. identity, sex), or counterbalanced variables (e.g. test trial order).",
                    "In these models, we essentially control for the variance of the random effect(s) specified according to our model."),
                 tags$p("Below are two mixed effects models fit to the dataset, and a comparison of these models. ",
                        tags$strong("This is not an exhaustive analysis"), "so please consider it as an example only!"),
                 h4("Model 1: Comparing looks across one fixed effect, including subjects as a random effect"),
                 verbatimTextOutput("model1"),
                 h4("Model 2: Add random intercept for additional random effect"),
                 verbatimTextOutput("model2"),
                 h4("Comparing Model 1 and Model 2"),
                 verbatimTextOutput("comparison12")),
        
        tabPanel("non-parametric analysis",
                 h4("Bootstrapped difference in medians, 5000 samples. May take a few seconds to complete!"),
                 tags$p("This method ignores structure in the data (e.g. repeated measures), so it is not as powerful as other methods,",
                 "Sample size is same as in dataset. Solid line indicates median, and dotted lines indicate 95% confidence interval."),
                 helpText(tags$strong("References: "),
                          tags$a(href = "https://en.wikipedia.org/wiki/Bootstrapping_(statistics)", "boostrapping, "),
                          tags$a(href = "https://www.rdocumentation.org/packages/car/versions/2.1-6/topics/Boot", "boot, "),
                          tags$a(href = "https://en.wikipedia.org/wiki/Confidence_interval", "confidence intervals")),
                 plotOutput("bootAnalysis"),
                 h4("Wilcoxon Signed-Rank Test"),
                 tags$p("Note: This test can only handle balanced datasets."),
                 verbatimTextOutput("wilcoxonAnalysis")
                 )
        )
    )
   )
)

# Define server logic ----------
server <- function(input, output, session) {
  
  # Require data and variables before proceeding
  reqAll <- reactive({req(input$dataset, input$look, input$type, input$subj)})
 
  # Return the requested dataset when the user clicks "Update datset" button ----
  Data <- eventReactive(input$updatedata, {
    if (is.null(input$dataset)){
      data <- NULL
    } else if (input$dataset == "default"){
      data <- read.csv("./datasets/doe_tidy.csv", header=TRUE)
    } else if (input$dataset == "upload my own"){
      data <- read.csv(input$userdata$datapath)
    }
      # log-transform looks
      data$loglook <- log(data$look)
      return(data)
  })

  # Change the values of the column headings in the UI to the column names of user-specified dataset -----
  observeEvent(input$updatedata,{
    updateSelectInput(session, "look", choices = names(Data()), selected = "look")
    updateSelectInput(session, "type", choices = names(Data()), selected = "trialtype")
    updateSelectInput(session, "subj", choices = names(Data()), selected = "subj")
    updateSelectInput(session, "rand", choices = c("none", names(Data())), selected = "testpair")
    })
  
  # Table: Show the first "n" observations ----
  output$dataTable <- renderTable({
    reqAll
    head(Data(), n = isolate(input$obs))
  })
  
  # Summary Table ----
  output$summary <- renderTable({
    reqAll
    input$updatevars
    if (isolate(input$rand) != "none" & isolate(!is.null(input$rand))){
      isolate(describeBy(Data()[[input$look]], list(Data()[[input$type]], Data()[[input$rand]]), mat = TRUE))
    }
    else {
      isolate(describeBy(Data()[[input$look]], list(Data()[[input$type]]), mat = TRUE))
    }
  })
  
  # Data Structure ----
  output$structure <- renderPrint({
    reqAll
    input$updatevars
    isolate(str(Data()))
  })
  
  # Histogram ------------
  # Do not update reactive values until the user clicks 'Update variables' button. See isolate() below.
  output$distPlot <- renderPlot({ 
    reqAll
    input$updatevars
    # Plot raw or log looks depending on user input
    if (isolate(input$log) == FALSE) {
      plot <- ggplot(data = Data(), isolate(aes_string(input$look, fill = input$type)))
    } else {
      plot <- ggplot(data = Data(), isolate(aes_string(quote(loglook), fill = input$type))) + xlab("Log-Transformed Looks")
    }
    plot + geom_density(alpha = 0.2) + theme_linedraw(15)
  })
  
  # Boxplot + Violin Plot ------------
  output$boxVioPlot <- renderPlot({
    reqAll
    input$updatevars
    if (isolate(input$log) == FALSE) {
      plot <- ggplot(data = Data(), isolate(aes_string(x = input$type, y = input$look, fill = input$type)))
    } else {
      plot <- ggplot(data = Data(), isolate(aes_string(x = input$type, y = quote(loglook), fill = input$type))) + ylab("Looking Time (log s)")
    }
    
    # Facet plot by additional random var only if user specifies
    if (isolate(input$rand) == "none" | is.null(isolate(input$rand))) {
    plot + 
      geom_violin(alpha = 0.2) +
      geom_boxplot(width = 0.1, alpha = 0.75) +
      geom_point()+
      stat_summary(fun.y = mean, geom = "point", fill = "white", shape= 23, size = 5) +
      theme_linedraw(15)+
      theme(legend.position = "none")
    } else {
      plot + 
        geom_violin(alpha = 0.2) +
        geom_boxplot(width = 0.1, alpha = 0.75) +
        geom_point()+
        stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 23, size = 5) +
        theme_linedraw(15)+
        isolate(facet_wrap(as.formula(paste("~",input$rand)), labeller = label_both))+
        theme(legend.position="none")
    }
    })
  
  # lme4 analysis: model 1 --------
  output$model1 <- renderPrint({
    reqAll
    input$updatevars
    if (isolate(input$log) == FALSE) {
      formula1 <- isolate(as.formula(paste(input$look,"~",input$type,"+(1|",input$subj,")")))
    } else {
      formula1 <- isolate(as.formula(paste("loglook~",input$type,"+(1|",input$subj,")")))
    }
    model1 <- lmer(formula1, data = Data(), REML = FALSE)
    summary(model1)
})
  
  # lme4 analysis: model 2 (only if user specified random effect) -------------
  output$model2 <- renderPrint({
    reqAll
    input$updatevars
    if (isolate(input$rand) == "none" | is.null(isolate(input$rand))){
      return("No additional random effects specified.")
    } else {
      if (isolate(input$log) == FALSE) {
        formula2 <- isolate(as.formula(paste(input$look,"~",input$type,"+(1|",input$subj,")+(1|",input$rand,")")))
      }
      else {
        formula2 <- isolate(as.formula(paste("loglook~",input$type,"+(1|",input$subj,")+(1|",input$rand,")")))
      }
      model2 <- lmer(formula2, data = Data(), REML = FALSE)
      summary(model2)
    }
  })

  # lme4 analysis: compare models 1 and 2 (only if user specified random effect) -----------
  output$comparison12 <- renderPrint({
    reqAll
    input$updatevars
    if (isolate(input$rand) == "none" | is.null(isolate(input$rand))){
      return("No additional random effects specified.")
    } else {
        if (isolate(input$log) == FALSE) {
          formula1 <- isolate(as.formula(paste(input$look,"~",input$type,"+(1|",input$subj,")")))
          formula2 <- isolate(as.formula(paste(input$look,"~",input$type,"+(1|",input$subj,")+(1|",input$rand,")")))
        }
        else {
          formula1 <- isoalate(as.formula(paste("loglook~",input$type,"+(1|",input$subj,")")))
          formula2 <- isolate(as.formula(paste("loglook~",input$type,"+(1|",input$subj,")+(1|",input$rand,")")))
        }
        model1 <- lmer(formula1, data = Data(), REML = FALSE)
        model2 <- lmer(formula2, data = Data(), REML = FALSE)
    isolate(anova(model1,model2))
    }
  })
  
  # non-parameteric analysis: boostrapping ------------------
  output$bootAnalysis <- renderPlot({
    reqAll
    input$updatevars 
    
    # save each vector of looks to a variable
    look1 <- isolate(dplyr::filter(Data(), Data()[[input$type]] == levels(Data()[[input$type]])[1])$look)
    look2 <- isolate(dplyr::filter(Data(), Data()[[input$type]] == levels(Data()[[input$type]])[2])$look)
    
    # initialized medians vector
    medians <- NULL
    
    # sample looks with replacement 5000 times, and return difference between the medians
    for (i in 1:5000) {
      medians[i] <- isolate(median(na.omit(sample(look1, length(look1), replace = T)))-median(na.omit(sample(look2, length(look2), replace = T))))
    }
    
    # density plot of bootstrapped medians
    qplot(isolate(medians), geom="density")+
      theme_linedraw(15)+
      xlab("Bootstrapped Medians")+
      
      # lines indicate median, and 95% CI
      geom_vline(xintercept=median(medians))+
      geom_vline(xintercept=quantile(medians, c(0.025)), linetype = 'dashed')+
      geom_vline(xintercept=quantile(medians, c(0.975)), linetype = 'dashed')
  })
  
  # non-parametric analysis: wilcox ------
  output$wilcoxonAnalysis <- renderPrint({
    reqAll
    input$updatevars
    formula <- isolate(as.formula(paste(input$look,"~",input$type)))
    look1 <- isolate(dplyr::filter(Data(), Data()[[input$type]] == levels(Data()[[input$type]])[1])[[input$look]])
    look2 <- isolate(dplyr::filter(Data(), Data()[[input$type]] == levels(Data()[[input$type]])[2])[[input$look]])
    wilcox.test(formula, data = Data(), paired = TRUE, conf.int = TRUE)
  })

}
# Run the application -------
shinyApp(ui = ui, server = server)


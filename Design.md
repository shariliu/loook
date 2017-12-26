# Loook: A tool for visualizing and analyzing infant looking-time data

This is a [Shiny](https://shiny.rstudio.com/) app built by a developmental psychologist, for developmental psychologists who work with infant looking time datasets. This is not meant to replace your data analysis pipeline, but rather introduces and implements useful packages and tools from R. 

This *design document* distills information widely available about how Shiny works and presents it in the context of Loook. References are listed at the bottom of each subsection.

### Overview

Like all Shiny apps, the `app.R` file for Loook has three components:

* a user interface object, `ui`, which controls layout and appearance
* a `server` function, which contains instructions for building outputs from inputs
* and a call to the `shinyApp` function, which pairs the UI and server together

References: [Shiny Tutorial Lesson 1](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/)

### Layout

Loook includes a `titlePanel`, a `sidebarLayout`, and a `mainPanel` with several `tabPanels`. 

In the sidebar, the user can use the default dataset or upload their own, and select the variables that they would like to summarize, visualize, and analyze.

In the main panel, tabulated summaries, plots, and analyses are displayed to the user.

Content is stylized using HTML5 equivalents in R.

References: [Shiny Tutorial Lesson 2](https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/)

### Getting input from the user

Loook gets user input via control widgets, which include file inputs, checkboxes, select boxes, and radio buttons. When the user interacts with the app, `ui` records and updates user input from these widgets in `input$*` variables.

Here is a list of `input$*` variables in Loook:

* `dataset`: specifies whether user wants to use our dataset or upload their own
* `userdata`: user's uploaded dataset
* `obs`: number of observations to view
* `updatedata`: update values for `dataset`, `userdata`, and `obs`
* `look`: column containing looking times
* `type`: column containing fixed effect
* `subj`: column containing subject IDs
* `rand`: optional column containing random effect
* `log`: whether raw or logged looking times should be used for plotting and parametric analyses
* `updatevars`: update values for `look`, `type`, `subj`, `rand`, and `log`

By default, input variables are reactive, which means that whenever the user interacts with the UI and changes an input variable, this is passed onto the server and the rendered ouputs are updated immediately. However, this behavior is not always desirable -- in this case, we want to wait for the user to update their data and variables before updating outputs.

Loook includes two action buttons, `input$updatedata` and `input$updatevars`, which make this possible. Throughout the `server` function, we include calls to these variables to make the server sensitive to when the user clicks these buttons, and we use the `isolate` function to prevent reactive variables from reacting to user input until these buttons are clicked.

References: [Shiny Tutorial Lesson 3](https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/)

### Rendering output

Loook takes user inputs and outputs summaries, analyses, and plots. These `ouput$*` variables are rendered using `render*` as specific classes of objects in the UI. For instance, the `renderPlot()` object works with the `plotOutput()` function.

When the user chooses a dataset, this is saved as a reactive object, `Data`. The column headings of this dataset are then passed to the UI using `updateSelectInput`, and the user can then select the variables from the dataset that they would like to work with.

Loook then takes this dataset and these variables, and renders summaries, plots, and analyses, but not until the user specifies a dataset and variable. We use the `req()` function to require certain inputs from the user to make sure that output expressions proceed only under these conditions.

Here is a list of `output$*` variables in Loook:

* `dataTable`: table of observations
* `structure`: structure of data frame
* `summary`: descriptive statistics of user-specified variables
* `distPlot`: density plot of distribution
* `boxVioPlot`: box and violin plots
* `model1`: mixed effects model 1
* `model2`: mixed effects model 2
* `comparison12`: comparison of model 1 and model 2
* `bootAnalysis`: non-parametric analysis (bootstrapping)
* `wilcoxonAnalysis`: non-parametric analysis (Wilcoxon signed rank test)

References: [Shiny Tutorial Lesson 4](https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/), [Handling missing inputs with req(...)](https://shiny.rstudio.com/articles/req.html)


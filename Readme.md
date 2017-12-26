# Loook: A tool for visualizing and analyzing infant looking time data

This is a [Shiny](https://shiny.rstudio.com/) app built by a developmental psychologist, for developmental psychologists who work with looking time datasets. This is not meant to replace your data analysis pipeline, but rather introduces and implements useful packages and tools from R. 

There are two ways of accessing Loook: by visiting [https://shariliu.shinyapps.io/loook/](https://shariliu.shinyapps.io/loook/), or by running it on your local machine (e.g. within R Studio). To run the app via R Studio, open up `app.R` in R Studio and either use the `runApp()` function use the 'Run App' button. If running locally, be sure to update your working directory to the place where Loook is stored using the `setwd` function.

### Features

Loook is broken down into 5 main components: one for uploading and specifying variables in a dataset, one for tabulating a summary of the dataset, one for visualizing the distribution of looks and descriptives, and two for analyzing the dataset.

By default, the app displays in [showcase mode](https://shiny.rstudio.com/articles/display-modes.html), which includes the code that generates the app. This is very useful if you'd like to learn more about how the code generates the app, because Shiny showcase will highlight changes in reactive values and portions of `server.R` as it runs them.

#### Preparing Data

You can either use the default dataset, or upload your own .csv. 

If you upload your own data, keep in mind that the app will only work properly with a dataset that includes a header row, contains a categorical fixed effect with 2 levels, and is in tidy format.

In tidy datasets, 

* Each variable has its own column
* Each observation has its own row
* Each value has its own cell

Tidy:
```
subj	test_event	test_pair	look
1		expected	first		12.3
1		unexpected	first		15.1
1		expected	second		5.2
1		unexpected	second		6.9
2		expected	first		8.2
2		unexpected	first		6.1
2		expected	second		10.7
2		unexpected	second		4.0
```

Not tidy:
```
subj	expected1	unexpected1 expected2	unexpected2
1		12.3		15.1		5.2			6.9
2		8.2			6.1			10.7		4.0
```

After clicking the "Updated Dataset" button, you should see the header columns populate the dropdown selectors in Step 2. Select the columns in your dataset that contain looks, a fixed effect, subject IDs, and (optionally) another random effect. This tells shiny to assign those columns to the variables `input$look`, `input$type`, `input$subj`, and `input$rand`.

Check the box 'Log-transform looks?' if you would like to log-transform the looking times in your dataset. This will update the variable `input$log` and will only affect the 'visualize' and 'lm4 analysis' panels.

Click 'Update variables' in order to set these variable specifications. Every time you would like to change the variables in a plot or analysis, be sure to hit this button again. The app will not respond to changes in the dropdown selectors unless the user clicks this button.

#### Summarize

In this panel, you will see a list of observations (`output$dataTable`), the structure of your dataframe (`output$structure`), and a summary (`ouput$summary`) of your dataframe. 

You can adjust the number of observations you view (`input$obs`) by using the text box in Step 1.

The summary table includes many useful descriptive statistics, including the mean, median, range, and so on. By default, this table includes user-specified random variables, so if you would like to un-nest your table, select 'none' in the last dropdown selector (`input$rand`) and click 'Update variables'.

#### Visualize

In this panel, you will see a density plot of looks for each level of your fixed effect (`output$distPlot`), and violin and boxplots of looks for each level of your fixed effect, faceted by your random effect if applicable (`output$boxVioPlot`). 

If you would like to plot new variables, or toggle between raw and log-transformed looking times (`input$log`), be sure to click the 'Update variables' button.

#### Mixed effects models (lme4)

In this panel, you will see summary outputs of one or two mixed effects models (`output$model1` and `output$model2`). This includes information about the fit method, the model formula, fit criterion, and descriptions of random and fixed effects. 

If you specify a random effect, the app will fit a second model with a second random intercept, and also compare the fits of the two models (`output$comparison12`). 

To replicate these models in R, use `lmer()` and `summary()`:

```
model_name = lmer(look ~ trialtype + (1 | subj), REML = FALSE)
summary(model_name)
```

To compare model fits in R, use `anova()`:

```
model_name_1 = lmer(....)
model_name_2 = lmer(....)
anova(model_name_1, model_name_2)
```

#### Non-parametric analyses (boot, Wilcoxon)

In this panel, you will see a density plot from boostrapped difference in medians (`output$bootAnalysis`), and also the results of a Wilcoxon signed rank test (`output$wilcoxonAnalysis`).

This is the logic of bootstrapping:

* First, we separate looking times into two vectors (e.g. `look1` = all looks to expected, `look2` = all looks to unexpected).
* We `sample` random looks from these vectors with replacement. The size of this sample matches the original sample size of the dataset.
* Then, we calculate the median of each of these samples, subtract one from the other, record this value in a vector, `medians`, and repeat the process 4999 more times.
* At the end, `medians` contains a distribution of 5000 bootstrapped median differences. This distribution is plotted as a density plot in this panel.

This is considered a non-parametric analysis because we do not make any assumptions about the underlying distribution producing the looking times in our sample. Instead, we use our empirical looking times to approximate this distribution.

### Deployment

Loook is hosted using shinyapps.io at [https://shariliu.shinyapps.io/loook/](https://shariliu.shinyapps.io/loook/), or by running it on your local machine (e.g. within R Studio). To run the app via R Studio, open `app.R` and launch using the 'Run App' button.

### Built With

* [RStudio](https://www.rstudio.com/) - IDE
* [R Shiny](https://shiny.rstudio.com/) - The web framework used
* [shinyapps.io](http://www.shinyapps.io/) - Hosting
* [tidyverse](https://www.tidyverse.org/) - Includes diplyr, tidyr, and ggplot2
* [psych](https://cran.r-project.org/web/packages/psych/index.html) - Used to generate summary table
* [boot](https://cran.r-project.org/web/packages/boot/) - Bootstrapping tools
* [lme4](https://cran.r-project.org/web/packages/lme4/index.html) - Mixed effects models

### Authors

* **Shari Liu** - *Initial work* - [website](https://www.shariliu.com)

### License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

### Acknowledgments

Special thanks to Patrick Mair for the statistical training, and the CS50 team for helpful feedback and support.

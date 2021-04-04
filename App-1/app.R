library(shiny)
library(DT)
library(plsdepot)
library(ggplot2)
library(visreg)
library(caret)
library(dplyr)
script.dir = getwd()
t = read.csv(file = "./Video_Game_Sales_by_Gregory_Smith/vgsales.csv", header = TRUE, row.names = 1)


convertNA = function(x) { 
  if(is.character(x)||is.factor(x)){
    is.na(x) <- x %in% c("NA", "<NA>", "N/A"); x}
  else {x}
}

t[] = lapply(t, convertNA)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  navbarPage(
    "Lifetime Video Game Sales",
    id = "main_navbar",
    tabsetPanel(
      tabPanel("Data", 
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "platform", label = "Display Entries by Platform", choices = NULL,
                     multiple = TRUE
                   ),
                   selectInput(
                     "publisher", "Display Entries by Publisher", choices = NULL,
                     multiple = TRUE
                   ),
                   selectInput(
                     "genre", "Display Entries by Genre", choices = NULL,
                     multiple = TRUE
                   ),
                   sliderInput(
                     "years", 
                     label = p("Date Range"), 
                     min = as.numeric(min(t$Year, na.rm = TRUE)),
                     max = as.numeric(max(t$Year, na.rm = TRUE)),
                     value = c(as.numeric(min(t$Year, na.rm = TRUE)), as.numeric(max(t$Year, na.rm = TRUE)))
                   ),
                   conditionalPanel(condition="input.tabselected==1", selectInput(
                     "columns", "Show select columns", choices = colnames(t[-1]),
                     multiple = TRUE
                   )),
                   conditionalPanel(condition="input.tabselected==2", selectInput(
                     "xCor", "Sales Data to Be Predicted", choices = colnames(t[c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")]),
                     multiple = FALSE,
                     selected = "NA_Sales"
                   )),
                   conditionalPanel(condition="input.tabselected==2", selectInput(
                     "yCor", "Sales Data Predictor", choices = colnames(t[c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")]),
                     multiple = FALSE,
                     selected = "EU_Sales"
                   ))
                 ),
                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("Table", value=1, DT::dataTableOutput('table_main')),
                               tabPanel("Plot", value=2, mainPanel(
                                 h3(textOutput("r2")),
                                 plotOutput("plot_main"), 
                                 h4(textOutput("text_sub1")),
                                 plotOutput("plot_sub1"), 
                                 textOutput("press"),
                                 textOutput("rmsep"),
                                 textOutput("sst"),
                                 DT::dataTableOutput("table_sub1"))),
                               id = "tabselected"
                   ),
                 )
               )
               ),
      tabPanel("About", (mainPanel(
          h3("What is this?"),
          p("This application allows the user to search through the lifetime, physical sales data for every video game released since 1980.
                    In this tab, I will walk you through this application's feature set and explain in laymen's terms how it works."),
          p("The \"Table\" view allows you to browse the data set as a table as well as to filter the table to only display the rows and columns most relevant to your needs."),
          p("The \"Plot\" view allows you to find the correlation between the video game sales in different international regions and then predict future sales
            numbers per region. This is done by performing a simple linear regression, a machine learning technique that predicts the future values of a dependent
            variable x by finding its correlation with an independent variable y."),
          p("For More information on how to navigate this application, go to the Tutorial tab."),
          h3("How does a linear regression system work?"),
          p("A linear regression system used for this application works in the following way: First, the comparative performance of each video game in the given
            data set in both reasons is taken and 20% of it is partitioned off as a testing cluster. Next, the remaining 80% is passed to the lm() function in
            order to determine the beta coefficients, which are the y-intercept and slope of the line that best illustraits the correlation between x and y
            (ie. If y is a particular value, what's a reasonable estimate for x's value?). lm() finds the beta coefficients with the following linear model
            equation: \"X = y-intercept + (slope * Y)\". With the beta coefficients, we can construct the regression line."),
          p("Next, we test the regression line's accuracy using the testing cluster we partitioned off at the beginning. Using the regression line equation,
            we estimate the x value of each video game in the test cluster by providing the equation with the game's y value. We then compare the difference
            between the predicted values and the actual values in order to determine the Coefficient of Determination (R^2), which represents the proportion
            of the test cluster that adheres to the regression model. This is found by subtracting the Predicted Residual Sum of Squares (PRESS) 
            divided by the Total Sum of Squares (SST) from 1 to get a number between 0 and 1. The closer the Coefficient is to 1, the more accurate
            the regression model is.")
      ))),
      tabPanel("Tutorial", (mainPanel(
        h3("The Table View: "),
        p("By default, the table displays the first 25 entries in the data set,
          with a page-select option at the bottom to cycle through further results. A navigation panel can be found on the left side of the screen. With this panel, the user can narrow down the information in the data set they'd like to see. 
          In the \"Display Entries by Platform\", \"Display Entries by Publisher\", and \"Display Entries by Genre\" fields, select one or more caregories from the list provided.
          The data table will then dynamically change to only display the entries that fit the criteria specified."),
        p("Using the \"Date Range\" field, select a period of years from 1980 to 2020. The table will then dynamically change to only display entries who's release date occurred within the given range.
          Lastly, by typing one or more column names in the \"Select Columns\" field, the table will update and only display the game's title and the columns requested."),
        p("The Table itself also has further search features built in. By using the search field to the top-right of the table, you may search through the data set by
          Name. You may also order the table in ascending/descending order by a particular column by clicking the column's name at the top of the table."),
        h3("The Plot View: "),
        p("In the plot view, a navigation panel can be found on the left side of the screen. A navigation panel can be found on the left side of the screen. With this panel, the user can narrow down the information in the data set they'd like to see. 
          In the \"Display Entries by Platform\", \"Display Entries by Publisher\", and \"Display Entries by Genre\" fields, select one or more caregories from the list provided.
          The plot will then dynamically change to only use the entries that fit the criteria specified."),
        p("Using the \"Date Range\" field, select a period of years from 1980 to 2020. The plot view will then dynamically change to only use entries whose release date occurred within the given range.
          Lastly, select the regional sales data you want to predict from the list of options in the \"Sales Data To Be Predicted\" field, and the regional sales data you wish to use as a predictor
          in the \"Sales Data Predictor\" field. By default, these entries are given the values:  \"NA_Sales\" and \"EU_Sales\". The output will dynamically change when either of these values are changed. "),
        p("By default, the Plot view will display four outputs: 1)A text display of the regression's Coefficient of Determination and an informal evaluation on the accuracy of the regression equation. 2) A plot of every entry in the data set that matches the criteria set in the Navigation Panel.
          This table also contains the calculated linear regression line. 3) A plot that compares every predicted value in the Test cluster to its Actual Value.
          4) A table displaying more information about the predictions made for each entry in the test set (its predicted total sales, its actual total sales, 
          and the lower/upper limits of the prediction). This table can be searched using the search bar in the top-right corner of the table, and can be sorted in ascending/descending order
            by clicking the name of the column you wish the table to be sorted by.")
      )))
    )
  ),
  

)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  rowCheck = function() {
    rows = c(1:nrow(t))
    
    if(is.character(input$platform)){
      rows = intersect(rows, which(t$Platform == input$platform))
    }
    if(is.character(input$publisher)){
      rows = intersect(rows, which(t$Publisher == input$publisher))
    }
    if(is.character(input$genre)){
      rows = intersect(rows, which(t$Genre == input$genre))
    }
    rows = intersect(rows, which(t$Year > input$year[1] & t$year < input$year[2]))
    return(rows)
  }
  main_reactive = reactive({
    #Within a rule, you need to include all entries that have one value or any of the others.
    #For each column, you need to exclude all entries that do not follow every rule.
    #You are creating three separate lists, then intersecting the lists.
    output <- list()
    
    rows = c(1:nrow(t))
    rowsDuplicate = c(1:nrow(t))
    if(is.character(input$platform)){
      rowsPlatform = numeric()
      rowsPlatform = c(rowsPlatform, unlist(lapply(input$platform, function(platform){intersect(rowsDuplicate, which(t$Platform == platform))})))
      rows = intersect(rows, rowsPlatform)
    }
    if(is.character(input$publisher)){
      rowsPublisher = numeric()
      rowsPublisher = c(rowsPublisher, unlist(lapply(input$publisher, function(publisher){intersect(rowsDuplicate, which(t$Publisher == publisher))})))
      rows = intersect(rows, rowsPublisher)
    }
    if(is.character(input$genre)){
      rowsGenre = c()
      rowsGenre = c(rowsGenre, unlist(lapply(input$genre, function(genre){intersect(rowsDuplicate, which(t$Genre == genre))})))
      rows = intersect(rows, rowsGenre)
    }
    rowsYears = intersect(rowsDuplicate, which(t$Year >= input$years[1]))
    rowsYears = intersect(rowsYears, which(t$Year <= input$years[2]))
    rows = intersect(rows, rowsYears)
    
    if(is.character(input$columns)) {
      output$main_table = DT::datatable(t[rows, c("Name", input$columns)], options = list(pageLength = 25))
    } else {
      output$main_table = DT::datatable(t[rows, ], options = list(pageLength = 25))
    }
    
    tSet = t[rows, c(input$xCor, input$yCor, "Name")]
    corr_T = cor(tSet[, 1], tSet[, 2])
    print(corr_T)
    
    set.seed(5478)
    splitT = caret::createDataPartition(tSet[, 1], p = 0.8, list=F, times=1)
    training_tSet = tSet[splitT, ]
    
    test_tSet = tSet[!row.names(tSet) %in% row.names(training_tSet), ]
    test_tSet = tSet[-splitT, ]
    
    print(training_tSet)
    f = as.formula(paste(colnames(training_tSet)[1], colnames(training_tSet)[2], sep = " ~ "))
    lr_tSet = lm(formula = f, data = training_tSet)
    #lr_tSet = lm(colnames(training_tSet)[1] ~ colnames(training_tSet)[2], data = training_tSet)
    fitted(lr_tSet)
    resid(lr_tSet)
    
    plot1 = ggplot(tSet, aes_string(x=colnames(tSet)[1], y=colnames(tSet)[2])) + geom_point(colour = "#24c7b9", size = 3)
    plot2 = plot1 + geom_abline(intercept = lr_tSet[1]$coefficients[1], slope = lr_tSet[1]$coefficients[2], color = "red")
    testPlot = visreg(lr_tSet)
    
    predictedSales = data.frame(predict(lr_tSet, newdata = test_tSet))
    names(predictedSales)[1] = "Predicted"
    predictedSales$Actual = test_tSet[, colnames(test_tSet)[1]]
    
    predictedSales$lower = predict(lr_tSet, newdata = test_tSet, interval = "prediction")[, 2]
    predictedSales$upper = predict(lr_tSet, newdata = test_tSet, interval = "prediction")[, 3]
    
    predictedSales = cbind(Name = test_tSet$Name, predictedSales)
    
    plot3 = qplot(Actual, Predicted, data=predictedSales) + geom_point(colour = "#443d8f", size = 3) + geom_errorbar(aes(ymin = lower,ymax = upper))
    p_text = paste("Predicted NA Sales in the Test Set if the Regression Continues")
    
    PRESS = sum((predictedSales$Actual - predictedSales$Predicted)^2)
    press_text = paste("Predicted Residual Sum of Squares: ", PRESS)
    
    RMSEP = sqrt(PRESS/nrow(predictedSales))
    rmsep_text = paste("Residual Mean Squared Error of Prediction: ", RMSEP)
    
    SST = sum((predictedSales$Actual - mean(predictedSales$Actual))^2)
    sst_text = paste("Total Sum of Squares: ", SST)
    
    R2 = 1 - (PRESS/SST)
    print(typeof(R2))
    r2_text = paste("R^2: ", R2)
    
    output$main_plot = plot2
    output$predict = plot3
    output$predictPlotTitle = p_text
    output$predictTable = DT::datatable(predictedSales, options = list(pageLength = 25))
    output$press = press_text
    output$rmsep = rmsep_text
    output$sst = sst_text
    output$r2 = r2_text
    return(output)
  })
  
  updateSelectizeInput(session, "platform", choices = t$Platform, selected = NULL, server = TRUE)
  updateSelectizeInput(session, "publisher", choices = t$Publisher, selected = NULL, server = TRUE)
  updateSelectizeInput(session, "genre", choices = t$Genre, selected = NULL, server = TRUE)
  
  output$table_main <- DT::renderDataTable(main_reactive()$main_table)
  output$plot_main <- renderPlot(main_reactive()$main_plot)
  output$text_sub1 <- renderText(main_reactive()$predictPlotTitle)
  output$plot_sub1 <- renderPlot(main_reactive()$predict)
  output$press <- renderText(main_reactive()$press)
  output$rmsep <- renderText(main_reactive()$rmsep)
  output$sst <- renderText(main_reactive()$sst)
  output$r2 <- renderText(main_reactive()$r2)
  output$table_sub1 <- DT::renderDataTable(main_reactive()$predictTable)
}

shinyApp(ui = ui, server = server)
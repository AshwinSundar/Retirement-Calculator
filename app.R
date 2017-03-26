# Retirement Calculator
# Ashwin Sundar
# March 2017

# Issues
# 1) Make it so user can't input total expenses that are larger than monthly income (or find a way to handle that in a reasonable way - maybe output text saying that they can never retire)
# 2) FIXED Line 10 outputs numeric(0) for 12*(input$incPerMonthPreTax - input$expPerMonth)

# To deploy app to ashwinsundar.shinyapps.io 
# > rsconnect::deployApp('C:/Users/Ashwin/Dropbox/Side Projects/Retirement Calculator')

require("shiny")

server <- function(input, output) {

  observeEvent(input$calculateButton, {
    # savings after one year (age 27):
    output$savingsRate <- renderTable(
      align = "c",
      {
      
      # Initializes an array. First column will have ages from user's current age up until the assumption for life expectancy
      numOfYears <- input$lifeExp - input$startAge
      ages <- array(input$startAge:input$lifeExp, dim = c(numOfYears+1, 1), dimnames(list("age")))
      colnames(ages) <- c("Age")
      
      # Second column will have pre tax income for each age, accounting for inflation
      preTaxIncomeCol <- array(dim = c(numOfYears+1, 1))
      colnames(preTaxIncomeCol) <- c("Yearly Pre-Tax Income")
      # first cell is current pre-tax income
      preTaxIncomeCol[1] <- 12*input$incPerMonthPreTax
      # pre tax income increases as a function of inflation rate
      for(i in 2:length(preTaxIncomeCol)) {
        preTaxIncomeCol[i] = preTaxIncomeCol[i-1]*exp(input$inflationRate)
      }
      
      # third column will have post-tax income, as a function of effective tax rate
      postTaxIncomeCol <- array(dim = c(numOfYears+1,1))
      colnames(postTaxIncomeCol) <- c("Yearly Post-Tax Income")
      # just pretax income times effective tax rate
      for (i in 1:length(postTaxIncomeCol)) {
        postTaxIncomeCol[i] = preTaxIncomeCol[i]*(1 - input$effTaxRate)
      }
      
      # fourth column will have expenses, accounting for inflation
      expenseCol <- array(dim = c(numOfYears+1, 1))
      colnames(expenseCol) <- c("Yearly Expenses")
      # expenseRatio is the percentage of your income that you spend when you start out. Basically it assumes that you're not gonna change your lifestyle as you get older...which may be too much of an assumption. But for now it will have to do. 
      expenseRatio <- input$expPerMonth/input$incPerMonthPreTax
      # first cell is current expenses
      expenseCol[1] <- 12*input$expPerMonth
      # expenses also increase and is compounded continuously
      for(i in 2:length(expenseCol)) {
        expenseCol[i] = preTaxIncomeCol[i]*expenseRatio
      }
      
      # fifth column will have $ saved, continuously compounded
      savingsCol <- array(dim = c(numOfYears+1, 1))
      colnames(savingsCol) <- c("Total Savings")
      savingsCol[1] <- 0
      for(i in 2:length(savingsCol)) {
        savingsCol[i] = (postTaxIncomeCol[i] - expenseCol[i]) + savingsCol[i-1]*exp(input$invReturnRate)
      }
      
      # sixth column will have $ needed to live rest of life
      moneyForRestOfLifeCol <- array(dim = c(numOfYears+1, 1))
      colnames(moneyForRestOfLifeCol) <- c("Money needed til EOL")
      for (i in 1:length(moneyForRestOfLifeCol)) {
        # add up everything in the expensesCol from this row downward
        moneyForRestOfLifeCol[i] = sum(expenseCol[i:numOfYears+1])
      }
      
      # seventh column will say yes or no if fourth column > fifth column
      # retireBoolCol <- array(dim = c(numOfYears+1, 1))
      # for (i in 1:length(retireBoolCol)) {
      #   if((savingsCol[i] - moneyForRestOfLifeCol[i]) > 0) {
      #     retireBoolCol[i] = "YES"
      #   }
      # 
      # }
      
      # finally, bind everything together and post the array and renderTable will handle it
      outputArray <- cbind(ages, preTaxIncomeCol, postTaxIncomeCol, expenseCol, savingsCol, moneyForRestOfLifeCol)
    },
    striped = TRUE) # makes rows alternate between grey and white
    
  })
}

ui <- fluidPage(
  navlistPanel(well = TRUE,
               fluid = TRUE,
    tabPanel("Retirement",
             {
      sidebarLayout(
        # page spans 12 "units", so width = 4 means 33% of the page
        sidebarPanel(width = '6',
                     ## Part 1: Personal Details ##
                     ##
                     ##
                     HTML("<div align = 'left'><font size = '12'><b>Personal Details: </b></font>"),
                     # asks users for their monthly income after taxes
                     numericInput("incPerMonthPreTax", "Pre-tax Monthly Income ($):", min = 0, max = 100000, value = 2000, step = 50),
                     # asks user for what they expect their tax rate to be
                     numericInput("effTaxRate", "Effective Tax Rate", min = 0, max = 1.0, value = 0.115, step = 0.001),
                     # asks user for their monthly expenses. 
                     numericInput("expPerMonth", "Monthly Expenses ($):", min = 0, max = 100000, value = 1000, step = 50),
                     numericInput("startAge", "Current Age", min = 16, max = 99, value = 26, step = 1),
                     numericInput("currentSavings", "Current Savings", value = 0, step = 50),
                     ## Part 2: Financial assumptions
                     ##
                     ##
                     HTML("<br>"),
                     HTML("<font size = '12'><b>Assumptions: </b></font>"),
                     numericInput("inflationRate", "US Inflation Rate:", value = 0.03, step = 0.001),
                     numericInput("invReturnRate", "Investment Return Rate:", value = 0.0616, step = 0.0001),
                     numericInput("lifeExp", "Life Expectancy:", value = 95, min = 65, max = 122, step = 1),
                     HTML("<br>"),
                     actionButton("calculateButton", "Start"),
                     HTML("</div>")
        ),
        
        mainPanel(tableOutput("savingsRate"))
      )}
    )
    
  )
  
)

shinyApp(ui = ui, server = server)
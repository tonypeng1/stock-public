library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(xts)
library(quantmod)
library(formattable)
library(scales)
library(DT)
library(rhandsontable)
library(diffr)
library(rdrop2)


# This function return % stock price change between the last day and the "start" date.
percentage <- function(name, price, start, type){
    column <- paste(name, 'Close', sep = '.')  # column name of close value
    value <- as.numeric(xts::last(price[, column]))  # last value
    value_before <- as.numeric(price[start][, column])  # earlier value
    while (length(price[start][, column]) == 0) {  
        start <- start + 1
        value_before <- as.numeric(price[start][, column])  # earlier value
    }
    
    change <- value - value_before
    percentage <- scales::percent(change / value_before, accuracy = 0.1)
    if (type == 'string') {
        output <- paste0(round(change, 2), "   (", percentage, ")")
    }
    else if (type == 'short') {
        output <- percentage
    }
    else if (type == 'value') {
        output <- round(change / value_before, 3)
    }
    return(output)
}


# This function add notes based on 'table' to the original data 'df_temp'.
add_note <- function(df_temp, symbol_names, table) {
    for (name in symbol_names) {
        # browser()
        temp <- table %>% filter(SYMBOLS == name) %>% select('NOTES')
        note <- temp$NOTES
        df_temp$Note[df_temp$Symbol == name] <- note
    }
    return(df_temp)
}

# This function creates stock table rows for the input symbols.
table_build <- function(data_list_temp, symbols_data_temp) {
    
    columns <- c('1 day', '1 week', '2 weeks', '1 month', '2 months', 
                 'Current price', '52 week low', '52 week high', '52 week ratio',
                 'Note')
    
    df_temp <- data.frame(Symbol = character(),
                          One_Day = double(),
                          One_Week = double(),
                          Two_Weeks = double(),
                          One_Month = double(),
                          Two_Months = double(),
                          Current_Price = double(),
                          One_Year_Low = double(),
                          One_Year_High = double(),
                          Current_in_One_Year = double(),
                          Note = character()
    )
    for (price in data_list_temp) {
        name <- strsplit(names(price)[1], ".", fixed = TRUE)[[1]][1]
        
        for (col in columns) {
            if (col == '1 day') {
                start <- xts::last(index(price), 2)[1]  # 1 day ago
                output_1_day <- percentage(name, price, start, 'value')
            }
            else if (col == '1 week') {
                start <- xts::last(index(price), 5)[1]  # 5 days (with data) ago
                output_1_week <- percentage(name, price, start, 'value')
            }
            else if (col == '2 weeks') {
                start <- xts::last(index(price), 10)[1]  # 10 days (with data) ago
                output_2_weeks <- percentage(name, price, start, 'value')
            }
            else if (col == '1 month') {
                start <- today %m+% months(-1)  # start = 1 month ago
                output_1_month <- percentage(name, price, start, 'value')
            }
            else if (col == '2 months') {
                start <- today %m+% months(-2)  # start = 2 months ago
                output_2_months <- percentage(name, price, start, 'value')
            }
            else if (col == 'Current price') {
                column <- paste(name, 'Close', sep = '.')  # column name of close value
                value_current <- as.numeric(xts::last(price[, column]))  # last value
                output_current <- round(value_current, 2)
            }
            else if (col == '52 week low' || col == '52 week high') {
                t <- as.POSIXlt(today)
                t$year <- t$year - 1
                start <- as.Date(t)  # start = 1 year ago
                PRICE <- price[paste(start, "", sep = "/")]  # set data range
                
                if (col ==  '52 week high') {
                    column_high <- paste(name, 'High', sep = '.')  # column name of high value
                    value_high <- max(PRICE[, column_high])  # 52 week high value
                    output_52_high <- round(value_high, 2)  # value in string
                }
                else {
                    column_low <- paste(name, 'Low', sep = '.')  # column name of low value
                    value_low <- min(PRICE[,column_low])  # 52 week low value
                    output_52_low <- round(value_low, 2)  # value in string
                }
            }
            else if (col == '52 week ratio') {
                value <- (value_current - value_low) / (value_high - value_low)
                output_ratio_52 <- round(value, 2)
            }
            else if (col == 'Note') {
                temp <- symbols_data_temp %>% filter(SYMBOLS == name) %>% select('NOTES')
                note <- temp$NOTES
            }
        }
        df_temp <- rbind(df_temp, list(Symbol = name,
                                       One_Day = output_1_day,
                                       One_Week = output_1_week,
                                       Two_Weeks = output_2_weeks,
                                       One_Month = output_1_month,
                                       Two_Months = output_2_months,
                                       Current_Price = output_current,
                                       One_Year_Low = output_52_low,
                                       One_Year_High = output_52_high,
                                       Current_in_One_Year = output_ratio_52,
                                       Note = note
        )
        )
    }
    return(df_temp)
}

# This function find in each stock with 1-day, 1-week, or 2-week change > 5%, or 
# 1-month and 2-month change > 10%. Only save at the end the greatest change of 
# a stock for the notification. This data frame is called 'df'.

notification <- function(data_temp) {
    date_range <- c('1 day', '1 week', '2 weeks', '1 month', '2 months')
    
    df_temp <- data.frame(Symbol = character(), 
                          Range = character(), 
                          Change = character())
    
    for (price in data_temp) {
        
        change_min <- 0
        change_max <- 0
        range_min <- ''
        range_max <- ''
        
        name <- strsplit(names(price)[1], ".", fixed = TRUE)[[1]][1]
        
        for (range in date_range) {
            if (range == '1 day') {
                start <- xts::last(index(price), 2)[1]  # 1 day ago
                output <- percentage(name, price, start, 'value')
                per <- formattable::percent(output, 1)
                
                if (per < -0.1) {
                    if (per < change_min) {
                        change_min <- per
                        range_min <- '1 day'
                    }
                }
                else if (per > 0.1) {
                    if (per > change_max) {
                        change_max <- per
                        range_max <- '1 day'
                    }
                }
            }
            else if (range == '1 week') {
                start <- xts::last(index(price), 5)[1]  # 5 days (with data) ago
                output <- percentage(name, price, start, 'value')
                per <- formattable::percent(output, 1)
                
                if (per < -0.1) {
                    if (per < change_min) {
                        change_min <- per
                        range_min <- '1 week'
                    }
                }
                else if (output > 0.1) {
                    if (per > change_max) {
                        change_max <- per
                        range_max <- '1 week'
                    }
                }
            }
            else if (range == '2 weeks') {
                start <- xts::last(index(price), 10)[1]  # 10 days (with data) ago
                output <- percentage(name, price, start, 'value')
                per <- formattable::percent(output, 1)
                
                if (per < -0.1) {
                    if (per < change_min) {
                        change_min <- per
                        range_min <- '1 week'
                    }
                }
                else if (per > 0.1) {
                    if (per > change_max) {
                        change_max <- per
                        range_max <- '1 week'
                    }
                }
            }
            else if (range == '1 month') {
                start <- today %m+% months(-1)  # start = 1 month ago
                output <- percentage(name, price, start, 'value')
                per <- formattable::percent(output, 1)
                
                if (per < -0.2) {
                    if (per < change_min) {
                        change_min <- per
                        range_min <- '1 month'
                    }
                }
                else if (per > 0.2) {
                    if (per > change_max) {
                        change_max <- per
                        range_max <- '1 month'
                    }
                }
            }
            else if (range == '2 months') {
                start <- today %m+% months(-2)  # start = 2 months ago
                output <- percentage(name, price, start, 'value')
                per <- formattable::percent(output, 1)
                
                if (per < -0.2) {
                    if (per < change_min) {
                        change_min <- per
                        range_min <- '2 months'
                    }
                }
                else if (per > 0.2) {
                    if (per > change_max) {
                        change_max <- per
                        range_max <- '2 months'
                    }
                }
            }
        }
        if (change_min != 0) {
            df_temp <- rbind(df_temp, list(Symbol = name, Range = range_min, 
                                           Change = toString(change_min)))
        }
        if (change_max != 0) {
            df_temp <- rbind(df_temp, list(Symbol = name, Range = range_max, 
                                           Change = toString(change_max)))
        }
    }
    return(df_temp)
}

# a new styleColorBar with bar length proportional to value 
# credit: Stéphane Laurent, https://stackoverflow.com/questions/56382522/why-datatable-breaks-the-last-bar-using-shiny
styleColorBar2 <- function (data, color1, color2) 
{
    M <- max(abs(data), na.rm = TRUE)
    js <- c(
        "value <= 0 ? ",  
        sprintf("'linear-gradient(90deg, transparent ' + (1+value/%f) * 100 + '%%, %s ' + (1+value/%f) * 100 + '%%)'", 
                M, color1, M),
        " : ",
        sprintf("'linear-gradient(90deg, transparent ' + (1-value/%f) * 100 + '%%, %s ' + (1-value/%f) * 100 + '%%)'", 
                M, color2, M) 
    )
    JS(js)
}

# select a date 5 years ago
today <- Sys.Date()
t <- as.POSIXlt(today)
t$year <- t$year - 5
years_ago <- as.Date(t)

# Read the stock symbol names and notes to be analyzed.
# symbols_data <- read.csv('public.csv')
symbols_data <- drop_read_csv('public.csv')
symbols_data <- symbols_data %>% arrange(SYMBOLS)
symbols <- symbols_data$SYMBOLS
len <- length(symbols)

if (class(symbols_data$NOTES) != 'character') {
    for (i in seq(len)) {symbols_data$NOTES[i] <- ''}
}

# Load stock data from yahoo. Split stock symbols into groups of 5 to avoid
# needing to wait 1 sec for symbols >= 5.

mod <- len %/% 5
rem <- len %% 5

if (rem == 0) {
    ite = mod
} else {
    ite = mod + 1
}

data <- new.env()
for (i in seq(ite)) {
    j <- i + (i - 1) * 4  # start
    if (i != ite) {
        getSymbols(symbols[j : (j + 4)], env = data, src = 'yahoo',
                   from = years_ago, to = today, auto.assign = TRUE)
    }
    else {
        getSymbols(symbols[j : len], env = data, src = 'yahoo',
                   from = years_ago, to = today, auto.assign = TRUE)
    }
}

data_list <- mget(symbols, envir = data)  # convert files in env to a list of xts

# Added 9-25-2021 to fill NA data from Yahoo so the code won't crash
for (i in 1:length(data_list)) {
    data_list[[i]] <- na.approx(data_list[[i]])
}

date_last <- last(index(data_list[[1]]))  # find the last day in data 


# Shiny

ui <- dashboardPage(
    dashboardHeader(title = 'Stock Data'),
    dashboardSidebar(
        sidebarMenu(
            menuItem('Stock Summary Table', tabName = 'widgets', 
                     icon = icon('comment-dollar')),
            menuItem('Stock Chart', tabName = 'dashboard', icon = icon('chart-bar')),
            menuItem('Change Stock Input Table', tabName = 'tab_table',
                     icon = icon('file-import')),
            menuItem('Source code', icon = icon('file-code-o'),
                     href = 'https://github.com/tonypeng1/stock-public/', newtab = FALSE),
            menuItem('Price Change Notifications', 
                     tableOutput('notice'),
                     icon = icon('dog'),
                     newtab = FALSE)
                ),
            width = 270
            ),

    dashboardBody(
        tabItems(
                tabItem(tabName = 'dashboard',
                        fluidRow(
                            infoBoxOutput('stock', width = 2),
                            infoBoxOutput('date', width = 2),
                            infoBoxOutput('value', width = 2),
                            infoBoxOutput('change', width = 3),
                            infoBoxOutput('change_1', width = 3)
                        ),
                        fluidRow(
                                tabBox(title = 'Time Range',
                                       id = 'tabset1',
                                       selected = '1 year',
                                       width = 10,
                                       tabPanel('5 days',
                                                plotOutput('plot_1', height = 600)),
                                       tabPanel('1 month',
                                                plotOutput('plot_2', height = 600)),
                                       tabPanel('2 months',
                                                plotOutput('plot_3', height = 600)),
                                       tabPanel('6 months',
                                                plotOutput('plot_4', height = 600)),
                                       tabPanel('YTD',
                                                plotOutput('plot_5', height = 600)),
                                       tabPanel('1 year',
                                                plotOutput('plot_6', height = 600)),
                                       tabPanel('2 years',
                                                plotOutput('plot_7', height = 600)),
                                       tabPanel('5 years',
                                                plotOutput('plot_8', height = 600))
                                        ),
                                box(title = 'Stock Symbol', status = 'primary', 
                                    solidHeader = TRUE, width = 2,
                                    selectInput(inputId = 'ticket',
                                                label = '',
                                                choices = symbols
                                                ),
                                                selected = symbols[1])
                                )
                            ),
                tabItem(tabName = 'widgets',
                        box(title = textOutput('table_title'),
                            status = 'primary', solidHeader = TRUE,
                            div(DTOutput('table'), style = "font-size: 90%; width: 90%"),
                            width = 12
                            )
                        ),
                tabItem(tabName = 'tab_table',
                        fluidRow(
                            box(title = "Make changes and click 'UPDATE AND SAVE 
                                TO FILE' to update the current stock summary table, 
                                stock chart, and price change notification, and to 
                                svae this table as the stock input file for use in 
                                the future.",
                                status = 'primary', solidHeader = TRUE, width = 5,
                                rHandsontableOutput('table_change', width = "350%")
                                ),
                            actionButton('update', 'UPDATE AND SAVE TO FILE'),
                            )
                        )
                )
            )
)

server <- function(input, output, session) {
    
    rv <- reactiveValues(input = symbols_data,  # input table
                         symbol = symbols,  #  all symbols
                         raw = data_list,  # list of xts
                         data = table_build(data_list, symbols_data),  # full table
                         notice = notification(data_list))  # price change notice
    
    data_1 <- new.env()
    
    observeEvent(input$update, {
        if (!is.null(input$table_change)) {
            
            new_table <- hot_to_r(input$table_change)  # new input table
            new_table$SYMBOLS <- toupper(new_table$SYMBOLS)
            
            symbols_1 <- sort(new_table$SYMBOLS)  # new symbol list
            
            if (length(symbols_1) != length(unique(symbols_1))) {  
                # duplicated symbols
                showModal(modalDialog(
                    title = 'Update Failed!',
                    "Please remove duplicated symbols and click 'UPDATE AND SAVE 
                    TO FILE' again.",
                    size = 's',
                    easyClose = TRUE,
                    footer = modalButton('OK')
                ))
            } else {
                # browser()
                symbols_diff <- setdiff(union(rv$symbol, symbols_1),
                                        intersect(rv$symbol, symbols_1))
                # in union but not in intersect
                
                if (!(length(symbols_diff) == 0))  {
                    
                    # if there is a difference
                    df_2 <- rv$data  # full table
                    data_list_1 <- rv$raw  # list of xts
                    for (s in symbols_diff) {
                        # browser()
                        if (s %in% rv$symbol) {
                            # if symbol s is deleted 
                            df_2 <- df_2[!(df_2$Symbol == s), ]  # remove s from full table
                            position <- match(s, names(data_list_1))  # get the s ticket position
                            data_list_1[[position]] <- NULL  # remove the s xts 
                        }
                        else {
                            # if symbol s is added
                            getSymbols(s, env = data_1, src = 'yahoo',
                                       from = years_ago, to = today, auto.assign = TRUE)
                            data_list_add <- mget(s, envir = data_1)  
                            # convert files in env to a list of xts
                            df_add <- table_build(data_list_add, new_table)
                            df_2 <- rbind(df_2, df_add)
                            data_list_1 <- c(data_list_1, data_list_add)
                        }
                    }
                    df_2 <- df_2 %>% arrange(Symbol)
                    row.names(df_2) <- NULL  # reset row names starting from 1
                    data_list_1 <- data_list_1[order(names(data_list_1))]  # order xts in list
                    
                    rv$raw <- data_list_1  # list of xts
                    rv$data <- add_note(df_2, symbols_1, new_table)  # full table
                    rv$input <- new_table  # input table
                    rv$symbol <- symbols_1  # symbols
                    rv$notice <- notification(data_list_1)  # notification
                    
                    updateSelectInput(session, 'ticket', choices = rv$symbol, 
                                      selected = rv$symbol[1])
                }
                else {
                    rv$input <- new_table
                    rv$data <- add_note(rv$data, rv$symbol, rv$input)
                }
                write.csv(rv$input, file = 'public.csv', row.names = FALSE)
                drop_upload('public.csv')
            }}
    })
    
    # (table_change can be used as both input and output)
    
    output$table <- renderDT({
                datatable(rv$data,  
                          options = list(pageLength = 17,
                                      lengthMenu=c(5,10,25,50,100),
                                      autoWidth = FALSE,
                                      scrollX = TRUE,
                                      columnDefs = list(list(width='12%',
                                                             targets=c(11))),
                                      lengthChange = FALSE)) %>% 
                          # rownames = FALSE) %>%
                formatPercentage(c('One_Day', 'One_Week', 'Two_Weeks', 'One_Month',
                                   'Two_Months'), 1
                ) %>% 
                formatCurrency(c('Current_Price', 'One_Year_Low', 'One_Year_High')
                ) %>% 
                formatStyle('Current_in_One_Year',
                            background = styleColorBar2(rv$data$'Current_in_One_Year',
                                                       'red', 'lightblue'),
                            backgroundSize = '100% 70%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition ='left'
                ) %>%
                formatStyle(c('One_Day', 'One_Week', 'Two_Weeks'),
                            color = styleInterval(c(-0.1, 0.1), c('red', 'black', 'green'))
                ) %>%
                formatStyle(c('One_Month', 'Two_Months'),
                            color = styleInterval(c(-0.2, 0.2), c('red', 'black', 'green')))
        }
    )
    
    output$notice <- renderTable({
        rv$notice
    })
    
    output$table_title <- renderText({
        paste0(
        'Stock Price Changes (in %) in Different Time Ranges, Current Price, 
        1-Year High and Low, and Current Price Location in 1-Year 
        High and Low (', toString(date_last), ')')
        
    })
    
    output$table_change <- renderRHandsontable(
        rhandsontable(rv$input, width = 400))

    rv_h <- reactiveValues(row = c(), col = c())
    
    observeEvent(input$table_change$changes$changes, {
        
        rv_h$row <- c(rv_h$row, input$table_change$changes$changes[[1]][[1]])
        rv_h$col <- c(rv_h$col, input$table_change$changes$changes[[1]][[2]])
        
        output$table_change <- renderRHandsontable({
            
            rhandsontable(hot_to_r(input$table_change), row_highlight = rv_h$row, 
                          col_highlight = rv_h$col, width = 800) %>% 
                hot_cols(
                    renderer = "
                     function(instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.TextRenderer.apply(this, arguments);

                     if (instance.params) {
                     hrows = instance.params.row_highlight;
                     hrows = hrows instanceof Array ? hrows : [hrows];
                     
                     hcols = instance.params.col_highlight;
                     hcols = hcols instanceof Array ? hcols : [hcols];
                     }

                     for (let i = 0; i < hrows.length; i++) {
                        if (instance.params && hrows[i] == row && hcols[i] == col) {
                        td.style.background = 'pink';
                            }
                        }
                     }")
        })
    })
    
    output$stock <- renderInfoBox({
        name <- input$ticket  # get ticket name
        infoBox('Stock', name, icon = icon('earlybirds'), color = 'yellow')
    })
    
    output$date <- renderInfoBox({
        infoBox('Date', date_last, icon = icon('calendar-alt'), 
                color = 'yellow')
    })
    
    output$value <- renderInfoBox({
        
        name <- input$ticket  # get ticket name
        position <- match(name, rv$symbol)  # get ticket position in symbols
        price <- rv$raw[[position]]  # get the corresponding xts
        column <- paste(name, 'Close', sep = '.')  # column name of close value
        value <- as.numeric(xts::last(price[, column]))
        infoBox('Price', round(value, 2), icon = icon('dollar-sign'), 
                color = 'yellow')
    })
    
    output$change <- renderInfoBox({
        name <- input$ticket  # get ticket name
        position <- match(name, rv$symbol)  # get ticket position in symbols
        price <- rv$raw[[position]]  # get the corresponding xts
        start <- xts::last(index(price), 2)[1]  # 1 day ago
        output <- percentage(name, price, start, 'string')
        infoBox('1 Day Change', output, icon = icon('chart-line'), 
                color = 'yellow')
    })
    
    observe(
        if (input$tabset1 == '5 days') {
            name <- input$ticket  # get ticket name
            position <- match(name, rv$symbol)  # get ticket position in symbols
            price <- rv$raw[[position]]  # get the corresponding xts
            start <- xts::last(index(price), 5)[1]  # 5 days ago
            PRICE <- price[paste(start, "", sep = "/")]  # set data range
                
            output$plot_1 <- renderPlot({
                candleChart(PRICE, up.col = 'green', dn.col = 'red', theme = 'white')
                })
            output$change_1 <- renderInfoBox({
                output <- percentage(name, price, start, 'string')
                title <- paste(input$tabset1, 'change', sep = ' ')
                infoBox(title, output, icon = icon('chart-line'), 
                        color = 'yellow')
            })
            }
        else if (input$tabset1 == '1 month') {
            name <- input$ticket  # get ticket name
            position <- match(name, rv$symbol)  # get ticket position in symbols
            price <- rv$raw[[position]]  # get the corresponding xts
            start <- today %m+% months(-1)  # start = 1 month ago
            PRICE <- price[paste(start, "", sep = "/")]  # set data range
            
            output$plot_2 <- renderPlot({
                candleChart(PRICE, up.col = 'green', dn.col = 'red', theme = 'white')
                })
            output$change_1 <- renderInfoBox({
                output <- percentage(name, price, start, 'string')
                title <- paste(input$tabset1, 'change', sep = ' ')
                infoBox(title, output, icon = icon('chart-line'), color = 'yellow')
            })
            }
        else if (input$tabset1 == '2 months') {
            name <- input$ticket  # get ticket name
            position <- match(name, rv$symbol)  # get ticket position in symbols
            price <- rv$raw[[position]]  # get the corresponding xts
            start <- today %m+% months(-2)  # start = 2 month ago
            PRICE <- price[paste(start, "", sep = "/")]  # set data range
            
            output$plot_3 <- renderPlot({
                candleChart(PRICE, up.col = 'green', dn.col = 'red', theme = 'white')
            })
            output$change_1 <- renderInfoBox({
                output <- percentage(name, price, start, 'string')
                title <- paste(input$tabset1, 'change', sep = ' ')
                infoBox(title, output, icon = icon('chart-line'), color = 'yellow')
            })
        }
        else if (input$tabset1 == '6 months') {
            name <- input$ticket  # get ticket name
            position <- match(name, rv$symbol)  # get ticket position in symbols
            price <- rv$raw[[position]]  # get the corresponding xts
            start <- today %m+% months(-6)  # start = 6 months ago
            PRICE <- price[paste(start, "", sep = "/")]  # set data range
            
            output$plot_4 <- renderPlot({
                candleChart(PRICE, up.col = 'green', dn.col = 'red', theme = 'white')
                })
            output$change_1 <- renderInfoBox({
                output <- percentage(name, price, start, 'string')
                title <- paste(input$tabset1, 'change', sep = ' ')
                infoBox(title, output, icon = icon('chart-line'), color = 'yellow')
            })
            }
        else if (input$tabset1 == 'YTD') {
            name <- input$ticket  # get ticket name
            position <- match(name, rv$symbol)  # get ticket position in symbols
            price <- rv$raw[[position]]  # get the corresponding xts
            start <- format(today, "%Y")  # start = this year
            start = as.Date(paste(start, 1, 1, sep='-'))  # first day of this year
            PRICE <- price[paste(start, "", sep = "/")]  # set data range
            
            output$plot_5 <- renderPlot({
                candleChart(PRICE, up.col = 'green', dn.col = 'red', theme = 'white')
                })
            output$change_1 <- renderInfoBox({
                output <- percentage(name, price, start, 'string')
                title <- paste(input$tabset1, 'change', sep = ' ')
                infoBox(title, output, icon = icon('chart-line'), color = 'yellow')
            })
            }
        else if (input$tabset1 == '1 year') {
            # browser()
            name <- input$ticket  # get ticket name
            position <- match(name, rv$symbol)  # get ticket position in symbols
            price <- rv$raw[[position]]  # get the corresponding xts
            t <- as.POSIXlt(today)
            t$year <- t$year - 1  # 
            start <- as.Date(t)  # start = 1 year ago
            PRICE <- price[paste(start, "", sep = "/")]  # set data range
            
            output$plot_6 <- renderPlot({
                candleChart(PRICE, up.col = 'green', dn.col = 'red', theme = 'white')
                })
            output$change_1 <- renderInfoBox({
                
                output <- percentage(name, price, start, 'string')
                title <- paste(input$tabset1, 'change', sep = ' ')
                infoBox(title, output, icon = icon('chart-line'), color = 'yellow')
            })
        }
        else if (input$tabset1 == '2 years') {
            name <- input$ticket  # get ticket name
            position <- match(name, rv$symbol)  # get ticket position in symbols
            price <- rv$raw[[position]]  # get the corresponding xts
            t <- as.POSIXlt(today)
            t$year <- t$year - 2  # 
            start <- as.Date(t)  # start = 2 year ago
            PRICE <- price[paste(start, "", sep = "/")]  # set data range
            
            output$plot_7 <- renderPlot({
                candleChart(PRICE, up.col = 'green', dn.col = 'red', theme = 'white')
            })
            output$change_1 <- renderInfoBox({
                output <- percentage(name, price, start, 'string')
                title <- paste(input$tabset1, 'change', sep = ' ')
                infoBox(title, output, icon = icon('chart-line'), color = 'yellow')
            })
        }
        else if (input$tabset1 == '5 years') {
            name <- input$ticket  # get ticket name
            position <- match(name, rv$symbol)  # get ticket position in symbols
            price <- rv$raw[[position]]  # get the corresponding xts
            start <- xts::first(index(price))
            PRICE <- price  # full data range
            
            output$plot_8 <- renderPlot({
                candleChart(PRICE, up.col = 'green', dn.col = 'red', theme = 'white')
            })
            output$change_1 <- renderInfoBox({
                output <- percentage(name, price, start, 'string')
                title <- paste(input$tabset1, 'change', sep = ' ')
                infoBox(title, output, icon = icon('chart-line'), color = 'yellow')
            })
        }
        )
    }

shinyApp(ui, server)

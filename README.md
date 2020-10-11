# stock-public
- This app reads an input file of stock symbols (like AAPL, AMZN, ... etc.) and downloads the price data for the last 5 years from Yahoo Finance, and creates bar charts and a customizable table with a variety of the stocks' statistics. The input list of symbols can be revised and the stock statistics be updated in realtime. 

- The input stock symbol file can be updated by clicking the 'Change Stock Input Table' tab. Row(s) containing stock information can be removed or added and notes are directly added to the cells in the 'NOTES' column. After clicking the 'UPDATE AND SAVE TO FILE' button, new stock symbol data (if added) will be downloaded from Yahoo Finance and the stock charts and tables in other tabs will be updated, and the new input stock file will be uploaded to dropbox (overwrite the old one) for use in the future. 

- In the 'Stock Summary Table', if the price changes in the columns 'One_Day', 'One_Week', and 'Two_Weeks' are greater (less) than 10% (-10%) then the values will be in green (red). The same colors also apply to the price changes in the columns 'One_Month' and 'Two_Months' except that the thresholds are greater at 20% and -20%.   

- The drop-down box 'Price Change Notifications' includes the one time period (in all the above periods) that has the greatest price change of a stock symbol (+ or -) that satisfies the above thresholds.

- Each time the app is run a .csv file ('public.csv') containing the input stock symbols (and notes) is downloaded from dropbox using `drop_read_csv('public.csv')`. If this input file is revised in the app by clicking the 'UPDATE AND SAVE TO FILE' button in the 'Change Stock Input Table' tab it will be uploaded back to dropbox using `drop_upload('public.csv')`. Please note that the authentication file (.httr-oauth) needed to connect to the author's dropbox is not included here. To use this code locally for your list of stock symbols, just replace `drop_read_csv('public.csv')` with `read.csv('public.csv')`, comment out the line `drop_upload('public.csv')`, and run the code in your local RSudio. Or you can create your own dropbox .httr-oauth file by running `drop_auth()` in RStudio console, and then log in to your dropbox account in the pop up window (if you have not already logged in to your account). The file .httr-oauth will be automatically saved to your local directory where the app.R file is located (need to make the hidden files visible in your folder to see the file). Finally, you can upload this .httr-oauth file together with the app.R file to your shinyapps.io account.

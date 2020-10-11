# stock-public
- Each time the app is run a .csv file ('public.csv') containing the input stock symbols (and notes) is downloaded from dropbox using `drop_read_csv('public.csv')`. If this input file is revised in the app by clicking the 'UPDATE AND SAVE TO FILE' button in the 'Change Stock Input Table' tab it will be uploaded back to dropbox using `drop_upload('public.csv')`. Please note that the authentication file (.httr-oauth) needed to connect to the author's dropbox is not included here. To use this code locally for your own list of stock symbols, just replace `drop_read_csv('public.csv')` with `read.csv('public.csv')` and comment out the line `drop_upload('public.csv')` and run the code in your local RSudio. Or you can create your own dropbox .httr-oauth file by running `drop_auth()` in RStudio console, and then log in to your dropbox account in the pop up window (if you have not already logged in to your account). The file .httr-oauth will be automatically saved to your local directory where the app.R file is located (need to make the hidden files visible in your folder to see the file). Finally you can upload this .httr-oauth file together with the app.R file to your shinyapps.io account.

- The input stock file can be updated by clicking the 'Change Stock Input Table' tab. Row(s) containg stock information can be removed or added and notes can be directly added to the cells in the 'NOTES' column. After clicking the 'UPDATE AND SAVE TO FILE' button, new stock symbol data (if added) will be downloaded from Yahoo Finance and the stock charts, tables, and notifications in other tabs will be updated, and the new input stock file will be uploaded to dropbox (overwrite the old one) for use in the future. 

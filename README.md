#Using Internet Dervied News and Trends to Predict Stock Market Returns

I find that trends in stock market returns can be predicted by the relative changes in amount terms related to the stock index is searched on search engines, and by sentiment derived by news headlines realted to publicy-traded companies. I use google trends since it gathers the total number of searches relative to the total search volume in google, and text-mine google news headlines for the sentiment as my "signals" to indicate buy-sell positions.

I find good results (profit) and the model runs in 29% of the time when the signals are incorporated to the ARIMA-GARCH model compared to without.

A more thorough description can be found at my website [here](https://moocarme.github.io/AppleStockPred/), as well as a trading simulation derived from the results.

Project_23Jul16.R contains the model, getIndices.R contains all the stock indices to model, as well as their company name, for use in the app and for getting the news sentiment. gNews-sentimentAnalysis.R has functions to get the google news headlines to make the overall code a little cleaner. 
The model generates output files that are subsequently read by the app. The app file and style sheet are contained in the app_files folder, and the model must be ran to generate the output files before the app can be run locally.

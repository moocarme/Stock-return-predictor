getNews <- function(symbol, number){
  
  # Warn about length
  if (number>300) {
    warning("May only get 300 stories from google")
  }
  
  # load libraries
  require(XML); require(plyr); require(stringr); require(lubridate);
  require(xts); require(RDSTK)
  
  # construct url to news feed rss and encode it correctly
  url.b1 = 'http://www.google.com/finance/company_news?q='
  url    = paste(url.b1, symbol, '&output=rss', "&start=", 1,
                 "&num=", number, sep = '')
  url    = URLencode(url)
  
  # parse xml tree, get item nodes, extract data and return data frame
  doc   = xmlTreeParse(url, useInternalNodes = TRUE)
  nodes = getNodeSet(doc, "//item")
  mydf  = ldply(nodes, as.data.frame(xmlToList))
  
  # clean up names of data frame
  names(mydf) = str_replace_all(names(mydf), "value\\.", "")
  
  # convert pubDate to date-time object and convert time zone
  pubDate = strptime(mydf$pubDate, 
                     format = '%a, %d %b %Y %H:%M:%S', tz = 'GMT')
  pubDate = with_tz(pubDate, tz = 'America/New_york')
  mydf$pubDate = NULL
  
  #Parse the description field
  mydf$description <- as.character(mydf$description)
  parseDescription <- function(x) {
    out <- html2text(x)$text
    out <- strsplit(out,'\n|--')[[1]]
    
    #Find Lead
    TextLength <- sapply(out,nchar)
    Lead <- out[TextLength==max(TextLength)]
    
    #Find Site
    Site <- out[3]
    
    #Return cleaned fields
    out <- c(Site,Lead)
    names(out) <- c('Site','Lead')
    out
  }
  description <- lapply(mydf$description,parseDescription)
  description <- do.call(rbind,description)
  mydf <- cbind(mydf,description)
  
  #Format as XTS object
  mydf = xts(mydf,order.by=pubDate)
  
  # drop Extra attributes that we don't use yet
  mydf$guid.text = mydf$guid..attrs = mydf$description = mydf$link = NULL
  return(mydf) 
  
}
# news <- getNews('AAPL',1000)
# news_df <- data.frame(date = index(news), coredata(news))

positives= readLines("pos-neg-words/positive-words.txt")
negatives = readLines("pos-neg-words/negative-words.txt")
i=0

sentiment_scores = function(tweets, positive_words, negative_words, .progress='none'){
  scores = laply(tweets,
                 function(tweet, positive_words, negative_words){
                   tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
                   tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
                   tweet = gsub('\\d+', '', tweet)          # remove digits
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   tweet = sapply(tweet, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(tweet, "\\s+")
                   words = unlist(word_list)
                   # compare words to the dictionaries of positive & negative terms
                   positives_matches = positive_words
                   print(head(positive_words))
                   positive.matches = match(words, positive_words)
                   negative.matches = match(words, negative_words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive_matches = !is.na(positive.matches)
                   negative_matches = !is.na(negative.matches)
                   # final score
                   score = sum(positive_matches) - sum(negative_matches)
                   return(score)
                 }, positive_words, negative_words, .progress=.progress )
  return(scores)
}

# score = sentiment_scores(news_df$Lead, positives, negatives, .progress='text')
# news_df$score=score


# Define server logic required to do Twitter account analysis
server <- function(input, output) {
  # Use user's keys to pull tweets of account of interest
  consumerKey <- renderText({input$consumer_key})
  consumerKey <- reactive({consumerKey()})
  consumerSecret <- renderText({input$consumer_secret})
  consumerSecret <- reactive({consumerSecret()})
  accessToken <- renderText({input$access_token})
  accessToken <- reactive({accessToken()})
  accessSecret <- renderText({input$access_secret})
  accessSecret <- reactive({accessSecret()})
  targetAccount <- renderText({paste0("'", input$twitter_name, "'")})
  user <- reactive({targetAccount()})
  
  #input update - user
  observeEvent(input$newUser, handlerExpr = {
    user <- sample(users, size = 1, replace = F)
    updateTextInput(session, "user", value=user)
    updateRadioButtons(session, "type", selected="user2")
  })
  
  #input update - hashtag
  observeEvent(input$newHashtag, handlerExpr = {
    tag <- sample(hashtags, size = 1, replace = F)
    updateTextInput(session, "user", value=tag)
    updateRadioButtons(session, "type", selected="hashtag")
  })
  
  observeEvent(input$update, once = T, handlerExpr = {
    #api validation at beginning of session
    api_key <- consumerKey
    api_secret <- consumerSecret
    access_token <- accessToken
    access_token_secret <- accessSecret
    setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
    
    twitteR:::setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  })
  
  #data gathering and plot generation
  observeEvent(input$update, handlerExpr = {
    #tweet gathering and error handling based on input type
    if (input$type == "user2") {
      tweets <- try(userTimeline(user = input$user,
                                 n = input$tws, includeRts = FALSE, retryOnRateLimit = 2000))
      
      if(inherits(tweets ,'try-error')){
        return(NULL)
      }
      
      
    } else if (input$type == "hashtag") {
      tweets <- try(searchTwitter(input$user, lang = "en",
                                  n = input$tws, retryOnRateLimit = 2000))
      
      if(inherits(tweets ,'try-error')){
        return(NULL)
      }
    }
    #put tweets and metadata into dataframe
    tweets.df <- try(twListToDF(tweets))
    if(inherits(tweets.df ,'try-error')){
      return(NULL)
    }
    #copy of dataframe for use later
    tweets.df.copy <- tweets.df
    
    #clean text
    cleanStrVec <- function(string_vec) {
      clean_vec <- c()
      for (k in c(1:length(string_vec))) {
        n <-iconv(string_vec[k], "latin1", "ASCII",sub='')
        clean_vec <- c(clean_vec, n)
      }
      return(clean_vec)
    } 
    
    text <- tweets.df$text
    cleaned_text<-cleanStrVec(text)
    
    #create the Corpus object
    myCorpus <- Corpus(VectorSource(cleaned_text))
    # convert to lower case 
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    # remove punctuation
    myCorpus <- tm_map(myCorpus, removePunctuation) 
    # remove numbers
    myCorpus <- tm_map(myCorpus, removeNumbers)
    # remove URLs
    removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
    myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
    # add two extra stop words
    myCorpusFull <- myCorpus
    myStopwords <- c(stopwords("english"),stopwords("SMART"), "amp", "use", "see", "used", "will", "im")
    # remove stopwords from corpus
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    #extra stopwords based on input hashtag
    word <- tolower(input$user)
    stop <- gsub("#", "", word)
    moreStopwords <- c(stop, "rt", "fb", "retweet", "a", "b" , "c", "d", "e", "f", "g", "h", "i", 
                       "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t","u","v", "w", "x", "y", "z")
    myCorpus <- tm_map(myCorpus, removeWords, moreStopwords)
    #create copy
    myCorpusCopy <- myCorpus
    # stem words
    myCorpus <- tm_map(myCorpus, stemDocument)
    
    #complete stems
    stemCompletion2 <- function(x, dictionary) {
      x <- unlist(strsplit(as.character(x), " "))
      x <- x[x != ""]
      x <- stemCompletion(x, dictionary=dictionary)
      x <- paste(x, sep="", collapse=" ")
      PlainTextDocument(stripWhitespace(x))
    }
    myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
    myCorpus <- Corpus(VectorSource(myCorpus))
    
    #word frequency dataframe function  
    getConditionedDataFrame <- function(myCorpus) {
      #create the term matrix
      tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
      # calculate the frequency of words and sort it by frequency
      word.freq <- sort(rowSums(as.matrix(tdm)), decreasing = T)
      word.freq <- subset(word.freq, word.freq >=1)
      df <- data.frame(term = names(word.freq), freq = word.freq)
      return(df)
    }
    
    #single string of all tweets for tweet generation function
    getNGramStr <- function(myCorpus) {
      str <- sapply(myCorpus, function(x){x$content})
      str <- concatenate(lapply(str,"[",1))
      return(str)
    }
    
    #dataframe of n-grams for table function
    getNGramDf <- function(myCorpus) {
      token_delim <- " \\t\\r\\n.!?,;\"()"
      sample_df <- data.frame(text=unlist(sapply(myCorpus, '[',"content")), stringsAsFactors=F)
      token <- NGramTokenizer(sample_df, Weka_control(min=input$nGram,max=input$nGram, delimiters = token_delim))
      grams <- data.frame(table(token))
      sorted <- grams[order(grams$Freq,decreasing=TRUE),]
      colnames(sorted) <- c("term", "freq")
      return(sorted)
    }
    
    #dataframe of n-grams for plot function
    getNGramDf2 <- function(myCorpus) {
      token_delim <- " \\t\\r\\n.!?,;\"()"
      sample_df <- data.frame(text=unlist(sapply(myCorpus, '[',"content")), stringsAsFactors=F)
      token <- NGramTokenizer(sample_df, Weka_control(min=input$nGram2,max=input$nGram2, delimiters = token_delim))
      grams <- data.frame(table(token))
      sorted <- grams[order(grams$Freq,decreasing=TRUE),]
      colnames(sorted) <- c("term", "freq")
      return(sorted)
    }
    
    #word frequency dataframe
    tweets.df <- getConditionedDataFrame(myCorpus)
    
    #TDM/DTM for topic modeling 
    tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
    dtm <- as.DocumentTermMatrix(tdm)
    #remove empty values from DTM and original dataframe copy
    rowTotals <- apply(dtm , 1, sum) 
    dtm <- dtm[rowTotals> 0, ]
    tweets.df.copy <- tweets.df.copy[rowTotals> 0,]
    
    #transpose dataframe for random word gathering
    tweets.df.t <- tweets.df[,-2]
    
    #n-gram string
    str <- getNGramStr(myCorpus)
    
    #n-gram dataframe
    df2 <- getNGramDf2(myCorpus)
    
    #randomized color values for plots
    hues <- c(60:330)
    pals <- c(3:8)
    
    observeEvent(input$update, once = T, handlerExpr = {
      #sample one word at random from the text
      word <- sample(tweets.df.t, size = 1, replace = F)
      updateTextInput(session, "word", value=word)
      
      #word frequency barplot
      output$freqPlot <- renderPlot({
        ggplot(tweets.df[1:input$numWords,], aes(x=reorder(term, freq), y=freq, fill = as.factor(term))) +
          geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Terms") + ylab("Count") +
          coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
      })
      
      #wordcloud
      output$wordPlot <- renderWordcloud2({
        validate(
          need(input$max <= nrow(tweets.df), "Selected size greater than number of elements in data")
        )
        new_df <- tweets.df[1:input$max,]
        wordcloud2(new_df, color="random-light", size = .6, shuffle=T, rotateRatio = sample(c(1:100) / 100))
      })
      
      #wordcloud generation
      observeEvent(input$newCloud, handlerExpr = {
        output$wordPlot <- renderWordcloud2({
          validate(
            need(input$max <= nrow(tweets.df), "Selected size greater than number of elements in data")
          )
          new_df <- tweets.df[1:input$max,]
          wordcloud2(new_df, color = "random-light", shuffle=T, size = .6, rotateRatio = sample(c(1:100) / 100))
        })
      })
      
      output$corrPlotText <- renderText ({
        paste("Word Correlation Plot")
      })
      
      #word correlation plot
      output$corrPlot <- renderPlot({
        validate(
          need(input$lowFreq < tweets.df[1,2], "Selected frequency is greater than highest value in data,
               please choose a lower value.")
          )
        plot((tdm),
             terms=findFreqTerms(tdm, lowfreq=input$lowFreq),
             corThreshold=input$corThreshold,
             weighting = T)
      })
      
      output$corrTableText <- renderText ({
        paste("Word Correlation Table")
      })
      
      #word correlation table
      output$corrTable <- renderPrint({
        as.data.frame(findAssocs(tdm, input$word, corlimit=input$corLimit))
      })
      
      #random word gathering from dataframe
      observeEvent(input$randWord, handlerExpr = {
        word <- sample(tweets.df.t, size = 1, replace = F)
        updateTextInput(session, "word", value=word)
      })
      
      #topic modeling 
      observeEvent(input$newTopics, handlerExpr = {
        
        #a kind of average to judge log-liklihood by
        harmonicMean <- function(logLikelihoods, precision = 2000L) {
          llMed <- median(logLikelihoods)
          as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                               prec = precision) + llMed))))
        }
        
        #run LDA model n times
        seqk <- seq(2, input$timesToRun, 1)
        burnin <- 1000
        iter <- 1000
        keep <- 50
        fitted_many <- lapply(seqk, function(k) LDA(dtm, k = k, method = "Gibbs",
                                                    control = list(burnin = burnin, iter = iter, keep = keep)))
        
        # extract logliks from each topic
        logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
        
        # compute harmonic means
        hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
        
        #create model
        lda.mod <- LDA(dtm, seqk[which.max(hm_many)], method = "Gibbs", control = list(iter=2000))
        
        #gather topics
        topics <- topics(lda.mod, 1)
        
        output$numTopics <- renderText ({
          paste("Optimal number of topics is", seqk[which.max(hm_many)])
        })
        
        #topic table  
        output$topics <- renderPrint({
          validate(
            need(try(seqk[which.max(hm_many)] >= input$topicNum), "Selected number of topics greater than optimal number.")
          )
          terms <- as.data.frame(terms(lda.mod, input$terms), stringsAsFactors = FALSE)
          t(terms[1:input$topicNum])
        })
        
        #topic/tweet probability heatmap
        output$topicPlot <- renderPlotly({
          validate(
            need(try(seqk[which.max(hm_many)] >= input$topicNum2), "Selected number of topics greater than optimal number.")
          )
          validate(
            need(try(nrow(tweets.df.copy) >= input$tweetNum), "Selected number of tweets greater than amount gathered.")
          )
          #get probabilities
          topics <- as.matrix(posterior(lda.mod, dtm)[["topics"]])
          #trim matrix
          topics <- topics[1:input$tweetNum,1:input$topicNum2]
          #gather and clean tweet text
          text1 <- tweets.df.copy$text
          text1 <- text1[1:input$tweetNum]
          text1<-cleanStrVec(text1)
          text1 <- gsub('http\\S+\\s*', '', text1)
          #copy tweet data for use in heatmap
          n <- nrow(topics) - 1
          text1 = cbind(text1, replicate(n,text1))
          #x-axis parameters
          x <- list(
            title = "Topic number",
            autotick = F,
            ticks = "outside",
            range = c(0.5, input$topicNum2 + 0.5),
            dtick = 1,
            tickcolor = toRGB("blue")
          )
          #y-axis parameters
          y <- list(
            title = "Tweet number",
            autotick = T,
            ticks = "outside",
            range = c(1, input$tweetNum),
            tickcolor = toRGB("blue")
          )
          plot_ly(z = topics,  x = colnames(topics), y = rownames(topics), type = "heatmap", 
                  hoverinfo = 'text', text = text1, colorbar = list(title = "Probability of topic given tweet")) %>% 
            layout(title = "Topic/Tweet Association Heatmap", xaxis = x, yaxis = y)
        })
        
      })
      
      #generate single random tweet at beginning of session
      output$babbleTable <- renderPrint({
        validate(
          need(try(input$nGram <= input$length), "Selected N-Gram value greater than requested tweet length")
        )
        ng <-  ngram(str, input$nGram)
        babble(ng, input$length)
      })
      
      #generate new random tweet    
      observeEvent(input$tweet, handlerExpr = {
        output$babbleTable <- renderPrint({
          validate(
            need(try(input$nGram <= input$length), "Selected N-Gram value greater than requested tweet length")
          )
          ng <-  ngram(str, input$nGram)
          babble(ng, input$length)
        })
      })
      
      #n-gram barplot
      output$plot <- renderPlot({ 
        df2 <- getNGramDf2(myCorpus)
        ggplot(df2[1:input$size,], aes(x=reorder(term, freq), y=freq, fill = as.factor(term))) + 
          geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Terms") + 
          ylab("Count") + coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
      })
      
      #n-gram wordcloud
      output$plot2 <- renderWordcloud2(expr = {
        df2 <- getNGramDf2(myCorpus)
        validate(
          need(input$size2 <= nrow(df2), "Selected size greater than number of elements in data")
        )
        df2 <- df2[1:input$size2,]
        wordcloud2(df2, size = 0.27, rotateRatio = sample(c(1:100) / 100))})
      
      #wordcloud generation
      observeEvent(input$newCloud2, handlerExpr = {
        output$plot2 <- renderWordcloud2(expr = {
          df2 <- getNGramDf2(myCorpus)
          validate(
            need(input$size2 <= nrow(df2), "Selected size greater than number of elements in data")
          )
          df2 <- df2[1:input$size2,]
          wordcloud2(df2, size = 0.27, rotateRatio = sample(c(1:100) / 100))})
      })
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
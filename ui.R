library(shiny)
library(plotly)
library(twitteR)
library(ROAuth)
library(ggplot2)
library(httr)
library(rjson)
library(tm)
library(gridExtra)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(topicmodels)
library(data.table)
library(dplyr)
library(ngram)
library(RWeka)
library(Rmpfr)
library(Rgraphviz)

ui <- shinyUI(
  pageWithSidebar(
   headerPanel("Bag-of-Words Tweet Analysis"),

   #loading message
   sidebarPanel(
       tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                        tags$div("Loading...",id="loadmessage")),
       #side panel 1
        conditionalPanel(condition="input.tabselected==1",
                         helpText("This app uses a simplified model of text representation where word order 
                         and grammar are ignored but frequency is not, effectively treating the corpus 
                         like a bag of all of its words together."),
           textInput(inputId = "user",
                      label = "Twitter User or Hashtag:"),
           radioButtons("type", "Search type:",
                        c("User" = "user2",
                          "Hashtag" = "hashtag")),
           actionButton("newUser", "Generate user"),
           actionButton("newHashtag", "Generate hashtag"),
           actionButton("update", "Gather data"),
           hr(),
           sliderInput("tws", "Maximum number of tweets to obtain:", 
                       min=100, max= 500, value=250, step = 1),
          sliderInput("numWords", "Words in bar graph:", 
                    min=1, max= 35, value=10, step = 1),
          sliderInput("max",
                      "Size of wordcloud:",
                      min = 5,  max = 100, value = 50, step = 5),
          actionButton("newCloud", "Generate new wordcloud")),
   
       #side panel 2
        conditionalPanel(
                 condition="input.tabselected==2",
                 hr(),
                 helpText("First adjust the word frequency to fit the current data, 
                 then experiment with the correlation threshold until the plot is most readable."),
                 sliderInput("lowFreq", "Lowest word frequency to appear in plot:", 
                             min=5, max= 100, value=10, step = 1),
                 sliderInput("corThreshold", "Minimum correlation to appear in plot:", 
                             min=.05, max= .8, value=.2, step = .01),
                 textInput(inputId = "word",
                           label = "Find word associations among data:"),
                 actionButton("randWord", "Generate word from corpus"),
                 sliderInput("corLimit",
                             "Correlation limit in associations:",
                             min = .01,  max = .6, value = .15, step = 0.01)),
       #side panel 3
        conditionalPanel(condition="input.tabselected==3",
                 hr(),
                 helpText("Topic modeling utilizes latent Dirichlet allocation to analyze the corpus, 
                 generate potential topics, and assign each document a topic based on similar words in the text. 
                 The algorithm is run up to 30 times to determine the optimal number of topics."),
                 sliderInput("timesToRun", "Number of topics to try:", 
                             min=5, max= 30, value=15, step = 1),
                 actionButton("newTopics", "Generate topics"),
                 sliderInput("topicNum", "Number of topics to display:", 
                             min=1, max= 30, value=10, step = 1),
                 sliderInput("terms", "Number of terms per topic:", 
                             min=2, max= 10, value=3, step = 1),
                 sliderInput("topicNum2", "Number of topics in heatmap:", 
                             min=2, max= 30, value=5, step = 1),
                 sliderInput("tweetNum", "Number of tweets in heatmap:", 
                             min=2, max= 500, value=50, step = 1)),
       #side panel 4
        conditionalPanel(condition="input.tabselected==4",
                         hr(),
                         helpText("An n-gram is a continuous sequence of n words from a given text. 
                         Tweet generation uses a Markov chain to generate random tweets from 
                         the corpus based on the selected n-gram value."),
                         sliderInput("nGram", "N-gram value (Tweet):", 
                                     min=1, max= 10, value=5, step = 1),
                         sliderInput("length", "Tweet length:", 
                                     min=2, max= 10, value=5, step = 1),
                         actionButton("tweet", "Generate tweet from corpus"),
                         sliderInput("nGram2", "N-gram value (Plot):", 
                                     min=2, max= 4, value=2, step = 1),
                         sliderInput("size", "Size of plot:",
                                     min = 1, max = 35, value = 10, step = 1),
                         sliderInput("size2", "Size of wordcloud:",
                                     min = 5, max = 30, value = 15, step = 5),
                         actionButton("newCloud2", "Generate new wordcloud"))),
   
      #conditional main panel
      mainPanel(
        tabsetPanel(
           tabPanel("Word Plots", plotOutput("freqPlot"), wordcloud2Output("wordPlot"), value = 1,  
           conditionalPanel(condition="input.tabselected==1")),
           tabPanel("Word Correlations", textOutput("corrPlotText"),plotOutput("corrPlot"), 
           textOutput("corrTableText"), verbatimTextOutput("corrTable"), value = 2,  conditionalPanel(condition="input.tabselected==2")),
           tabPanel("Topic Modeling" , textOutput("numTopics"), verbatimTextOutput("topics"), 
           plotlyOutput("topicPlot"), value = 3,  conditionalPanel(condition="input.tabselected==3")),
           tabPanel("N-Gram Segmentation", value = 4, conditionalPanel(condition="input.tabselected==4"), 
           verbatimTextOutput("babbleTable"), plotOutput("plot"), wordcloud2Output("plot2")),
           id = "tabselected")
        )
   
        )
   
        )

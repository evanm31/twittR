#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(plotly)
require(twitteR)
require(ROAuth)
require(ggplot2)
require(httr)
require(rjson)
require(tm)
require(gridExtra)
require(SnowballC)
require(wordcloud)
require(wordcloud2)
require(RColorBrewer)
require(topicmodels)
require(data.table)
require(dplyr)
require(ngram)
require(RWeka)
require(Rmpfr)
require(Rgraphviz)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  tags$head(tags$style(HTML("
                            .shiny-text-output {
                            background-color:#fff;
                            }"))),
  
  h1("Twitter Account Analysis by Evan Moore", 
     style = "font-family: 'Source Sans Pro';
     color: #000; text-align: center;
     padding: 20px"),
  br(),
  
  fluidRow(
    column(12,
           p("Twitter is one of the most popular social media platforms around the world.  Consequently, businesses and prominent 
             individuals use Twitter to express their emotions and opinions on a variety of topics.  They will also use to it to 
             promote products and ideologies to a mass audience.  This tool will analyze tweets of an account of interest.",
             style = "font-family: 'Source Sans Pro';"),
           p("However, in order to use this tool, you wil need to create an application on Twitter.  This will give you a consumer key, a 
             consumer secret key, an access token, and an access secret key.  If you have created an application on Twitter already, input 
             your keys below.  If you have not, please follow the steps below.",
             style = "font-family: 'Source Sans Pro';"),
           p("1. Go to https://dev.twitter.com/ and login.",
             style = "font-family: 'Source Sans Pro';"),
           p("2. Hover over your profile picture and click on 'My applications'.",
             style = "font-family: 'Source Sans Pro';"),
           p("3. Click on the 'Create a new application' button.",
             style = "font-family: 'Source Sans Pro';"),
           p("4. After you have the completed the form, click on the 'Create my access token' button.",
             style = "font-family: 'Source Sans Pro';"),
           p("5. Enter your consumer key, consumer secret key, access token, and access secret key in the appropriate fields below. 
             Don't worry - this application runs locally.  We do not record your keys in any database.",
             style = "font-family: 'Source Sans Pro';"))),
  br(),
  
  # Prompt user to input Twiiter keys and tokens
  fluidRow(column(12, wellPanel(
    textInput("consumer_key", "Consumer key", value = "xxxxxxxxxxxxxxxxxxxxxxxxx", width = NULL, placeholder = NULL),
    textInput("consumer_secret", "Consumer secret", value = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", width = NULL, placeholder = NULL),
    textInput("access_token", "Access token", value = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", width = NULL, placeholder = NULL),
    textInput("access_secret", "Access secret", value = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", width = NULL, placeholder = NULL),
    textInput("twitter_name", "Enter the Twitter handle of the account of interest without the '@' symbol", value = "xxxxxxxxxx", width = NULL, placeholder = "Twitter handle"),
    actionButton("submit", "Analyze Twitter Account")))),
  br(),
  
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
      id = "tabselected")))
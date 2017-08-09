# twittR: Bag-of-Words Tweet Analysis with R 

This R Shiny app creates a variety of informative figures based off of the bag-of-words distribution of a corpus of Twitter data collected from either a user or a hashtag, including frequency barplots and wordclouds for phrases 1-4 words long, a word correlation table and plot, topic modeling and a topic distribution heatmap, and a random tweet generator. To try the app, visit [this link](https://ermoore.shinyapps.io/twittr/).
 in your RStudio console.
![alt text](http://i.imgur.com/aSJurJ4.png "Screenshot 1")
![alt text](http://i.imgur.com/JSbkV8b.png "Screenshot 2")
![alt text](http://i.imgur.com/VY4CC3m.png "Screenshot 3")
![alt text](http://i.imgur.com/QWQT6gc.png "Screenshot 4")
![alt text](http://i.imgur.com/yOj62eF.png "Screenshot 5")
![alt text](http://i.imgur.com/datjk3q.png "Screenshot 6")

# Acknowledgements 

First off I would like to thank Gordon Anderson at UMass Amherst for giving me the idea to create this through an assignment in his Introduction to Data Science class, as well as providing code to access the Twitter API, extract tweets, and clean the corpus. RDataMining's [Twitter Data Analysis with R](http://www.rdatamining.com/docs/twitter-analysis-with-r) presentation inspired the word correlation plot and stem completion functions, and I borrowed much of the code to compute the optimal number of topics from David Meza's excellent post [Topic Modeling in R](http://davidmeza1.github.io/2015/07/20/topic-modeling-in-R.html). Drew Schmidt and Christian Heckendorf's [Guide to the ngram Package](https://cran.r-project.org/web/packages/ngram/vignettes/ngram-guide.pdf) also proved to be a great resource to learn the library from. Last but not least, I would be remiss to not mention the numerous StackOverflow/similar pages that I consulted along the way to make this app a reality. Many thanks to all of you!  

# Description

The information provided by this app is conditioned on the [bag-of-words model](https://en.wikipedia.org/wiki/Bag-of-words_model) of text representation, where grammar and word order are ignored in favor of tracking the frequency words appear among the corpus. Though simplistic in nature, this perspective quickly elucidates valuable information about what words are most important to the corpus through frequency, correlation, and n-gram analysis, and offers easy applications to approaches like topic modeling that help us to understand the text as whole more readily. 

The [latent Dirichlet allocation](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) model used in this app is based on the idea that each document of the corpus (or tweet, in this case) belongs to some small number of topics, the topics themselves being composed of a small number of words that are used most frequently in each one. The model is constructed multiple times to determine the maximum log liklihood that each document belongs to each topic, which is based on the probability that each "topic" word would appear in the document given its frequency (an aspect of the model that lends itself nicely to bag-of-words style text classification). Each document is assigned a probability of belonging to each topic once the optimal model has been fitted, which is used in the app to generate the heatmap of tweets and topics.

Another interesting aspect of the app is the [Markov chain](https://en.wikipedia.org/wiki/Markov_chain)-based tweet generation system, which is itself based on strings of n-grams (or n length sequences of words) from the corpus. A Markov chain is a random, probabilistic process that is able to make predictions of future events based solely on the present state of a system; in the app, this is utilized as sequences of n-grams being linked together based on the probability that each would appear next to each other in the corpus, using these values as the states of the Markov chain. Since the probabilities themselves are calculated based on the frequency of the n-1 length phrases that appear after each n-gram,this multiplicity-based approach complements the bag-of-words model used and greatly extends the possibilities for analysis that the rudimentary approach offers. 

# NLP_Project
The goal of this nature language processing project is to use the text data as the input, to create a shiny app that allow the user to input some words
and automatically predict the next few words. This project served as the capstone project for Coursera Data Science certificate fulfillment.
## The Input Data
The originl data is the text data provided by Swiftkey. You can download the data by clicking [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip). The three text files in the en_US folder are used as the data for this project.
## R Packages
The packages used in this projects are listed as follow:
* dplyr
* quanteda
* datasets
* data.table
* shiny
## Project Workflows
<font size="4">**1. Subset the data:** 30% of each text files are randomly sampled.</font>  
<font size="4">**2. Remove confounded:** Abbreviations like 'm,'t are replaced, and characters other than English removed.</font>  
<font size="4">**3. Tokenize the data:** Tokenize the data while removing punctuation, numbers, urls, etc.</font>  
<font size="4">**4. Stem the words:** Words are stemmed to avoid counting same words in different forms in multiple times.</font>  
<font size="4">**5. N-grams analysis:** Perform N-grams calculation and split the words in bigram and trigram for future analysis.</font>  
<font size="4">**6. Kneser Kny Smoothing:** Use Kneser-Ney smoothing methods to assign the probilities to each unigram, bigrams and trigrams.</font>  
<font size="4">**7. Predict the words:** Function will search the input in trigrams first then bigrams, if still no matches, one of the top 10 unigrams will be predicted.</font>
## The Shiny APP
The Shiny App can be accessed from [here](https://jliao12.shinyapps.io/NLP_Next_Word_Prediction/).
## Additional
* ok_getword.R: Please refer to this file for data processings
* unidata, bidata, tridata: Processed data files by using ok_getword, and is the data used for shiny app
* ui.R and server.R: r files for making the shiny app


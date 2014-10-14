### tweets_collection.R file ###
library (twitteR) # load package twitteR: provides interface to Twitter API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem') # download cacert.pem
# assign your Twitter Application data to R variables
reqURL <- "https://api.twitter.com/oauth/request_token" # request token URL
accessURL <- "https://api.twitter.com/oauth/access_token" # access token URL
authURL <- "https://api.twitter.com/oauth/authorize" # authorize URL
consumerKey <- "your_consumer_key" # consumer key
consumerSecret <- "your_consumer_secret" # consumer secret
twitter_cred <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, requestURL=reqURL, accessURL=accessURL, authURL=authURL) # create a OAuth object to access to Twitter API
twitter_cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) # execute handshake method to enable connection to Twitter API (you must open the provided URL in a web browser and write the code in the Console)
save(twitter_cred, file="twitter_auth.Rdata") # save the OAuth object for future sessions
load("twitter_auth.Rdata") # load the OAuth object
registerTwitterOAuth(twitter_cred) # register OAuth credentials to Twitter R session
tweets_list <- searchTwitter("GOOGLE", cainfo='cacert.pem', n=15) # tweets collection example: last 15 tweets containing the word GOOGLE
# each tweets_list element contains several attributes such as:
print(tweets_list[[1]]$id) # tweet id
print(tweets_list[[1]]$text) # tweet text content
print(tweets_list[[1]]$screenName) # user name
print(tweets_list[[1]]$created) # creation time
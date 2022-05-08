##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Supervised Learning for classification (Cont.)
# Api run
##########################################################################

library(plumber)

r <- plumb("model_api.R")
r$run(port=8000)

# curl -X GET "http://127.0.0.1:8000/predict?Pclass=1&Age=47" -H "accept: */*"
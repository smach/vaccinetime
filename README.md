# vaccinetime

This application looks at date and time trends for when new vaccine appointments first become publicly available, using the incredible [@vaccinetime]( https://twitter.com/vaccinetime) Twitter bot by [Dan Cahoon](https://twitter.com/dpcahoon) at [Bioworks](https://twitter.com/dpcahoon). _This is not meant for real-time use._ If you want to pounce on appointments quickly, use the [@vaccinetime bot]( https://twitter.com/vaccinetime), which updates much more frequently. This app aims to help you understand _when_ you might want to be paying special attention to that bot.

This repo includes two files: update_tweets.R to pull, wrangle, and store @vaccinetime's tweets; and app.R for a Shiny app to graph results based on user filters.

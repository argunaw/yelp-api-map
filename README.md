# Overview of Map 

This is a personal project to create a map of every East, Southeast, and South Asian restaurant in New York City. The map in its current form is hosted at https://ayanthi-g.shinyapps.io/asian-food-nyc/ .

I pulled this data from yelp using the Yelp API. More information on the Yelp API can be found at https://www.yelp.com/developers/documentation/v3/get_started . 

I also heavily borrowed code from https://urbandatapalette.com/post/2022-05-shiny-zoom-selected-features/ for the zoom function and from https://rpubs.com/fitzpatrickm8/yelpapi in creating the yelp function to pull the data.

The code used to pull the data can be found in the final function for yelp file, and the code used to create the app can be found in the server and UI files. The README_app file creates the "About" tab in the app.

# Data Caveats

Yelp assigns several restaurants as seafood or barbeque when they are Asian seafood or barbeque restaurants; in order to pull these records I used the search terms "asian seafood" and "asian barbeque" and manually went through the list of results by Googling each one to see which ones were Asian restaurants and which ones are not.

Yelp also uses the category "Himalayan/Nepalese" for all Nepalese and Tibetan restaurants. I manually categorized these restaurants based on Googling each one and making a judgment as to the country of origin; some have cuisines from multiple countries while some are primarily Tibetan or Nepalese.

I found the caveats above when my friends noticed some of their favorite restaurants were missing from earlier iterations of the map. There may be more Asian restaurants that I haven't included because of the nuances of Yelp's categories.

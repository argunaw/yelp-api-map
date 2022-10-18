library(tidyverse)
library(httr)
library(dplyr)
library(data.table)
library(plyr)


### Step 1: Prepare data for loop-----

## the below list refers to the search terms used in the yelp API
##these terms are paired with the categories in the yelp API output

list_of_cuisines <- list("chinese",  "indian", "sri lankan", "himalayan/nepalese"
                      ,"thai", "lao", "bangladeshi", "afghan"
                      ,"pakistani", "japanese", 
                      "malaysian", "filipino", "singaporean", "vietnamese"
                      ,"korean", "taiwanese", "mongolian", "asian"
                      ,"burmese", "indonesian"
                      ,"pan asian", "cambodian", "asian seafood", "asian barbeque")

##this list refers to the "categories_1" column in the dataframe 
## the yelp API returns. These grouped list are based on yelp api 
##documentation

yelp_terms_list <- list(list("chinese", "hotpot", "wok", "dimsum", 
                          "cantonese", "hainan","shanghainese",  "szechuan") ## all chinese
                     , list("indpak", "gastropubs") ##all indian
                     , "srilankan"
                     , "himalayan"
                     ,"thai"
                     , "laotian"
                     ,"bangladeshi"
                     , "afghani"
                     , "pakistani"
                     ,list("jpsweets", "japanese", "conveyorsushi", "japacurry", "izakaya", "ramen", "teppanyaki") ##all japanese
                     ,"malaysian"
                     , "filipino"
                     , "singaporean"
                     , "vietnamese"
                     ,"korean"
                     , "taiwanese"
                     , "mongolian"
                     ,"asian"
                     , "burmese"
                     ,"indonesian"
                     ,"panasian"
                     ,"cambodian"
                     ,"seafood", "bbq")

##nested list of variables
var_list <- list(cuisines=list_of_cuisines, terms = yelp_terms_list)

## locations to search for restaurants in, 5 boroughs plus "NY, NY"
location <- list("Staten Island, NY", "Brooklyn, NY", "Queens, NY", "Manhattan, NY", "Bronx, NY", "New York, NY")


###set api variables, stored in .RProfile (usethis::edit_r_profile())
client_id <- Sys.getenv("client_id_yelp")
client_secret <- Sys.getenv("client_secret_yelp")


###Step 2: Create functions to pull, parse, and process the data----

### parsing yelp data in to a dataframe
## source: https://rpubs.com/fitzpatrickm8/yelpapi
yelp_httr_parse <- function(x) {
  
  parse_list <- list(id = x$id, 
                     name = x$name, 
                     rating = x$rating, 
                     review_count = x$review_count, 
                     latitude = x$coordinates$latitude, 
                     longitude = x$coordinates$longitude, 
                     address1 = x$location$address1, 
                     city = x$location$city, 
                     state = x$location$state, 
                     categories = x$categories)
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- data_frame(id=parse_list$id,
                   name=parse_list$name, 
                   rating = parse_list$rating, 
                   review_count = parse_list$review_count, 
                   latitude=parse_list$latitude, 
                   longitude = parse_list$longitude, 
                   address1 = parse_list$address1, 
                   city = parse_list$city, 
                   state = parse_list$state
                   ,categories_1 = parse_list$categories[[1]][[1]]
                   ,categories_2 = parse_list$categories[[1]][[2]]
                  
  )
  df
}


### process for calling the API and looping through urls for each page
## each call returns. Sourced via https://rpubs.com/fitzpatrickm8/yelpapi
get_restaurants_yelp <- function(cuisines, location) {
  ###yelp api parameters
  client_id <- client_id
  client_secret <- client_secret
  
  res <- POST("https://api.yelp.com/oauth2/token",
              body = list(grant_type = "client_credentials",
                          client_id = client_id,
                          client_secret = client_secret))
  yelp <- "https://api.yelp.com"
  term <- paste(cuisines, "restaurant") 
  location <- location #5 boroughs and NYC
  limit <- 50
  radius <- 7500 ##radius around geography
  offset = seq(0,1000,50) ##each call returns 50 results, so we split 1000 results in to 50-call chunks
  ###get list of all urls for each API call
  urls = c() 
  for (i in offset){
    url = modify_url(yelp, path = c("v3", "businesses", "search"),
                     query = list(term = term
                                  ,location = location
                                  ,limit = limit
                                  ,radius = radius
                                  ,offset=i
                     ))
    urls = c(urls, url)}
  ###api calls
  new_results = c()
  for (i in urls){
    #print(i)
    res <- GET(i, add_headers('Authorization' = paste("Bearer", client_secret)))
    results <- content(res)
    new_results = c(new_results, results)
  }
  ###parse data
  results_list = c() 
  for (j in 1:length(new_results)){
    result = lapply(new_results[j]$businesses, FUN = yelp_httr_parse)
    results_list = c(results_list, result)
  }
  ###transform api results in to a dataframe
  business_data <- unique(setDT(do.call("rbind.fill", results_list)))
  business_data
}

### turn nested list from above in to single dataframe
finalize_data <- function(bd, yelp_terms){
  filtered_data <- bind_rows(Filter(function(x) nrow(x) > 0, bd))
  ##remove results not in NYC and limit to yelp categories listed earlier
  filtered_data <- filtered_data %>%
    filter(categories_1 %in% yelp_terms
           & state=="NY" 
           & !city %in% c("Yonkers", "Pelham","Mount Vernon")) %>%
    distinct(id, .keep_all = TRUE)
  
}



#### Step 3: Run code to get results in to a list of nested dataframes----

### loop for every cuisine based on nested list pairs
for (i in 1:length(var_list$cuisines)) {
  start_time <- Sys.time()
  bd <- lapply(location,  get_restaurants_yelp ,cuisines=var_list$cuisines[i])
  df <-finalize_data(bd, var_list$terms[[i]])
  if( !exists("x") )
  {
    x <- c()
  }
  x <- append(x, list(df), 0)
  
  Sys.sleep(5)
  end_time <- Sys.time()
  time_needed <- end_time - start_time   
  print(paste("Step", i, "was finished after", difftime(end_time, start_time, units = "mins"), "minutes.")) 
  
}


### Step 4: Turn nested dataframes in to useable data ------

###combine nested list in to one dataframe
all_restaurants <- bind_rows(x)

###check - any duplicate restaurants?
n_occur <- data.frame(table(all_restaurants$id))
n_occur[n_occur$Freq > 1,]
#none!

###write out file to csv to maintain a record of the original,unedited data pull
currentDate <- Sys.Date()
fileName="asian_restaurants_yelp_"
csvFileName <- paste(filepath_yelp,fileName,currentDate,".csv",sep="")
write.csv(all_restaurants, file=csvFileName) 

### Step 5: Manual Adjustments-----

##Some cuisines are too broadly categorized (tibetan/nepalese/himalayan are all one)
##Some are categorized as other types (seafood, bbq) but are Asian restaurants

##There is no programmatic way to do this, so it must be done manually by looking
## up each restaurant and making a judgment call

## This process is done 3 times for Himalayan, seafood, and bbq restaurants

###update tibetan/nepalese/himalayan food options:
##1. A&G Himalayan Fresh Food / Gang Chen Bodkyi Momo:RyY_ZQI10g7zynbPtKy6mg:Tibetan
#2. Annapurna Thakali Kitchen:Y9NNkpZCLJvnj6bwm9FDrg: Nepalese
#3. Cafe Himalaya: noBCTbD1Ylf5QR7PXy-rkA: Tibetan
#4: Cafe Tibet: m6WOGq7BSB8YPjKoOpNTLQ: Tibetan
#5: Dawa's: lcI9kOcRqxxMhM4b5pHqsg: Nepalese
#6: Expresseats: bsdrgwAzkOkhicQf58NLbQ: Nepalese
#7: Gakyizompe: o0KsfMVarWBDsTOGFXVHsA: Tibetan
#8: Gorkhali Nepalese Restaurant: h7SmXRnc1KD_bkkVVn7gXg: Nepalese
#9: Hamro Bhim's Cafe: Lz3v2MH3KwtGgz7MdR4fAQ: Nepalese
#10: Himalayan Curry House Restaurant and Bar: FqYgcc7Mk2XrYJ10OGgghQ: Nepalese
#11: Himalayan Yak: 3C6c_zqRz_U2AyeN_7LTsg: Nepalese
#12: K2 Delights: RFiR-vXB46apdqHYr4S57w: Tibetan
#13: Kanchanjunga Restaurant: pMDKAGBMZqUkZwfpOu4zxA: Nepalese
##14: Kathmandu Fusion Kitchen :KT8Th7GSEdBiIhkIlqOV5g: Nepalese
#15: Khampa Kitchen: GJ7Zpr5SfdJ0ML7kmyRiiw: Tibetan
#16: Laliguras Restaurant: vTX77dgdGryT9zc6mKXEsQ: Nepalese
#17: Lhasa Fresh Food: 8R2hDxGrppGO9mNHKqsWpA: Tibetan
#18: Lhasa Tibetan Restaurant: fNdqZHe3XiDKSgr1Ccmoow: Tibetan
#19: LHASA Tibetan Restaurant: PQOmBOFXF9FQNhOMEdrhSg: Tibetan
#20: Lungta - Asian Bistro: b8O2Tiz6Xdw25GXCxCm3wg: Tibetan
#21: Mom's Momo: ep0-kdNh6o0F6rEKZBzmNw:Tibetan
#22: Mom's Momo:DjyZd1H1X3BZBD9OPXpuUQ: Tibetan
#23: Momo Crave: viPKFWURtx09AVOa6iiKSg: Nepalese
#24: Mustang Thakali Kitchen: U0QlNZX8FVwiXCIIaHqhGw: Tibetan and Nepalese
#25: NAMASTE BROOKLYN: 9gD5ORFGgBRy8SgrLSwXHQ: Tibetan and Nepalese
#26: Namaste Chautari Restaurant: utbWLB3zYR1o__iDI9-Xgg: Nepalese
#27: Namaste Nepal: C6AAKuzSw-HrP7G47y47TQ: Nepalese
#28: Namaste Tashi Delek: TFE8EAjOqkrHYM9jH6De0A :Nepalese
#29: Nepali Bhanchha Ghar: pSxvfdkC3cDNp-cXRB68tw: Nepalese
#30: New Nepali Kitchen: PfSIawPma5gn8YF9VZFsPA: Nepalese
#31: Om Wok: Fj5RosdsJV24ZT8S6X-nNQ: asian
#32: Phayul: E1NbrEDNxq_cl39sMfxX5A: Tibetan
#33: Phayul: ZTmTubV5cyzeR0fRbGZ91g: Tibetan
#34: Potala Fresh Momo: 5yEX566DCppdRBcd_c1jvA: Tibetan
#35: Potala Fresh Momo: daYd9YFIwdc83ZcdkjCdqA: Tibetan
#36: Punda Tibetan Restaurant: KJNXC2NtxRPeu_nkk1PC4A: Tibetan
#37: Red Panda Restaurant: K9xO8xYvvpEwgsdonKplcA: Nepalese
#38: Sherpa Restaurant Bar & Sushi: 1LWovcNgB_bepndllNgz_w: Nepalese
#39: Spicy Tibet: OQjI8bMxpnx-9WKzyGqfmg: Tibetan
#40: Taste From Everest: NVMdf02l1GuzzDb7ESI_qQ: Nepalese
#41: Tawa Food: LaMbEoM5Vw9CQP5usaD2xA: Nepalese
#42: Tibet Kitchen: 7aH8qPfHxuy9Hub5Q8GKFw: Tibetan
#43: Woodside Cafe: 7X3hB3KtkHVKk1GZKAnlbA: Nepalese

#Based on the manual categorizations above, create lists based on country

tibetan <- c('RyY_ZQI10g7zynbPtKy6mg', '7aH8qPfHxuy9Hub5Q8GKFw', 'OQjI8bMxpnx-9WKzyGqfmg',
             'KJNXC2NtxRPeu_nkk1PC4A', 'daYd9YFIwdc83ZcdkjCdqA', '5yEX566DCppdRBcd_c1jvA'
             ,'ZTmTubV5cyzeR0fRbGZ91g',  'E1NbrEDNxq_cl39sMfxX5A'
             ,'DjyZd1H1X3BZBD9OPXpuUQ', 'ep0-kdNh6o0F6rEKZBzmNw'
             , 'b8O2Tiz6Xdw25GXCxCm3wg', 'PQOmBOFXF9FQNhOMEdrhSg'
             ,'fNdqZHe3XiDKSgr1Ccmoow', '8R2hDxGrppGO9mNHKqsWpA'
             ,'GJ7Zpr5SfdJ0ML7kmyRiiw', 'RFiR-vXB46apdqHYr4S57w'
             ,'o0KsfMVarWBDsTOGFXVHsA', 'm6WOGq7BSB8YPjKoOpNTLQ'
             ,'noBCTbD1Ylf5QR7PXy-rkA')
nepalese <- c('7X3hB3KtkHVKk1GZKAnlbA', 'LaMbEoM5Vw9CQP5usaD2xA'
              ,'NVMdf02l1GuzzDb7ESI_qQ', '1LWovcNgB_bepndllNgz_w'
              ,'K9xO8xYvvpEwgsdonKplcA', 'PfSIawPma5gn8YF9VZFsPA'
              ,'pSxvfdkC3cDNp-cXRB68tw', 'TFE8EAjOqkrHYM9jH6De0A'
              ,'C6AAKuzSw-HrP7G47y47TQ', 'utbWLB3zYR1o__iDI9-Xgg'
              ,'vTX77dgdGryT9zc6mKXEsQ', 'KT8Th7GSEdBiIhkIlqOV5g'
              ,'pMDKAGBMZqUkZwfpOu4zxA', '3C6c_zqRz_U2AyeN_7LTsg'
              ,'FqYgcc7Mk2XrYJ10OGgghQ', 'Lz3v2MH3KwtGgz7MdR4fAQ'
              ,'h7SmXRnc1KD_bkkVVn7gXg', 'bsdrgwAzkOkhicQf58NLbQ'
              ,'lcI9kOcRqxxMhM4b5pHqsg','Y9NNkpZCLJvnj6bwm9FDrg'
              ,'viPKFWURtx09AVOa6iiKSg')

asian <- c('Fj5RosdsJV24ZT8S6X-nNQ', '9gD5ORFGgBRy8SgrLSwXHQ')

tibetan_nepalese <- c('U0QlNZX8FVwiXCIIaHqhGw', '9gD5ORFGgBRy8SgrLSwXHQ')

##reassign categories_1 column as necessary
all_restaurants[all_restaurants$id %in% tibetan, ][,10] <- 'tibetan'
all_restaurants[all_restaurants$id %in% nepalese, ][,10] <- 'nepalese'
all_restaurants[all_restaurants$id %in% asian, ][,10] <- 'asian'
all_restaurants[all_restaurants$id %in% tibetan_nepalese, ][,10] <- 'tibetan_nepalese'


### there are some seafood and bbq restaurants that are Asian restaurants
###manually check every seafood restaurant

## mEi57As_Gwyt1F73V2x__A, 1392 Seafood BBQ, chinese
## TpyQqUOlZGwEUg50_DMudg, DIY Seafood Party, chinese
## zZecd370h2jxzh1kRJ4y2g, Asian Jewels Seafood Restaurant, cantonese
## gWaK8OEXawE43-Di0ykTNQ, Canton Lounge - New York, dimsum
## 6Rw91YkOUCzkhvdThDUjFg, Good Day Seafood Restaurant, dimsum
## EFihj7vsRRXiF5la-UURyw, Hop Kee, chinese
## MyFqNpQKY4JtQ-gXSeJxUA, Howong Seafood Restaurant, chinese
## tKkGBTtkPl_A9KXT-ODPpg, Hunan cafe, szechuan
## kisSrOTu8AET43_Q6ZX07A, King Crab House, cantonese
## kXYTWVIPTnCukSo2jfxZ8A, East River Eatery, dimsum
## X1upzQ1oDfU9Jb378kUkRA Fish Village, cantonese
## SQT1chaH2EmwYgNvvodGLQ,Ocean Bay Seafood Restaurant, dimsum
## spJTHaNjEzKwoEiYWR1ggA, Ping's Seafood, dimsum
## Yb6PQ_4YQpJ0SVZ1tetkbg, Queens Seafood Restaurant, cantonese
## RFgVkZHcWKD19vHFESuVLw ,RuYi Seafood Restaurant, chinese
## rpLZDxivPeb9ZXqqgDPfpg ,Sunshine Seafood Restaurant, dimsum
## X0RvICQVc4mww5Demc7L0w, Wing Hing Seafood Restaurant, chinese
## naKx74Q0wLANNxmVDtq_6Q, Dokdoya, korean
## 3W_yj4AVk32FIHnUvkB3sA, Dynamite Seafood, asianfusion
## Dr_eezrA2Cc_azMYnwhRkA, Sup Crab, asianfusion
## QXTvRZTAgynX9ZuXHqGPbg, Tianxia Asian Cuisine, asianfusion
## GURmB4kTMLpG8AGjx52ILw, New Pinang South East Asian Cuisine, asian
## WJS06XxRdc6DD1s4LWE9HA, Fish Cheeks, thai
## LpVbI-E9C-CSp7WhU5h2Iw, Fishmarket Restaurant, malaysian

chinese_seafood <- c('mEi57As_Gwyt1F73V2x__A', 'TpyQqUOlZGwEUg50_DMudg', 'zZecd370h2jxzh1kRJ4y2g', 'gWaK8OEXawE43-Di0ykTNQ'
                     , '6Rw91YkOUCzkhvdThDUjFg', 'EFihj7vsRRXiF5la-UURyw', 'MyFqNpQKY4JtQ-gXSeJxUA', 'tKkGBTtkPl_A9KXT-ODPpg'
                     ,'kisSrOTu8AET43_Q6ZX07A' ,'kXYTWVIPTnCukSo2jfxZ8A', 'X1upzQ1oDfU9Jb378kUkRA', 'SQT1chaH2EmwYgNvvodGLQ'
                     ,'spJTHaNjEzKwoEiYWR1ggA','Yb6PQ_4YQpJ0SVZ1tetkbg', 'RFgVkZHcWKD19vHFESuVLw', 'rpLZDxivPeb9ZXqqgDPfpg'
                     ,'X0RvICQVc4mww5Demc7L0w')

korean_seafood <- c('naKx74Q0wLANNxmVDtq_6Q')

asian_seafood <- c('3W_yj4AVk32FIHnUvkB3sA', 'Dr_eezrA2Cc_azMYnwhRkA', 'QXTvRZTAgynX9ZuXHqGPbg', 'GURmB4kTMLpG8AGjx52ILw')

thai_seafood <- c('WJS06XxRdc6DD1s4LWE9HA')

malaysian_seafood <- c('LpVbI-E9C-CSp7WhU5h2Iw')

##update table
all_restaurants[all_restaurants$id %in% chinese_seafood, ][,10] <- 'chinese'
all_restaurants[all_restaurants$id %in% korean_seafood, ][,10] <- 'korean'
all_restaurants[all_restaurants$id %in% asian_seafood, ][,10] <- 'asian'
all_restaurants[all_restaurants$id %in% thai_seafood, ][,10] <- 'thai'
all_restaurants[all_restaurants$id %in% malaysian_seafood, ][,10] <- 'malaysian'


###barbeque

## X4TgyhDDomDVbCvDAKe3PA, Bajie BBQ , chinese
## xzPEPEmH9AOuLebnuUlqjA ,Fat Ni BBQ Cart, chinese
## k8vE6g59i4JrAmXd6K1g5w, Friendship Foods BBQ, chinese
## fSrNR5LTGNZ0prZWGivLrA, Funny BBQ, chinese
## cMuHouN7_VzSsm6NsSkyAQ, iCook Buffet, chinese
## zkxEqO_vCVEdpnD6gJOMNg, Ming Xing BBQ, chinese
## hkbPyRXEZzR5LInlY0FGGg, Mingmen BBQ, chinese
## JVuNvnj0WgCOgp4ML-hggA, Night Market BBQ, chinese
## sfsjk1h50ooC2ioY7m48gw, Pig Heaven, szechuan
## tS4aiWBYgzlqKX898jr3_w, Shun Wang, cantonese
## d_eInFpX-Ix9qxFFYAqqLA, Xinjiang BBQ Cart, chinese
## wzmeaMTm5cZ1JCjep7iQeA, ZLS BBQ, chinese
## 8Fij9sx69MUc5iQl-5PzMw ,Fire Belly Korean Bbq, korean
## s0Klxfd2XRZSrATjzEimBg, GAN-HOO BBQ, korean
## _vQugjzPLr-4udYpiISieA, Gopchang Story BBQ, korean
## VwSxF9Ron5y0PIXTtGrqkQ, Gopchang Story Flushing, korean
## MnV_28NaCVTObHsDNshWcg, Meat Me BBQ, korean
## mnj9O1tbCKiAN5zWMe5v8w, Picnic Garden, korean
## wUu9WcGjP8KS6KlpARChyQ, Gyu-Kaku Japanese BBQ, Japanese
## NQbh8zKPqILV6WeYSOM-yQ, Makana Hawaiian & Japanese BBQ, japanese
## seGYMEYsw6F_H33Md1E1dA, Tito Rad's Grill, filipino


chinese_bbq <- c('X4TgyhDDomDVbCvDAKe3PA', 'xzPEPEmH9AOuLebnuUlqjA', 'k8vE6g59i4JrAmXd6K1g5w', 'fSrNR5LTGNZ0prZWGivLrA'
                 ,'cMuHouN7_VzSsm6NsSkyAQ', 'zkxEqO_vCVEdpnD6gJOMNg', 'hkbPyRXEZzR5LInlY0FGGg','JVuNvnj0WgCOgp4ML-hggA'
                 ,'sfsjk1h50ooC2ioY7m48gw','tS4aiWBYgzlqKX898jr3_w','d_eInFpX-Ix9qxFFYAqqLA', 'wzmeaMTm5cZ1JCjep7iQeA')

korean_bbq <- c('8Fij9sx69MUc5iQl-5PzMw', 's0Klxfd2XRZSrATjzEimBg', '_vQugjzPLr-4udYpiISieA','VwSxF9Ron5y0PIXTtGrqkQ'
                ,'MnV_28NaCVTObHsDNshWcg','mnj9O1tbCKiAN5zWMe5v8w')


japanese_bbq <- c('wUu9WcGjP8KS6KlpARChyQ', 'NQbh8zKPqILV6WeYSOM-yQ')

filipino_bbq <- c('seGYMEYsw6F_H33Md1E1dA')

##update table
all_restaurants[all_restaurants$id %in% chinese_bbq, ][,10] <- 'chinese'
all_restaurants[all_restaurants$id %in% korean_bbq, ][,10] <- 'korean'
all_restaurants[all_restaurants$id %in% japanese_bbq, ][,10] <- 'japanese'
all_restaurants[all_restaurants$id %in% filipino_bbq, ][,10] <- 'filipino'


###remove bbq and seafood restaurants that are not asian
all_restaurants <- all_restaurants %>% 
  filter(!grepl(paste(c('bbq', 'seafood'), collapse = '|'), categories_1))



##Step 6: Add country flags for eventual map markers----

library(countrycode)
library(ggimage)
library(flagon) ##https://coolbutuseless.github.io/2020/04/03/introducing-flagon-a-package-of-just-flag-images-in-png-and-svg-format/

###add flag urls as a column to the dataframe
country_codes <- flagon::country_codes
countries <- data.frame(table(all_restaurants$categories_1))


###country translations, done manually:
#1. afghani: Afghanistan
#2. asian: no flag
#3. bangladeshi: Bangladesh
#4. burmese: Myanmar
#5. cambodian: Cambodia
#6. cantonese: China
#7. chinese: China
#8. dimsum: China
#9. filipino: Philippines
#10. gastropubs: India
#11. hotpot: China
#12. indonesian: Indonesia
#13. indpak: India
#14. izakaya: Japan
#15. japacurry: Japan
#16. japanese: Japan
#17. korean: Korea, Republic of
#18.laotian: Lao People's Democratic Republic
#19. malaysian: Malaysia
#20. nepalese: Nepal
#21. pakistani: Pakistan
#22. panasian: none
#23. ramen: Japan
#24. shanghainese: China
#25. singaporean: Singapore
#26. srilankan: Sri Lanka
#27. szechuan: China
#28. taiwanese: Taiwan
#29. teppanyaki: Japan
#30. thai: Thailand
#31. tibetan: Tibet
#32. tibetan_nepalese: None
#33. vietnamese: Viet Nam

# Adding column with country name to join to flag table:
all_restaurants <- all_restaurants %>%
  mutate(full_country_name = case_when(
    categories_1=="afghani" ~ "Afghanistan",
    categories_1 %in% c("panasian", "asian", "tibetan_nepalese") ~ "none",
    categories_1=="bangladeshi" ~ "Bangladesh",
    categories_1=="burmese" ~ "Myanmar",
    categories_1=="cambodian" ~ "Cambodia",
    categories_1 %in% c("chinese", "hotpot", "wok", "dimsum", 
                        "cantonese", "hainan", "shanghainese", 
                        "szechuan") ~ "China",
    categories_1=="filipino" ~ "Philippines",
    categories_1 %in% c("indpak", "gastropubs") ~ "India",
    categories_1=="indonesian" ~ "Indonesia",
    categories_1 %in% c("jpsweets", "japanese", "conveyorsushi"
                        , "japacurry", "izakaya", "ramen"
                        , "teppanyaki") ~ "Japan",
    categories_1=="korean" ~ "Korea, Republic of",
    categories_1=="laotian" ~ "Lao People's Democratic Republic",
    categories_1=="malaysian" ~ "Malaysia",
    categories_1=="nepalese" ~ "Nepal",
    categories_1=="pakistani" ~ "Pakistan",
    categories_1=="singaporean" ~ "Singapore",
    categories_1=="srilankan" ~ "Sri Lanka",
    categories_1=="taiwanese" ~ "Taiwan",
    categories_1=="thai" ~ "Thailand",
    categories_1=="tibetan" ~ "Tibet",
    categories_1=="vietnamese" ~ "Viet Nam"
     
  ))

##left join to countries table to get flag url
restaurants_flag <- merge(x=all_restaurants,y=country_codes[,c("country","ccode")], by.x = "full_country_name" , by.y="country", all.x=TRUE)

##update country name for conciseness
restaurants_flag$full_country_name[restaurants_flag$full_country_name == "Korea, Republic of"] <- "South Korea"
restaurants_flag$full_country_name[restaurants_flag$full_country_name == "Lao People's Democratic Republic"] <- "Laos"
restaurants_flag$full_country_name[restaurants_flag$full_country_name == "Viet Nam"] <- "Vietnam"


  

##add flag urls
## I manually added a red dot to the list of flag images in the package folder for ones with no country
## I also manually added the Tibetan flag to the folder
restaurants_flag <- restaurants_flag %>%
  mutate(flag_url = case_when(full_country_name=="Tibet" ~ "C:/Users/argun/Documents/R/win-library/4.1/flagon/png/tib.png",
                              full_country_name=="none" ~ "C:/Users/argun/Documents/R/win-library/4.1/flagon/png/none.png",
                              TRUE ~ flagon::flags(ccode)))

###remove filepath for app
restaurants_flag$flag_url <- gsub("C:/Users/argun/Documents/R/win-library/4.1/flagon/png/", "", restaurants_flag$flag_url)

###remove non-parseable characters for eventual text searches in DT (pinyin is not searchable in DT)
restaurants_flag <- dplyr::mutate_if(restaurants_flag, is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))})



###write out file to csv. The shiny app will reference the csv
currentDate <- Sys.Date()
fileName="asian_restaurants_yelp_flag_"
csvFileName <- paste(filepath_yelp,fileName,currentDate,".csv",sep="")
write.csv(restaurants_flag, file=csvFileName,  fileEncoding = "UTF-8") 


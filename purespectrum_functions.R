require(httr)

## Survey without quotas: ------------------------------------------------------
ps_post_survey_noquotas <- function(survey_title,
                           survey_category_code = 232,  # default: "exciting-new"
                           survey_localization = "en_US",
                           sample_size,
                           expected_ir = 80, # expected IR in %
                           survey_length, # length of survey, in minutes
                           offer_price = NULL, # CPI to offer for payment in dollars - can leave empty for market price
                           field_time = 14, # field time in days
                           live_url,
                           test_url,
                           access_token) # user-specific API access token, which PureSpectrum should give you
  {
  
  request_body <- list(survey_title = survey_title,
                       survey_category_code = survey_category_code,  # exciting-new
                       survey_localization = survey_localization,
                       completes_required = sample_size,
                       expected_ir = expected_ir, # expected IR in %
                       expected_loi = survey_length, # length of survey, in minutes
                       offer_price = offer_price, 
                       field_time = field_time, # field time in days
                       live_url = live_url,
                       test_url = test_url
  )
  request_body <- request_body %>% discard(is.null) 
  
  
  basepath <- "https://staging.spectrumsurveys.com/buyers/v2/surveys"
  r <- POST(basepath, 
            add_headers(`access-token` = access_token, accept = "application/json"),
            body = request_body, 
            encode = "json")
  
  if(r$status_code==201){
    return(cat(paste0("Success: survey posted\nPS Test link: ",
                  content(r,as = "parsed")$test_ps_survey_entry_link,"\n",
                  "PS survey ID: ",content(r,as = "parsed")$ps_survey_id))
           )
  } else if (r$status_code==401){
    return(cat(paste0("Error: survey not posted.\nPS API returned ",r$status_code," error: ",
                      content(r,as = "parsed")$msg))
    )
  } else{
    return(cat(paste0("Error: survey not posted.\nPS API returned ",r$status_code," error: ",
                      content(r,as = "parsed")$ps_api_response_message))
    )
  }
}


## Survey with quotas: ---------------------------------------------------------

ps_post_survey <- function(survey_title = NULL,
                           survey_category_code = 232,  # default: "exciting-new"
                           survey_localization = "en_US",
                           sample_size,
                           expected_ir = 80, # expected IR in %
                           survey_length = NULL, # length of survey, in minutes
                           offer_price = NULL, # CPI to offer for payment in dollars - can leave empty for market price
                           field_time = 14, # field time in days
                           live_url,
                           test_url,
                           zips = NULL, # if targeting by zip code provide enquoted character vector of zip codes
                           counties = NULL,
                           states = NULL, # if targeting by states, provide enquoted list of FIPS codes from here: https://purespectrum.atlassian.net/wiki/spaces/PBA/pages/34552054/Location+codes+US+States+to+codes+mapping
                           quotas = NULL, # tidy df with columns "quota_name" "quota_category" "quota_prop"
                           access_token = NULL) # user-specific API access token, which PureSpectrum should give you
{
  ## sample quota DF below:
  # quotas <- data.frame(sample_frame ="St. Louis",
  #                      quota_name = c(rep("race",5),
  #                                     rep("gender",2)),
  #                      quota_category = c(c("white","black","hispanic","asian","other"),
  #                                         c("male","female")
  #                      ),
  #                      quota_prop = c(c(0.6,0.3,0.05,0.05,0),
  #                                     c(0.48,0.52))
  # )
  # quotas <- read_csv("sample_quotas.csv")
  
  
  ## create actual target numbers from the quota %s ----------------------------
  quotas <- mutate(quotas,
                   quota_value = round(quota_prop*sample_size,1))
  
  
  # create geographic qualifications -------------------------------------------
  if(!is.null(zips)){zips_list <- list(qualification_code = 229, # zip codes
                                       condition_codes = c(zips)
  )
  } else{zips_list <- NULL}
  
  if(!is.null(counties)){counties_list <- list(qualification_code = 228, # counties
                                               condition_codes = c(counties)
  )
  } else{counties_list <- NULL}
  
  if(!is.null(states)){states_list <- list(qualification_code = 225, # state
                                           condition_codes = c(states)
  )
  } else{states_list <- NULL}
  
  
  
  # Setup qualifications -------------------------------------------------------
  qualifications_list <- list(
    list(qualification_code = 211, # gender
         condition_codes = c("111","112") # codes must be in quotations marks
    ),
    list(qualification_code = 214, # race
         condition_codes = c("111","112","113","114","115","116","117") # for race, can't use 118-120 even though they're in Qualifications doc here: https://purespectrum.atlassian.net/wiki/spaces/PBA/pages/33493049/Qualification+and+Condition+Codes
    ),
    list(qualification_code = 212, # age
         range_sets = list(list(from=18,
                           to = 99,
                           units=311)) # 311=years, set units to 312 for months instead per https://purespectrum.atlassian.net/wiki/spaces/PBA/pages/735903884/Codes+Units
    ),
    list(qualification_code = 213, # income
         range_sets = list(list(from=0,
                                to = 999999,
                                units=321)) # 321=USD
    ),
    list(qualification_code = 217, # relationship status
         condition_codes = c("111","112","113","114","115","116") # no 117, https://purespectrum.atlassian.net/wiki/spaces/PBA/pages/33493049/Qualification+and+Condition+Codes
    ),
    list(qualification_code = 218, # children or not
         condition_codes = c("111","112") # per https://purespectrum.atlassian.net/wiki/spaces/PBA/pages/33493049/Qualification+and+Condition+Codes
    ),
    list(qualification_code = 215, # employment
         condition_codes = c("111","112","113","114","115") # 116-117 not available in US despite https://purespectrum.atlassian.net/wiki/spaces/PBA/pages/33493049/Qualification+and+Condition+Codes
    ),
    list(qualification_code = 216, # education
         condition_codes = c("111","112","113","114","115","116") # can't use 117-119 despite https://purespectrum.atlassian.net/wiki/spaces/PBA/pages/33493049/Qualification+and+Condition+Codes
    ),
    
    zips_list,
    counties_list,
    states_list
  )
  
  # remove empty qualifications - for instance, if not targeting by zip and doing states instead:
  qualifications_list <- qualifications_list %>% discard(is.null) 
  

  # Setup quotas ---------------------------------------------------------------
  if("gender" %in% quotas$quota_name){
    gender_quotas_list <- list(
      list(buyer_quota_id = "gender-quota-men",
           required_count = filter(quotas,quota_name=="gender" & quota_category=="male") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  211,
                                condition_codes= list("111")))
      ),
      list(buyer_quota_id = "gender-quota-women",
           required_count = filter(quotas,quota_name=="gender" & quota_category=="female") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  211,
                                condition_codes= list("112")))
      )
    )
  } else{gender_quotas_list <- NULL}
  
  if("race" %in% quotas$quota_name){
    race_quotas_list <- list(
      list(buyer_quota_id = "race-quota-white",
           required_count = filter(quotas,quota_name=="race" & quota_category=="white") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  214,
                                condition_codes= list("111")))
      ),
      list(buyer_quota_id = "race-quota-black",
           required_count = filter(quotas,quota_name=="race" & quota_category=="black") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  214,
                                condition_codes= list("113")))
      ),
      list(buyer_quota_id = "race-quota-hispanic",
           required_count = filter(quotas,quota_name=="race" & quota_category=="hispanic") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  214,
                                condition_codes= list("112")))
      ),
      list(buyer_quota_id = "race-quota-asian",
           required_count = filter(quotas,quota_name=="race" & quota_category=="asian") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  214,
                                condition_codes= list("114")))
      ),
      list(buyer_quota_id = "race-quota-other",
           required_count = filter(quotas,quota_name=="race" & quota_category=="other") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  214,
                                condition_codes= list("115","116","117")))
      )
    )
  } else{race_quotas_list <- NULL}
  
  if("age" %in% quotas$quota_name){
    if("18-29" %in% quotas$quota_category){# Set up for Justin's age categories
      age_quotas_list <- list(
        list(buyer_quota_id = "age-quota-18-29", # rename this quota whatever you want
             required_count = filter(quotas,quota_name=="age" & quota_category=="18-29") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=18,to=29, # set bin range for age here
                                                        units=311))))
        ),
        list(buyer_quota_id = "age-quota-30-44",
             required_count = filter(quotas,quota_name=="age" & quota_category=="30-44") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=30,to=44, # set bin range for age here
                                                        units=311))))
        ),
        list(buyer_quota_id = "age-quota-45-64",
             required_count = filter(quotas,quota_name=="age" & quota_category=="45-64") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=45,to=64, # set bin range for age here
                                                        units=311))))
        ),
        list(buyer_quota_id = "age-quota-65p",
             required_count = filter(quotas,quota_name=="age" & quota_category=="65-99") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                                        units=311))))
        )
      )
    }
    if("18-24" %in% quotas$quota_category){ # Set up for Matt's age categories instead
      age_quotas_list <- list(
        list(buyer_quota_id = "age-quota-18-24", # rename this quota whatever you want
             required_count = filter(quotas,quota_name=="age" & quota_category=="18-24") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                                        units=311))))
        ),
        list(buyer_quota_id = "age-quota-25-34",
             required_count = filter(quotas,quota_name=="age" & quota_category=="25-34") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                                        units=311))))
        ),
        list(buyer_quota_id = "age-quota-35-44",
             required_count = filter(quotas,quota_name=="age" & quota_category=="35-44") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                                        units=311))))
        ),
        list(buyer_quota_id = "age-quota-45-54",
             required_count = filter(quotas,quota_name=="age" & quota_category=="45-54") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                                        units=311))))
        ),
        list(buyer_quota_id = "age-quota-55-64",
             required_count = filter(quotas,quota_name=="age" & quota_category=="55-64") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                                        units=311))))
        ),
        list(buyer_quota_id = "age-quota-65p",
             required_count = filter(quotas,quota_name=="age" & quota_category=="65-99") %>% select(quota_value) %>% as.numeric(),
             criteria = list(list(qualification_code =  212,
                                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                                        units=311))))
        )
      )
    }
  } else{age_quotas_list <- NULL}
  
  if("education" %in% quotas$quota_name){
    educ_quotas_list <- list(
      list(buyer_quota_id = "educ-quota-nohs",
           required_count = filter(quotas,quota_name=="education" & quota_category=="no hs") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  216,
                                condition_codes= list("111"))) # some HS
      ),
      list(buyer_quota_id = "educ-quota-hs",
           required_count = filter(quotas,quota_name=="education" & quota_category=="hs") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  216,
                                condition_codes= list("112"))) # high school grad
      ),
      list(buyer_quota_id = "educ-quota-somecoll",
           required_count = filter(quotas,quota_name=="education" & quota_category=="some college") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  216,
                                condition_codes= list("113"))) # people with some college
      ),
      list(buyer_quota_id = "educ-quota-colldeg",
           required_count = filter(quotas,quota_name=="education" & quota_category=="college degree") %>% select(quota_value) %>% as.numeric(),
           criteria = list(list(qualification_code =  216,
                                condition_codes= list("114","115","116"))) # bachelors, masters, doctorate
      )
      
    )
  } else{
    educ_quotas_list <- list(buyer_quota_id = "educ-quota-all",
                             required_count = sample_size,
                             criteria = list(list(qualification_code =  216,
                                                  condition_codes= list("111","112","113","114","115","116")))
    )
  }
  
  ## Setup nested quotas -------------------------------------------------------
  # doing gender X race X age (Matt's age categories), 
  # race X gender, 
  # and gender X age  (Matt's age categories)
  
  if("gender-race-age" %in% quotas$quota_name){
    genderXraceXage_quotas_list <- list(
      list(buyer_quota_id = "genderXraceXage-quota-male-white-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-white-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-white-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-white-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-white-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-white-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-white-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-white-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-white-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-white-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-white-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-white-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXraceXage-quota-female-white-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-white-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-white-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-white-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-white-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-white-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-white-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-white-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-white-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-white-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-white-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-white-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      
      list(buyer_quota_id = "genderXraceXage-quota-male-black-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-black-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-black-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-black-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-black-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-black-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-black-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-black-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-black-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-black-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-black-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-black-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXraceXage-quota-female-black-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-black-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-black-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-black-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-black-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-black-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-black-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-black-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-black-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-black-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-black-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-black-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("113")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXraceXage-quota-male-hispanic-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-hispanic-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-hispanic-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-hispanic-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-hispanic-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-hispanic-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-hispanic-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-hispanic-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-hispanic-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-hispanic-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-hispanic-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-hispanic-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXraceXage-quota-female-hispanic-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-hispanic-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-hispanic-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-hispanic-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-hispanic-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-hispanic-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-hispanic-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-hispanic-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-hispanic-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-hispanic-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-hispanic-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-hispanic-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXraceXage-quota-male-asian-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-asian-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-asian-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-asian-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-asian-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-asian-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-asian-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-asian-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-asian-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-asian-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-asian-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-asian-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXraceXage-quota-female-asian-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-asian-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-asian-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-asian-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-asian-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-asian-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-asian-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-asian-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-asian-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-asian-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-asian-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-asian-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("114")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXraceXage-quota-male-other-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-other-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-other-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-other-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-other-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-other-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-other-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-other-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-other-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-other-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-male-other-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="male-other-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXraceXage-quota-female-other-18-24",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-other-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-other-25-34",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-other-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-other-35-44",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-other-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-other-45-54",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-other-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-other-55-64",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-other-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXraceXage-quota-female-other-65-99",
           required_count = filter(quotas,quota_name=="gender-race-age" & quota_category=="female-other-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      )
    )
  } else{genderXraceXage_quotas_list <- NULL}
  
  if("gender-race" %in% quotas$quota_name){
    genderXrace_quotas_list <- list(
      list(buyer_quota_id = "genderXrace-quota-male-white",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="male-white") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("111"))
           )
      ),
      
      list(buyer_quota_id = "genderXrace-quota-female-white",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="female-white") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("111"))
           )
      ),
      
      list(buyer_quota_id = "genderXrace-quota-male-black",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="male-black") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("113"))
           )
      ),
      
      list(buyer_quota_id = "genderXrace-quota-female-black",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="female-black") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("113"))
           )
      ),
      list(buyer_quota_id = "genderXrace-quota-male-hispanic",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="male-hispanic") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("112"))
           )
      ),
      
      list(buyer_quota_id = "genderXrace-quota-female-hispanic",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="female-hispanic") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("112"))
           )
      ),
      list(buyer_quota_id = "genderXrace-quota-male-asian",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="male-asian") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("114"))
           )
      ),
      
      list(buyer_quota_id = "genderXrace-quota-female-asian",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="female-asian") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("114"))
           )
      ),
      list(buyer_quota_id = "genderXrace-quota-male-other",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="male-other") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117"))
           )
      ),
      
      list(buyer_quota_id = "genderXrace-quota-female-other",
           required_count = filter(quotas,quota_name=="gender-race" & quota_category=="female-other") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  214,
                  condition_codes= list("115","116","117"))
           )
      )
    )
  } else{genderXrace_quotas_list <- NULL}
  
  if("gender-age" %in% quotas$quota_name){
    genderXage_quotas_list <- list(
      list(buyer_quota_id = "genderXage-quota-male-18-24",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="male-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-male-25-34",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="male-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-male-35-44",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="male-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-male-45-54",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="male-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-male-55-64",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="male-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-male-65-99",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="male-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("111")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      ),
      
      list(buyer_quota_id = "genderXage-quota-female-18-24",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="female-18-24") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=18,to=24, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-female-25-34",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="female-25-34") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=25,to=34, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-female-35-44",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="female-35-44") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=35,to=44, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-female-45-54",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="female-45-54") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=45,to=54, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-female-55-64",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="female-55-64") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=55,to=64, # set bin range for age here
                                        units=311)))
           )
      ),
      list(buyer_quota_id = "genderXage-quota-female-65-99",
           required_count = filter(quotas,quota_name=="gender-age" & quota_category=="female-65-99") %>% select(quota_value) %>% as.numeric(),
           criteria = list(
             list(qualification_code =  211,
                  condition_codes= list("112")),
             list(qualification_code =  212,
                  range_sets= list(list(from=65,to=99, # set bin range for age here
                                        units=311)))
           )
      )
    )
  } else{genderXage_quotas_list <- NULL}
  
  ## Setup fake quotas to enable variable pass-thru ----------------------------
  fake_quotas <- list(
    list(buyer_quota_id = "income-quota-all",
         required_count = sample_size,
         criteria = list(list(qualification_code =  213,
                              range_sets= list(list(from=0,to=999999, # set bin range for age here
                                                    units=321))))
    ),
    list(buyer_quota_id = "relationship-quota-all",
         required_count = sample_size,
         criteria = list(list(qualification_code =  217,
                              condition_codes= list("111","112","113","114","115","116")))
    ),
    list(buyer_quota_id = "children-quota-all",
         required_count = sample_size,
         criteria = list(list(qualification_code =  218,
                              condition_codes= list("111","112")))
    ),
    list(buyer_quota_id = "employment-quota-all",
         required_count = sample_size,
         criteria = list(list(qualification_code =  215,
                              condition_codes= list("111","112","113","114","115")))
    )
  )
  
  quota_list <-  c(
    gender_quotas_list,
    race_quotas_list,
    age_quotas_list,
    educ_quotas_list,
    genderXraceXage_quotas_list,
    genderXrace_quotas_list,
    genderXage_quotas_list,
    fake_quotas
    
  )
  
  # remove empty quotas - for instance, if not nesting/not using one of the characteristics as quotas
  quota_list <- quota_list %>% discard(is.null) 
  
  
  ## Create API request body ---------------------------------------------------
  request_body <- list(
    survey_title = survey_title,
    survey_category_code = survey_category_code,  # exciting-new
    survey_localization = survey_localization,
    completes_required = sample_size,
    expected_ir = expected_ir, # expected IR in %
    expected_loi = survey_length, # length of survey, in minutes
    offer_price = offer_price,
    field_time = field_time, # field time in days
    live_url = live_url,
    test_url = test_url,
    
    qualifications = qualifications_list,
    quotas = quota_list
  )
  
  request_body <- request_body %>% discard(is.null) 
  
  basepath <- "https://staging.spectrumsurveys.com/buyers/v2/surveys"
  
  # Post API request -----------------------------------------------------------
  r <- POST(basepath, 
            add_headers(`access-token` = access_token, accept = "application/json"),
            body = request_body, 
            encode = "json")
  
  # Parse server response ------------------------------------------------------
  if(r$status_code==201){
    return(cat(paste0("Success: survey posted\nPS Test link: ",
                      content(r,as = "parsed")$test_ps_survey_entry_link,"\n",
                      "PS survey ID: ",content(r,as = "parsed")$ps_survey_id))
    )
  } else if (r$status_code==401){
    return(cat(paste0("Error: survey not posted.\nPS API returned error: ",
                      content(r,as = "parsed")$msg))
    )
  } else{
    return(cat(paste0("Error: survey not posted.\nPS API returned error: ",
                      content(r,as = "parsed")$ps_api_response_message))
    )
  }
}

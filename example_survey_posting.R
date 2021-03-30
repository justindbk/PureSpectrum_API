library(tidyverse)

my_token <- "" # add your API access key here

source("purespectrum_functions.R")

quotas <- read_csv("sample_quotas.csv")

ps_post_survey_noquotas(survey_title = "sample-noquotas-noprice",
               survey_category_code = 232,  # exciting-new
               survey_localization = "en_US",
               sample_size = 200,
               expected_ir = 80, # expected IR in %
               survey_length = 10, # length of survey, in minutes
               # offer_price = NULL, # no CPI
               field_time = 14, # field time in days
               live_url = "https://harvard.az1.qualtrics.com/jfe/form/SV_5d41NokucVvbGbY?sample_frame=St. Louis&zipcode=[%%229%%]&gender=[%%211%%]&age=[%%212%%]&race=[%%214%%]&educ=[%%216%%]&state=[%%225%%]&inc=[%%213%%]&relat=[%%217%%]&child=[%%218%%]&emp=[%%215%%]",
               test_url = "https://harvard.az1.qualtrics.com/jfe/preview/SV_5d41NokucVvbGbY?Q_CHL=preview&Q_SurveyVersionID=current&sample_frame=St. Louis&zipcode=[%%229%%]&gender=[%%211%%]&age=[%%212%%]&race=[%%214%%]&educ=[%%216%%]&state=[%%225%%]&inc=[%%213%%]&relat=[%%217%%]&child=[%%218%%]&emp=[%%215%%]",
               access_token = my_token
)

ps_post_survey(survey_title = "sample-wquotas-nested-nogeo",
               survey_category_code = 232,  # exciting-new
               survey_localization = "en_US",
               sample_size = 200,
               expected_ir = 80, # expected IR in %
               survey_length = 10, # length of survey, in minutes
               # offer_price = 1.5, # leaving this out
               field_time = 14, # field time in days
               live_url = "https://harvard.az1.qualtrics.com/jfe/form/SV_5d41NokucVvbGbY?sample_frame=St. Louis&zipcode=[%%229%%]&gender=[%%211%%]&age=[%%212%%]&race=[%%214%%]&educ=[%%216%%]&state=[%%225%%]&inc=[%%213%%]&relat=[%%217%%]&child=[%%218%%]&emp=[%%215%%]",
               test_url = "https://harvard.az1.qualtrics.com/jfe/preview/SV_5d41NokucVvbGbY?Q_CHL=preview&Q_SurveyVersionID=current&sample_frame=St. Louis&zipcode=[%%229%%]&gender=[%%211%%]&age=[%%212%%]&race=[%%214%%]&educ=[%%216%%]&state=[%%225%%]&inc=[%%213%%]&relat=[%%217%%]&child=[%%218%%]&emp=[%%215%%]",
               quotas = filter(quotas,sample_frame=="St. Louis"),
               access_token = my_token
)

ps_post_survey(survey_title = "sample-st.louis-wquotas-nested-wzips",
               survey_category_code = 232,  # exciting-new
               survey_localization = "en_US",
               sample_size = 200,
               expected_ir = 80, # expected IR in %
               survey_length = 10, # length of survey, in minutes
               offer_price = 1.5,
               field_time = 14, # field time in days
               live_url = "https://harvard.az1.qualtrics.com/jfe/form/SV_5d41NokucVvbGbY?sample_frame=St. Louis&zipcode=[%%229%%]&gender=[%%211%%]&age=[%%212%%]&race=[%%214%%]&educ=[%%216%%]&state=[%%225%%]&inc=[%%213%%]&relat=[%%217%%]&child=[%%218%%]&emp=[%%215%%]",
               test_url = "https://harvard.az1.qualtrics.com/jfe/preview/SV_5d41NokucVvbGbY?Q_CHL=preview&Q_SurveyVersionID=current&sample_frame=St. Louis&zipcode=[%%229%%]&gender=[%%211%%]&age=[%%212%%]&race=[%%214%%]&educ=[%%216%%]&state=[%%225%%]&inc=[%%213%%]&relat=[%%217%%]&child=[%%218%%]&emp=[%%215%%]",
               quotas = filter(quotas,sample_frame=="St. Louis"),
               zips = c("02138","02139"), # vector list of enquoted zip codes
               access_token = my_token
)

ps_post_survey(survey_title = "sample-wquotas-nested-wstate",
               survey_category_code = 232,  # exciting-new
               survey_localization = "en_US",
               sample_size = 2000,
               expected_ir = 80, # expected IR in %
               survey_length = 10, # length of survey, in minutes
               # offer_price = 1.5,
               field_time = 14, # field time in days
               live_url = "https://harvard.az1.qualtrics.com/jfe/form/SV_5d41NokucVvbGbY?sample_frame=St. Louis&zipcode=[%%229%%]&gender=[%%211%%]&age=[%%212%%]&race=[%%214%%]&educ=[%%216%%]&state=[%%225%%]&inc=[%%213%%]&relat=[%%217%%]&child=[%%218%%]&emp=[%%215%%]",
               test_url = "https://harvard.az1.qualtrics.com/jfe/preview/SV_5d41NokucVvbGbY?Q_CHL=preview&Q_SurveyVersionID=current&sample_frame=St. Louis&zipcode=[%%229%%]&gender=[%%211%%]&age=[%%212%%]&race=[%%214%%]&educ=[%%216%%]&state=[%%225%%]&inc=[%%213%%]&relat=[%%217%%]&child=[%%218%%]&emp=[%%215%%]",
               quotas = filter(quotas,sample_frame=="Massachusetts"),
               states = filter(quotas,sample_frame=="Massachusetts") %>% select(state_fips_code) %>% unique() %>% as.character(),
               access_token = my_token
)

# get limesurvey


library(limer)


# Setup API details
options(lime_api = 'http://opinify.solutions/survey/admin/remotecontrol')
options(lime_username = 'admin')
options(lime_password = 'pUTAMEN123')

# Do stuff with LimeSurvey API
get_session_key()
responses <- get_responses(348543, sLanguageCode= 'en', sResponseType='short')


call_limer(method = "list_surveys")


release_session_key()

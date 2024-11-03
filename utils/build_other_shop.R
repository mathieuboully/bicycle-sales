# Fonctions
source(file.path("./utils/openrouteservice_api.R"), encoding = "UTF-8")

path_data = "./data"

openrouteservice::ors_pois(request = "pois", api_key = "5b3ce3597851110001cf624862acdd8f2bcc444c9771f00176cb6898", geometry = list(c(151.2, -33.9), c(151.3, -33.8)))

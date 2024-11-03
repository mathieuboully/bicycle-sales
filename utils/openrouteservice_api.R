library(httr)
library(openrouteservice)

# https://bookdown.org/nicohahn/making_maps_with_r5/docs/leaflet.html
# https://giscience.github.io/openrouteservice-r/articles/openrouteservice.html

api_key = "5b3ce3597851110001cf624862acdd8f2bcc444c9771f00176cb6898"

get_geocode_search = function(q, limit) {
  params =
    list(text = q, api_key = api_key)
  
  url =
    "https://api.openrouteservice.org/geocode/search"
  
  response =
    httr::GET(url, query = params)
  
  if (response$status_code == 200) {
    message("api.openrouteservice.org status 200")
    if (limit == 1) {
      res = list()
      res$coords$lat = content(response)$features[[1]]$geometry$coordinates[[2]]
      res$coords$lon = content(response)$features[[1]]$geometry$coordinates[[1]]
      return(res)
    } else if (limit == -1) {
      length_res = length(content(response)$features)
      res = list(coords = list(lat = c(), lon = c()))
      for (i in seq(1, length_res, 1)) {
        res$coords$lat = c(res$coords$lat,
                           content(response)$features[[1]]$geometry$coordinates[[2]])
        res$coords$lon = c(res$coords$lon,
                           content(response)$features[[1]]$geometry$coordinates[[1]])
        return(res)
      }
    }
  }  else {
    warning("api.openrouteservice.org status different 200")
    return(NA)
  }
}

get_geocode_search_structured = function(limit,
                                         address = "",
                                         postalcode = "",
                                         country = "",
                                         region = "",
                                         locality = "") {
  params =
    list(
      address = address,
      postalcode = postalcode,
      country = country,
      region = region,
      locality = locality,
      api_key = api_key
    )
  
  url =
    "https://api.openrouteservice.org/geocode/search/structured"
  # https://bookdown.org/nicohahn/making_maps_with_r5/docs/leaflet.html
  response =
    httr::GET(url, query = params)
  
  if (response$status_code == 200) {
    message("api.openrouteservice.org status 200")
    if (limit == 1) {
      res = list()
      res$coords$lat = content(response)$features[[1]]$geometry$coordinates[[2]]
      res$coords$lon = content(response)$features[[1]]$geometry$coordinates[[1]]
      return(res)
    } else if (limit == -1) {
      length_res = length(content(response)$features)
      res = list(coords = list(lat = c(), lon = c()))
      res = list()
      for (i in seq(1, length_res, 1)) {
        res$coords$lat = c(res$coords$lat,
                           content(response)$features[[1]]$geometry$coordinates[[2]])
        res$coords$lon = c(res$coords$lon,
                           content(response)$features[[1]]$geometry$coordinates[[1]])
        return(res)
      }
    }
  }  else {
    warning("api.openrouteservice.org status different 200")
    return(NA)
  }
}

get_pois = function(lat, lon, poi_type, buffer = 200) {
  geometry <- list(geojson = list(type = "Point", coordinates = c(lon, lat)),
                   buffer = 2000)
  
  res = ors_pois(
    api_key = api_key,
    request = 'pois',
    geometry = geometry,
    limit = 2000,
    sortby = "distance",
    filters = list(category_ids = 429),
    output = "sf"
  )
  
  return(res)
}

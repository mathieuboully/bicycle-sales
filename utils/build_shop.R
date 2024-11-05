# Fonctions
source(file.path("./utils/openrouteservice_api.R"), encoding = "UTF-8")
source(file.path("./utils/web_scraping.R"), encoding = "UTF-8")

path_data = "./data"

build_shop = function(other_shop_buffer = 100, mode_iso = "car", range_iso =1800, interval_iso = 600) {
  shop = data.frame(
    shop_url = as.character(),
    shop_name = as.character(),
    image_url = as.character(),
    direction_url = as.character(),
    address = as.character(),
    locality = as.character(),
    postcode = as.integer(),
    email = as.character(),
    phone = as.character(),
    lat = as.numeric(),
    lon = as.numeric()
  )
  
  other_shop = data.frame()
  
  page = rvest::read_html("https://www.99bikes.com.au/stores")
  
  city_urls = get_attr(page = page,
                       xpath = "//a[contains(text(), 'all store')]",
                       attr = "href")
  
  for (city_url in city_urls[1:2]) {
    message(city_url)
    page = rvest::read_html(paste0("https://www.99bikes.com.au/", city_url))
    
    shop_urls = get_attr(page = page,
                         xpath = "//h3//a",
                         attr = "href")
    
    for (shop_url in shop_urls) {
      message(shop_url)
      page = rvest::read_html(paste0("https://www.99bikes.com.au/", shop_url))
      
      shop_name = get_text(page = page, xpath = "//h1")
      
      postcode = get_text(page = page, xpath = "//p[contains(@class, 'store-address')]") %>%
        str_extract(., "\\d{4}")
      
      address = get_text(page = page, xpath = "//p[contains(@class, 'store-address-wrap')]/br[1]/following-sibling::text()[1]")
      
      locality = get_text(page = page, xpath = "//p[contains(@class, 'store-address-wrap')]/br[2]/following-sibling::text()[1]")
      
      image_url = get_attr(page = page,
                           xpath = "//img[contains(@class, 'store-image')]",
                           attr = "src")
      
      direction_url = get_attr(page = page,
                               xpath = "//div[contains(@class, 'store_directions')]//a",
                               attr = "href")
      
      coords = get_text(page = page, xpath = "//p[contains(@class, 'store-address-wrap')]//a")
      
      phone = coords[1]
      email = coords[2]
      if (nrow(shop) %% 100 == 0 & nrow(shop) != 0) {
        Sys.sleep(61)
      }
      res = get_geocode_search(q = paste(address, locality, "Australia"),
                               limit = 1)
      
      lat = res$coords$lat
      lon = res$coords$lon
      
      other_shop = rbind(
        other_shop,
        get_pois(
          lat = lat,
          lon = lon,
          poi_type = 429,
          buffer = other_shop_buffer
        )
      ) %>%
        data.frame(.)
      
      shop = rbind(
        shop,
        data.frame(
          shop_url = shop_url,
          shop_name = shop_name,
          image_url = image_url,
          direction_url = direction_url,
          address = address,
          locality = locality,
          postcode = postcode,
          email = email,
          phone = phone,
          lat = lat,
          lon = lon
        )
      ) %>%
        data.frame(.)
    }
  }
  
  isochrones = get_isochrones(
    lat_l = shop$lat[1:5],
    lon_l = shop$lon[1:5],
    mode = mode_iso,
    range = range_iso,
    interval = interval_iso
  ) %>%
    dplyr::mutate(mode = mode_iso)
  
  shops = list(
    shop = shop,
    other_shop = other_shop,
    isochrones = isochrones,
    other_shop_buffer = other_shop_buffer
  )
  save(shops, file = "./data/shops.rds")
  return(shops)
}

build_shop(other_shop_buffer = 500, mode_iso = "car", range_iso = 1800, interval_iso = 600)

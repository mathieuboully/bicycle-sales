# Fonctions
source(file.path("./utils/openrouteservice_api.R"), encoding = "UTF-8")

path_data = "./data"

build_customer = function() {
  customer_address = readxl::read_xlsx(file.path(path_data, "Customer Address.xlsx"))
  customer_demography = readxl::read_xlsx(file.path(path_data, "Customer Demography.xlsx"))
  
  customer = customer_address %>%
    left_join(., customer_demography, by = c("customer_id")) %>%
    mutate(
      gender = case_when(
        gender %in% c("Male", "M") ~ "Homme",
        gender %in% c("Femal", "F") ~ "Femme",
        TRUE ~ NA
      ),
      state = case_when(
        state %in% c("New South Wales", "NSW") ~ "New South Wales",
        state %in% c("Victoria", "VIC") ~ "Victoria",
        state %in% c("QLD") ~ "Queensland",
        TRUE ~ NA
      )
    ) %>%
    data.frame(.)
  
  customer = customer[1:10, ] %>%
    rowwise() %>%
    dplyr::mutate(geocode = get_geocode_search_structured(limit = 1, address = address, postalcode = as.character(postcode), region = state, country = country)) %>%
    tidyr::unnest_wider(geocode) %>%
    as.data.frame(.)
  
  save(customer, file = "./data/customer.rds")
  return(customer)
}
build_customer()

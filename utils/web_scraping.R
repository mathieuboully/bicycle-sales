library(rvest)

get_attr = function(page, xpath, attr) {
  res = page %>%
    html_nodes(xpath = xpath) %>%
    html_attr(attr)
  return(res)
}

get_text = function(page, xpath) {
  res = page %>%
    html_nodes(xpath = xpath) %>%
    html_text(trim = TRUE)
  return(res)
}
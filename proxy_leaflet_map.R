library(tidyverse)
library(sf)
library(leaflet)
library(leafpop)
library(R.matlab)
library(htmlwidgets)


#downloaded from https://zenodo.org/record/1189006
proxies <- readMat("~/Downloads/proxydata_aprmar_lmr_v0.2.0_pages2k_v2.mat")
proxies_df <-  tibble('msrmt' = unlist(proxies[["msrmt"]]),
                      'archive' = unlist(proxies[["archive"]]),
                      'names' = unlist(proxies[["lmr2k.names"]]),
                      'lat' = unlist(proxies[["p.lat"]]),
                      'lon' = unlist(proxies[["p.lon"]])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

#using the metbrewer nattier pallette
pal <- colorFactor(c("#022a2a",'#184948', '#7f793c',
                     '#52271c','#c08e39',"184948"),
                   levels = c("Marine Cores",'Lake Cores',
                              "Tree Rings","Speleothems",
                              "Corals and Sclerosponges",
                              "Ice Cores"))

proxy_data <- proxies$lmr2k.data %>% as_tibble() %>% 
  pivot_longer(cols=everything()) %>% 
  group_by(name) %>% nest() %>% ungroup() %>%
  mutate(name = proxies_df$names)

d_plots <- proxy_data %>% 
  mutate(plot = map2(data, name,
                     ~ ggplot(data = .x, aes(x = proxies$year, y = value)) +
                       geom_point(size=.1, alpha=.5) + ggtitle(.y) +
                       theme_minimal()
  ))


map <- leaflet(proxies_df) %>% 
  setView(lng = 116.84438, lat = -20.73488, zoom = 2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(color = ~pal(archive), label = ~names,
                   radius=9, stroke = FALSE, fillOpacity = 0.5,
                   popup = popupGraph(d_plots$plot, height=120)) %>% 
  addLegend(pal = pal, values = ~archive,
            position = 'bottomright', opacity = 1,
            title = "Proxy type") %>% 
  htmlwidgets::onRender(paste0("
    function(el, x) {
      $('head').append('<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no\" />');
    }"))

map

saveWidget(map, file="index.html", title="PHYDA proxies map", selfcontained = TRUE)

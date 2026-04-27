library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(leaflet)
library(plotly)
library(ggplot2)
library(forcats)
library(scales)
library(leaflet.extras)

# ---------------------------------------------------------
# LOAD DATA
# ---------------------------------------------------------
map_df <- read.csv("map_pa.csv", stringsAsFactors = FALSE)

checkins <- read_csv("philly_checkins.csv", show_col_types = FALSE)


# ---------------------------------------------------------
# CLEANING
# ---------------------------------------------------------
map_df <- map_df %>%
  select(
    business_id, name, latitude, longitude, stars, review_count,
    address, categories, major_cuisine, RestaurantsPriceRange2,
    NoiseLevel, Alcohol_type,
    trendy, romantic, intimate, touristy, hipster, divey, classy, upscale, casual,
    RestaurantsTakeOut, RestaurantsReservations, Caters, DriveThru,
    breakfast, lunch, dinner, latenight, dessert, brunch,
    Alcohol_clean, HappyHour, RestaurantsGoodForGroups, GoodForKids,
    DogsAllowed, WiFi_paid, CoatCheck, OutdoorSeating, Open24Hours,
    HasTV, BusinessAcceptsBitcoin, WheelchairAccessible,
    background_music, jukebox, live, video, karaoke
  )

bool_cols <- c(
  "trendy", "romantic", "intimate", "touristy", "hipster",
  "divey", "classy", "upscale", "casual",
  "RestaurantsTakeOut", "RestaurantsReservations", "Caters", "DriveThru",
  "breakfast", "lunch", "dinner", "latenight", "dessert", "brunch",
  "Alcohol_clean", "HappyHour", "RestaurantsGoodForGroups", "GoodForKids",
  "DogsAllowed", "WiFi_paid", "CoatCheck", "OutdoorSeating", "Open24Hours",
  "HasTV", "BusinessAcceptsBitcoin", "WheelchairAccessible",
  "background_music", "jukebox", "live", "video", "karaoke"
)

bool_cols_existing <- intersect(bool_cols, names(map_df))

map_df[bool_cols_existing] <- lapply(map_df[bool_cols_existing], function(x) {
  tolower(trimws(as.character(x))) == "true"
})
map_df$RestaurantsPriceRange2 <- ifelse(
  is.na(map_df$RestaurantsPriceRange2),
  "Unknown",
  map_df$RestaurantsPriceRange2
)

if (!"review_count" %in% names(map_df)) map_df$review_count <- 1
if (!"stars" %in% names(map_df)) map_df$stars <- NA_real_
if (!"address" %in% names(map_df)) map_df$address <- NA_character_
if (!"categories" %in% names(map_df)) map_df$categories <- NA_character_
if (!"major_cuisine" %in% names(map_df)) map_df$major_cuisine <- "Other"
if (!"postal_code" %in% names(map_df)) map_df$postal_code <- NA_character_
if (!"NoiseLevel" %in% names(map_df)) map_df$NoiseLevel <- NA_character_
if (!"Alcohol_type" %in% names(map_df)) map_df$Alcohol_type <- NA_character_
map_df$Alcohol_type <- tolower(trimws(as.character(map_df$Alcohol_type)))
if (!"RestaurantsPriceRange2" %in% names(map_df)) map_df$RestaurantsPriceRange2 <- NA_character_

# make sure checkins has parsed date/day/hour
if ("date" %in% names(checkins) && !"day" %in% names(checkins)) {
  checkins <- checkins %>%
    mutate(
      date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"),
      day = wday(date, label = TRUE, abbr = FALSE),
      hour = hour(date)
    )
}
map_df$NoiseLevel <- tolower(trimws(map_df$NoiseLevel))

map_df$NoiseLevel <- map_df$NoiseLevel %>%
  str_replace_all("'", "") %>%
  recode(
    "quiet" = "Quiet",
    "average" = "Moderate",
    "loud" = "Loud",
    "very_loud" = "Very Loud",
    .default = NA_character_
  )

# ---------------------------------------------------------
# PRECOMPUTED CHECKIN TABLES
# ---------------------------------------------------------

checkins_by_business_day_hour <- checkins %>%
  filter(!is.na(business_id), !is.na(day), !is.na(hour)) %>%
  count(business_id, day, hour, name = "n_checkins")

checkins_day_hour_summary <- checkins %>%
  filter(!is.na(business_id), !is.na(day), !is.na(hour)) %>%
  count(business_id, day, hour, name = "n")

# ---------------------------------------------------------
# HELPERS
# ---------------------------------------------------------
pretty_alcohol_label <- function(x) {
  dplyr::case_when(
    x == "none" ~ "No Alcohol",
    x == "beer_and_wine" ~ "Beer & Wine",
    x == "full_bar" ~ "Full Bar",
    TRUE ~ gsub("_", " ", x)
  )
}
stable_cuisine_colors <- c(
  "American" = "#4E79A7",
  "Italian" = "#F28E2B",
  "Chinese" = "#E15759",
  "Japanese" = "#76B7B2",
  "Mexican" = "#59A14F",
  "Indian" = "#EDC948",
  "Thai" = "#B07AA1",
  "Mediterranean" = "#FF9DA7",
  "French" = "#9C755F",
  "Korean" = "#BAB0AC",
  "Vietnamese" = "#86BCB6",
  "Middle Eastern" = "#FABFD2",
  "Other" = "#8CD17D"
)

stable_price_colors <- c(
  "Under 10 per person" = "#31a354",     # green (good deal)
  "$11-$30 per person" = "#a1d99b",    # soft green
  "$31-$60 per person" = "#fdae6b",   # orange (neutral)
  "Over $61 per person" = "#de2d26",   # red (expensive), 
  "Unknown" = "#bdbdbd"
)

stable_noise_colors <- c(
  "Quiet" = "#66c2a5",
  "Moderate" = "#fc8d62",
  "Loud" = "#8da0cb",
  "Very Loud" = "#e78ac3"
)

stable_alcohol_colors <- c(
  "none" = "#cfcfcf",
  "beer_and_wine" = "#80b1d3",
  "full_bar" = "#fb8072"
)
get_feature_colors <- function(values, feature_type) {
  values <- unique(as.character(values))
  values <- values[!is.na(values)]
  
  if (feature_type == "major_cuisine") {
    base_cols <- stable_cuisine_colors
  } else if (feature_type == "RestaurantsPriceRange2") {
    base_cols <- stable_price_colors
  } else if (feature_type == "NoiseLevel") {
    base_cols <- stable_noise_colors
  } else if (feature_type == "Alcohol_type") {
    base_cols <- stable_alcohol_colors
  } else {
    base_cols <- c()
  }
  
  missing_vals <- setdiff(values, names(base_cols))
  
  if (length(missing_vals) > 0) {
    extra_cols <- grDevices::hcl.colors(length(missing_vals), "Set 3")
    names(extra_cols) <- missing_vals
    base_cols <- c(base_cols, extra_cols)
  }
  
  base_cols[values]
}
all_cuisine_levels <- sort(unique(na.omit(as.character(map_df$major_cuisine))))
all_price_levels <- sort(unique(na.omit(as.character(map_df$RestaurantsPriceRange2))))
all_noise_levels <- sort(unique(na.omit(as.character(map_df$NoiseLevel))))
all_alcohol_levels <- sort(unique(na.omit(as.character(map_df$Alcohol_type))))

cuisine_palette_fixed <- get_feature_colors(all_cuisine_levels, "major_cuisine")
price_palette_fixed <- get_feature_colors(all_price_levels, "RestaurantsPriceRange2")
noise_palette_fixed <- get_feature_colors(all_noise_levels, "NoiseLevel")
alcohol_palette_fixed <- get_feature_colors(all_alcohol_levels, "Alcohol_type")

get_fixed_palette <- function(feature_type) {
  if (feature_type == "major_cuisine") {
    cuisine_palette_fixed
  } else if (feature_type == "RestaurantsPriceRange2") {
    price_palette_fixed
  } else if (feature_type == "NoiseLevel") {
    noise_palette_fixed
  } else if (feature_type == "Alcohol_type") {
    alcohol_palette_fixed
  } else {
    c()
  }
}
classify_traffic <- function(df_counts) {
  active_vals <- df_counts$n[df_counts$n > 0]
  
  if (length(active_vals) < 2) {
    df_counts %>%
      mutate(
        traffic_label = case_when(
          n == 0 ~ "Quiet",
          TRUE ~ "Moderate"
        )
      )
  } else {
    thresh_low <- quantile(active_vals, 0.33, na.rm = TRUE)
    thresh_high <- quantile(active_vals, 0.66, na.rm = TRUE)
    
    if (thresh_low == thresh_high) thresh_high <- thresh_low + 0.001
    
    df_counts %>%
      mutate(
        traffic_label = case_when(
          n == 0 ~ "No recorded activity",
          n <= thresh_low ~ "Quiet",
          n <= thresh_high ~ "Moderate",
          TRUE ~ "Busy"
        )
      )
  }
}
premium_hoverlabel <- list(
  bgcolor = "rgba(255,255,255,0.96)",
  bordercolor = "rgba(0,0,0,0.08)",
  font = list(
    family = "-apple-system, BlinkMacSystemFont, Segoe UI",
    size = 12,
    color = "#2b2b2b"
  ),
  align = "left"
)
build_clickable_legend_html <- function(groups, color_values, excluded_groups = character(0), title = "Legend") {
  
  items <- vapply(groups, function(g) {
    
    is_excluded <- g %in% excluded_groups
    opacity <- if (is_excluded) "0.45" else "1"
    
    paste0(
      "<a href='#' class='legend-filter-item' data-value=\"", htmltools::htmlEscape(g), "\" ",
      "style='display:flex; align-items:center; gap:8px; margin-bottom:8px; text-decoration:none; color:#2b2b2b; opacity:", opacity, ";'>",
      
      "<span style='display:inline-block; width:12px; height:12px; border-radius:50%; background:", color_values[[g]], ";'></span>",
      
      "<span style='font-size:13px;'>", htmltools::htmlEscape(g), "</span>",
      "</a>"
    )
    
  }, character(1))
  
  paste0(
    "<div style='background: rgba(255,255,255,0.96); padding: 12px 14px; border-radius: 12px; ",
    "box-shadow: 0 4px 14px rgba(0,0,0,0.12); border: 1px solid #e5e7eb; min-width: 150px;'>",
    
    "<div style='font-weight:700; margin-bottom:10px; font-size:13px;'>", htmltools::htmlEscape(title), "</div>",
    
    paste(items, collapse = ""),
    
    "<div style='margin-top:10px; font-size:11px; color:#777;'>Click to hide/show</div>",
    
    "</div>"
  )
}
# ---------------------------------------------------------
# UI
# ---------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
                    
    body {
      background: #f5f5f5;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      color: #2b2b2b;
    }

    .container-fluid {
      padding-left: 22px;
      padding-right: 22px;
    }

    .yelp-header {
      background: linear-gradient(90deg, #d32323 0%, #b91c1c 100%);
      color: white;
      padding: 18px 24px;
      border-radius: 16px;
      margin-top: 14px;
      margin-bottom: 18px;
      box-shadow: 0 8px 24px rgba(211, 35, 35, 0.18);
    }

    .yelp-header-left {
      display: flex;
      align-items: center;
      gap: 18px;
    }

    .yelp-logo {
      height: 52px;
      width: auto;
      background: white;
      border-radius: 12px;
      padding: 8px 10px;
    }

    .yelp-title {
      margin: 0;
      font-size: 30px;
      font-weight: 800;
      letter-spacing: -0.5px;
    }

    .yelp-subtitle {
      margin: 3px 0 0 0;
      font-size: 14px;
      color: rgba(255,255,255,0.88);
    }

    .main-content {
      margin-top: 10px;
    }

    .dashboard-card {
      background: #ffffff;
      border: 1px solid #e9e9e9;
      border-radius: 18px;
      padding: 16px;
      box-shadow: 0 4px 14px rgba(0,0,0,0.06);
    }

    .dashboard-card h4 {
      font-weight: 700;
      margin-top: 0;
      margin-bottom: 12px;
      color: #1f1f1f;
    }

    .filter-panel {
      position: absolute;
      bottom: 70px;
      left: 20px;
      z-index: 1000;
      width: 360px;
      max-height: 80vh;
      overflow-y: auto;
      background: #ffffff;
      padding: 18px;
      border-radius: 18px;
      box-shadow: 0 10px 26px rgba(0,0,0,0.18);
      border: 1px solid #ececec;
    }

    .filter-button .btn {
      border-radius: 999px !important;
      padding: 10px 18px !important;
      font-weight: 700 !important;
    }
    
    #toggle_filters,
    #toggle_rating_controls {
      background: #d32323 !important;
      color: white !important;
      border: none !important;
      box-shadow: 0 6px 16px rgba(211,35,35,0.22);
    }
    
    #toggle_filters:hover,
    #toggle_rating_controls:hover {
      background: #b91c1c !important;
      color: white !important;
    }
    
    #reset_view {
      background: white !important;
      color: #2b2b2b !important;
      border: 1px solid #dddddd !important;
    }

    .nav-tabs {
      border-bottom: 1px solid #ececec;
      margin-bottom: 10px;
    }

    .nav-tabs > li > a {
      border: none !important;
      border-radius: 10px 10px 0 0 !important;
      color: #666 !important;
      font-weight: 600;
      padding: 10px 14px;
    }

    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus {
      background: #fff5f5 !important;
      color: #d32323 !important;
      border-bottom: 3px solid #d32323 !important;
    }

    .form-control, .selectize-input {
      border-radius: 12px !important;
      border: 1px solid #dddddd !important;
      min-height: 42px;
      box-shadow: none !important;
    }

    .selectize-input.focus {
      border-color: #d32323 !important;
      box-shadow: 0 0 0 3px rgba(211,35,35,0.10) !important;
    }

    .analysis-card {
      background: #ffffff;
      border: 1px solid #e9e9e9;
      border-radius: 18px;
      padding: 18px;
      box-shadow: 0 4px 14px rgba(0,0,0,0.06);
      margin-top: 14px;
    }

    .detail-card {
      background: #ffffff;
      border: 1px solid #e8e8e8;
      border-radius: 18px;
      padding: 16px;
      min-height: 650px;
      box-shadow: 0 4px 14px rgba(0,0,0,0.06);
    }

    .detail-name {
      font-size: 24px;
      font-weight: 800;
      color: #1f1f1f;
      margin-bottom: 10px;
    }

    .detail-pill {
      display: inline-block;
      background: #fff1f1;
      color: #d32323;
      border: 1px solid #ffd4d4;
      border-radius: 999px;
      padding: 6px 10px;
      margin: 4px 6px 0 0;
      font-size: 12px;
      font-weight: 700;
    }

    .section-spacing {
      margin-top: 14px;
    }

    .btn-default {
      border-radius: 999px !important;
      border: 1px solid #dddddd !important;
      font-weight: 600;
      background: white !important;
      color: #2b2b2b !important;
    }
    .filter-button .btn,
    #reset_view.btn {
      background: #d32323 !important;
      color: white !important;     
      border: none !important;
    }

    .leaflet-container {
      border-radius: 18px;
      overflow: hidden;
    }
    .kpi-card {
      background: #ffffff;
      border: 1px solid #e9e9e9;
      border-radius: 18px;
      padding: 18px 20px;
      box-shadow: 0 4px 14px rgba(0,0,0,0.06);
      margin-bottom: 12px;
      min-height: 120px;
    }
    
    .kpi-label {
      font-size: 13px;
      font-weight: 700;
      color: #777;
      text-transform: uppercase;
      letter-spacing: 0.4px;
      margin-bottom: 10px;
    }
    
    .kpi-value {
      font-size: 32px;
      font-weight: 800;
      color: #d32323;
      line-height: 1.1;
    }
    
    .kpi-value-small {
      font-size: 22px;
      font-weight: 800;
      color: #d32323;
      line-height: 1.2;
    }
    .status-badge {
      display: inline-block;
      padding: 8px 14px;
      border-radius: 999px;
      font-size: 14px;
      font-weight: 800;
      text-transform: uppercase;
      letter-spacing: 0.4px;
    }
    
    .status-busy {
      background: #fde2e2;
      color: #b91c1c;
      border: 1px solid #f5b5b5;
    }
    
    .status-moderate {
      background: #fff4db;
      color: #b45309;
      border: 1px solid #f7d89a;
    }
    
    .status-quiet {
      background: #e7f6ec;
      color: #15803d;
      border: 1px solid #b7e4c7;
    }
    
    .status-na {
      background: #f3f4f6;
      color: #6b7280;
      border: 1px solid #d1d5db;
    }
    .rating-panel {
    position: absolute;
    bottom: 70px;
    left: 400px;
    z-index: 1000;
    width: 320px;
    max-height: 80vh;
    overflow-y: auto;
    background: #ffffff;
    padding: 18px;
    border-radius: 18px;
    box-shadow: 0 10px 26px rgba(0,0,0,0.18);
    border: 1px solid #ececec;
    }
    .inline-rating-panel {
      background: #fafafa;
      border: 1px solid #ececec;
      border-radius: 14px;
      padding: 14px;
      margin-bottom: 14px;
    }

    #toggle_rating_controls {
      background: white !important;
      color: #2b2b2b !important;
      border: 1px solid #dddddd !important;
      border-radius: 999px !important;
      font-weight: 600 !important;
    }
    
    #toggle_rating_controls:hover {
      background: #f7f7f7 !important;
      color: #2b2b2b !important;
    }
    .filter-chip-wrap {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}

.filter-chip {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  background: #fff5f5;
  color: #b91c1c;
  border: 1px solid #ffd4d4;
  border-radius: 999px;
  padding: 7px 12px;
  font-size: 13px;
  font-weight: 700;
}

.filter-chip-empty {
  color: #777;
  font-size: 13px;
}

.filter-chip-remove {
  background: transparent;
  border: none;
  color: #b91c1c;
  font-weight: 800;
  cursor: pointer;
  padding: 0;
  line-height: 1;
}
  ")), 
    tags$script(HTML(
      "$(document).on('click', '.legend-filter-item', function(e) {
        e.preventDefault();
        var value = $(this).attr('data-value');
        Shiny.setInputValue('map_legend_click', value, {priority: 'event'});
      });"
    ))
  ),
  div(
    class = "yelp-header",
    div(
      class = "yelp-header-left",
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/commons/a/ad/Yelp_Logo.svg",
        class = "yelp-logo"
      ),
      div(
        h1("Yelp Dashboard", class = "yelp-title"),
        p("Philadelphia restaurant intelligence and customer behavior insights", class = "yelp-subtitle")
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.toggle_filters % 2 == 1",
    div(
      class = "filter-panel",
      
      h4("Filters"),
      
      tabsetPanel(
        id = "filter_tabs",
        
        tabPanel(
          "Map",
          selectInput(
            inputId = "color_by",
            label = "Color map points by:",
            choices = c(
              "Cuisine" = "major_cuisine",
              "Price Range" = "RestaurantsPriceRange2"
            ),
            selected = "major_cuisine"
          )
        ),
        
        tabPanel(
          "Restaurant",
          selectizeInput(
            inputId = "cuisine",
            label = "Cuisine:",
            choices = sort(unique(na.omit(map_df$major_cuisine))),
            multiple = TRUE,
            options = list(placeholder = "Select cuisines")
          ),
          
          selectizeInput(
            inputId = "price",
            label = "Price Range:",
            choices = sort(unique(
              na.omit(map_df$RestaurantsPriceRange2[
                !map_df$RestaurantsPriceRange2 %in% c("Unknown", "unknown", "NA", "", "N/A")
              ])
            )),
            multiple = TRUE,
            options = list(placeholder = "Select price ranges")
          ),
          
          selectizeInput(
            inputId = "alcohol",
            label = "Alcohol Type:",
            choices = sort(unique(na.omit(map_df$Alcohol_type))),
            multiple = TRUE,
            options = list(placeholder = "Select alcohol types")
          ),
          selectizeInput(
            inputId = "busyness_filter",
            label = "Live Activity:",
            choices = c("Busy", "Moderate", "Quiet", "No recorded activity"),
            multiple = TRUE,
            options = list(placeholder = "Select activity levels")
          ),
          
          sliderInput(
            inputId = "min_rating",
            label = "Minimum Rating:",
            min = 1,
            max = 5,
            value = 1,
            step = 0.5
          ),
          
          sliderInput(
            inputId = "min_reviews",
            label = "Minimum Review Count:",
            min = 0,
            max = max(map_df$review_count, na.rm = TRUE),
            value = 0,
            step = 10
          ),
          selectizeInput(
            inputId = "noise",
            label = "Noise Level:",
            choices = sort(unique(na.omit(map_df$NoiseLevel))),
            multiple = TRUE,
            options = list(placeholder = "Select noise levels")
          )
        ),
        
        tabPanel(
          "Vibes & Meals",
          selectizeInput(
            inputId = "vibe",
            label = "Vibe:",
            choices = c(
              "trendy", "romantic", "intimate", "touristy",
              "hipster", "divey", "classy", "upscale", "casual"
            ),
            multiple = TRUE,
            options = list(placeholder = "Select vibes")
          ),
          
          selectizeInput(
            inputId = "meal",
            label = "Meal Type:",
            choices = c(
              "Breakfast" = "breakfast",
              "Lunch" = "lunch",
              "Dinner" = "dinner",
              "Late Night" = "latenight",
              "Dessert" = "dessert",
              "Brunch" = "brunch"
            ),
            multiple = TRUE,
            options = list(placeholder = "Select meal types")
          )
        ),
        
        tabPanel(
          "Services",
          selectizeInput(
            inputId = "service",
            label = "Service:",
            choices = c(
              "Takeout" = "RestaurantsTakeOut",
              "Reservations" = "RestaurantsReservations",
              "Catering" = "Caters",
              "Drive-thru" = "DriveThru"
            ),
            multiple = TRUE,
            options = list(placeholder = "Select services")
          ),
          
          selectizeInput(
            inputId = "optional",
            label = "Amenities / Options:",
            choices = c(
              "Alcohol served" = "Alcohol_clean",
              "Happy Hour" = "HappyHour",
              "Good for groups" = "RestaurantsGoodForGroups",
              "Good for kids" = "GoodForKids",
              "Dogs allowed" = "DogsAllowed",
              "Free Wifi" = "WiFi_paid",
              "Coat Check" = "CoatCheck",
              "Outdoor seating" = "OutdoorSeating",
              "Open 24 Hours" = "Open24Hours",
              "Has TV" = "HasTV",
              "Accepts Bitcoin" = "BusinessAcceptsBitcoin",
              "Wheelchair Accessible" = "WheelchairAccessible"
            ),
            multiple = TRUE,
            options = list(placeholder = "Select options")
          ),
          
          selectizeInput(
            inputId = "music",
            label = "Music Type:",
            choices = c(
              "Background Music" = "background_music",
              "Jukebox" = "jukebox",
              "Live" = "live",
              "Video" = "video",
              "Karaoke" = "karaoke"
            ),
            multiple = TRUE,
            options = list(placeholder = "Select music types")
          )
        )
      )
    )
  ),
  
  fluidRow(
    column(
      width = 3,
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Visible Restaurants"),
        div(class = "kpi-value", textOutput("kpi_visible_restaurants", inline = TRUE))
      )
    ),
    column(
      width = 3,
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Average Rating"),
        div(class = "kpi-value", textOutput("kpi_avg_rating", inline = TRUE))
      )
    ),
    column(
      width = 3,
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Average Reviews"),
        div(class = "kpi-value", textOutput("kpi_avg_reviews", inline = TRUE))
      )
    ),
    column(
      width = 3,
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Live Activity"),
        uiOutput("kpi_busy_badge"),
        br(),
        div(
          style = "margin-top:8px; color:#666; font-size:13px;",
          textOutput("kpi_busy_hour_detail", inline = TRUE)
        )
      )
    )
  ),
  br(),
  div(
    class = "dashboard-card",
    style = "margin-bottom: 14px;",
    div(
      style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap;",
      div(
        style = "font-weight:700; color:#444;",
        "Applied Filters"
      )
    ),
    br(),
    uiOutput("active_filters_ui")
  ),
  div(
    class = "main-content",
    
    fluidRow(
      column(
        width = 8,
        div(
          style = "position: relative;",
          leafletOutput("map", height = 650),
          div(
            class = "filter-button",
            style = "display:flex; gap:10px; flex-wrap:wrap;",
            actionButton("toggle_filters", "Filters ⚙️"),
            actionButton("reset_view", "Re-center 📍", class = "btn btn-default")
          )
        )
      ),
      column(
        width = 4,
        div(
          class = "detail-card",
          h4("Restaurant Details"),
          uiOutput("restaurant_info")
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        div(
          class = "dashboard-card",
          h4("Visit Activity by Day and Hour"),
          plotlyOutput("heatmap", height = 420)
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        div(
          class = "analysis-card",
          
          div(
            style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px; flex-wrap:wrap; gap:10px;",
            h4(style = "margin:0;", "Rating Analysis Explorer"),
            div(
              style = "display:flex; gap:8px; flex-wrap:wrap;",
              actionButton("toggle_rating_controls", "Controls 📊", class = "btn btn-default"),
              actionButton("clear_interactions", "Clear Selection", class = "btn btn-default"),
              actionButton("clear_rating_analysis", "Reset Analysis", class = "btn btn-default")
            )
          ),
          
          conditionalPanel(
            condition = "input.toggle_rating_controls % 2 == 1",
            div(
              class = "inline-rating-panel",
              
              radioButtons(
                "analysis_mode",
                "Analysis type:",
                choices = c(
                  "Category comparison" = "category",
                  "Attribute comparison" = "attribute"
                ),
                selected = "category",
                inline = TRUE
              ),
              
              conditionalPanel(
                condition = "input.analysis_mode == 'category'",
                tagList(
                  selectInput(
                    "rating_feature",
                    "Compare ratings by:",
                    choices = c(
                      "Cuisine" = "major_cuisine",
                      "Vibe" = "dominant_vibe",
                      "Price Range" = "RestaurantsPriceRange2",
                      "Alcohol Type" = "Alcohol_type",
                      "Noise Level" = "NoiseLevel",
                      "Alcohol Type" = "alcohol_type"
                    ),
                    selected = "major_cuisine"
                  ),
                  
                  radioButtons(
                    "viz_type",
                    "Chart type:",
                    choices = c("Bar", "Boxplot"),
                    selected = "Bar",
                    inline = TRUE
                  ),
                  selectInput(
                    "rating_star_focus",
                    "Focus on star bucket:",
                    choices = c(
                      "All ratings" = "all",
                      "1 star" = "1",
                      "1.5 stars" = "1.5",
                      "2 stars" = "2",
                      "2.5 stars" = "2.5",
                      "3 stars" = "3",
                      "3.5 stars" = "3.5",
                      "4 stars" = "4",
                      "4.5 stars" = "4.5",
                      "5 stars" = "5"
                    ),
                    selected = "all"
                  ),
                  
                  checkboxInput("compare_mode", "Enable Compare Mode", value = FALSE),
                  
                  conditionalPanel(
                    condition = "input.compare_mode == true",
                    selectizeInput(
                      "compare_groups",
                      "Select up to 2 groups to compare:",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 2,
                        placeholder = "Choose up to 2 groups"
                      )
                    )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.analysis_mode == 'attribute'",
                tagList(
                  tabsetPanel(
                    id = "attribute_tabs",
                    
                    tabPanel(
                      "Vibes",
                      selectizeInput(
                        "boxplot_vibes",
                        "Choose vibes:",
                        choices = c(
                          "Trendy" = "trendy",
                          "Romantic" = "romantic",
                          "Intimate" = "intimate",
                          "Touristy" = "touristy",
                          "Hipster" = "hipster",
                          "Divey" = "divey",
                          "Classy" = "classy",
                          "Upscale" = "upscale",
                          "Casual" = "casual"
                        ),
                        multiple = TRUE,
                        options = list(placeholder = "Select vibe attributes")
                      )
                    ),
                    
                    tabPanel(
                      "Meals",
                      selectizeInput(
                        "boxplot_meals",
                        "Choose meal types:",
                        choices = c(
                          "Breakfast" = "breakfast",
                          "Lunch" = "lunch",
                          "Dinner" = "dinner",
                          "Late Night" = "latenight",
                          "Dessert" = "dessert",
                          "Brunch" = "brunch"
                        ),
                        multiple = TRUE,
                        options = list(placeholder = "Select meal attributes")
                      )
                    ),
                    
                    tabPanel(
                      "Services",
                      selectizeInput(
                        "boxplot_services",
                        "Choose services:",
                        choices = c(
                          "Takeout" = "RestaurantsTakeOut",
                          "Reservations" = "RestaurantsReservations",
                          "Catering" = "Caters",
                          "Drive-thru" = "DriveThru"
                        ),
                        multiple = TRUE,
                        options = list(placeholder = "Select service attributes")
                      )
                    ),
                    
                    tabPanel(
                      "Amenities",
                      selectizeInput(
                        "boxplot_amenities",
                        "Choose amenities/options:",
                        choices = c(
                          "Happy Hour" = "HappyHour",
                          "Good for groups" = "RestaurantsGoodForGroups",
                          "Good for kids" = "GoodForKids",
                          "Dogs allowed" = "DogsAllowed",
                          "Coat Check" = "CoatCheck",
                          "Outdoor seating" = "OutdoorSeating",
                          "Open 24 Hours" = "Open24Hours",
                          "Has TV" = "HasTV",
                          "Wheelchair Accessible" = "WheelchairAccessible"
                        ),
                        multiple = TRUE,
                        options = list(placeholder = "Select amenity attributes")
                      )
                    ),
                    
                    tabPanel(
                      "Music",
                      selectizeInput(
                        "boxplot_music",
                        "Choose music types:",
                        choices = c(
                          "Background Music" = "background_music",
                          "Jukebox" = "jukebox",
                          "Live Music" = "live",
                          "Video" = "video",
                          "Karaoke" = "karaoke"
                        ),
                        multiple = TRUE,
                        options = list(placeholder = "Select music attributes")
                      )
                    )
                  )
                )
              )
            )
          ),
          br(),
          div(
            style = "background:#fafafa; border:1px solid #ececec; border-radius:14px; padding:12px; margin-bottom:12px;",
            strong("Analysis Filters Applied"),
            br(),
            uiOutput("analysis_filters_ui")
          ),
          
          plotlyOutput("rating_analysis_plot", height = 520),
          br(),
          div(
            style = "background:#fafafa; border:1px solid #ececec; border-radius:14px; padding:14px;",
            strong("Live Insight"),
            br(),
            textOutput("insight_text")
          )
        )
      )
    )
  )
)
  

  

# ---------------------------------------------------------
# STATIC SNAPSHOT PLOTS FOR DATA STORY
# ---------------------------------------------------------

# 1. Cuisine distribution plot
cuisine_plot <- map_df %>%
  count(major_cuisine) %>%
  ggplot(aes(x = fct_reorder(major_cuisine, n), y = n, fill = major_cuisine)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = cuisine_palette_fixed) +
  labs(x = "Cuisine", y = "Count", title = "Cuisine Distribution") +
  theme_minimal(base_size = 14)

# 2. Rating distribution plot
rating_dist_plot <- map_df %>%
  filter(!is.na(stars)) %>%
  ggplot(aes(x = stars)) +
  geom_histogram(binwidth = 0.5, fill = "#d32323", color = "white") +
  labs(x = "Rating", y = "Count", title = "Rating Distribution") +
  theme_minimal(base_size = 14)

# 3. Price range plot
price_plot <- map_df %>%
  filter(
    !is.na(RestaurantsPriceRange2),
    RestaurantsPriceRange2 != "Unknown",
    !is.na(stars)
  ) %>%
  ggplot(aes(
    x = RestaurantsPriceRange2,
    y = stars,
    fill = RestaurantsPriceRange2
  )) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.3) +
  scale_fill_manual(values = price_palette_fixed) +
  labs(
    x = "Price Range",
    y = "Star Rating",
    title = "Distribution of Ratings by Price Range",
    fill = "Price Range"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 12
    )
  )


# 4. Noise level plot
noise_plot <- map_df %>%
  filter(!is.na(NoiseLevel)) %>%
  mutate(
    NoiseLevel = factor(
      NoiseLevel,
      levels = c("Moderate", "Loud", "Quiet", "Very Loud")
    )
  ) %>%
  ggplot(aes(x = NoiseLevel, y = stars, fill = NoiseLevel)) +
  geom_boxplot() +
  scale_fill_manual(values = noise_palette_fixed) +
  labs(
    x = "Noise Level",
    y = "Rating",
    title = "Rating by Noise Level"
  ) +
  theme_minimal(base_size = 14)

# 5. Heatmap plot
heatmap_plot <- checkins %>%
  filter(!is.na(day), !is.na(hour)) %>%
  mutate(
    day = factor(day, levels = c(
      "Monday", "Tuesday", "Wednesday",
      "Thursday", "Friday", "Saturday", "Sunday"
    ))
  ) %>%
  count(day, hour) %>%
  ggplot(aes(x = hour, y = day, fill = n)) +
  geom_tile() +
  scale_fill_gradient(
    name = "Check-ins",
    low = "#fee0d2",
    high = "#de2d26"
  ) +
  labs(x = "Hour", y = "Day", title = "Check-in Heatmap") +
  theme_minimal(base_size = 14)

# ---------------------------------------------------------
# EXPORT PNG SNAPSHOTS INTO /www/
# ---------------------------------------------------------
dir.create("www", showWarnings = FALSE)
ggsave("www/snapshot_cuisine.png", cuisine_plot, width = 8, height = 5, dpi = 150)
ggsave("www/snapshot_ratings.png", rating_dist_plot, width = 8, height = 5, dpi = 150)
ggsave("www/snapshot_price.png", price_plot, width = 8, height = 5, dpi = 150)
ggsave("www/snapshot_noise.png", noise_plot, width = 8, height = 5, dpi = 150)
ggsave("www/snapshot_heatmap.png", heatmap_plot, width = 8, height = 5, dpi = 150)



# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------
server <- function(input, output, session) {
  observe({
    showModal(modalDialog(
      title = NULL,
      size = "l", 
      easyClose = TRUE, 
      fade = TRUE,
      
      div(style = "padding: 10px 25px; color: #333;",
          div(style = "padding: 10px 25px; color: #333;",
              
              # Title
              div(style = "font-family: 'Times New Roman', serif; font-size: 38px; 
              font-weight: bold; text-align: center; border-bottom: 2px solid #000; 
              margin-bottom: 5px;",
                  "The Philadelphia Palate: A Data Story"),
              
              # Byline
              div(style = "text-align: center; font-family: 'Arial'; text-transform: uppercase; 
              font-size: 11px; letter-spacing: 1px; color: #666; margin-bottom: 25px;",
                  "By Group 7 (Mina Ahn, Alex Wang, Lyle Mora)"),
              
              # Opening paragraph
              p(style = "font-family: 'Georgia', serif; font-size: 17px; line-height: 1.8;",
                span(style = "float: left; font-size: 60px; line-height: 50px; padding-top: 4px; 
                  padding-right: 8px; font-family: 'Times New Roman'; font-weight: bold; 
                  color: #000;", "W"),
                "hen you step into Philadelphia’s food scene, the choices feel endless. 
     From corner cheesesteak shops to sleek Center City dining rooms, the city’s 
     culinary identity is as layered as its history. With more than 10GB of Yelp data 
     at our disposal, we set out to answer a deceptively simple question: 
     what actually makes a restaurant successful here?"),
              
              # Snapshot 1 — Cuisine distribution
              div(style = "text-align:center; margin: 25px 0;",
                  tags$img(src = "snapshot_cuisine.png", style = "max-width: 90%; border-radius: 12px;"),
                  div(style = "font-size: 12px; color: #777; margin-top: 6px;",
                      "Snapshot: Distribution of major cuisines across Philadelphia")),
              
              # Section 1
              div(style = "font-family: Arial, sans-serif; font-weight: bold; font-size: 18px; 
              margin-top: 25px; border-top: 1px solid #eee; padding-top: 15px;",
                  "Where the City Eats"),
              
              p(style = "font-family: 'Georgia', serif; font-size: 16px; line-height: 1.7;",
                "Philadelphia’s restaurant landscape is dominated by American, Italian, and 
     Chinese cuisines — but the city’s highest-rated spots aren’t always in the 
     categories you’d expect. When we mapped cuisine types across the city, 
     we found that smaller categories like Japanese and Middle Eastern cuisine 
     consistently punch above their weight in average rating."),
              
              # Snapshot 2 — Rating distribution
              div(style = "text-align:center; margin: 25px 0;",
                  tags$img(src = "snapshot_ratings.png", style = "max-width: 90%; border-radius: 12px;"),
                  div(style = "font-size: 12px; color: #777; margin-top: 6px;",
                      "Snapshot: Rating distribution across all restaurants")),
              
              # Section 2
              div(style = "font-family: Arial, sans-serif; font-weight: bold; font-size: 18px; 
              margin-top: 25px;",
                  "Price vs. Pleasure"),
              
              p(style = "font-family: 'Georgia', serif; font-size: 16px; line-height: 1.7;",
                "When we examine how price relates to satisfaction, the pattern is straightforward: higher prices generally correspond to higher ratings. Restaurants in the Over $61 per person tier earn the strongest reviews, with the highest median and upper‑quartile scores of any group. Mid‑range spots ($11–$30) remain reliable crowd‑pleasers, but the data shows that when diners pay more, they typically receive a higher‑quality experience."),
              
              # Snapshot 3 — Price range bar chart
              div(style = "text-align:center; margin: 25px 0;",
                  tags$img(src = "snapshot_price.png", style = "max-width: 90%; border-radius: 12px;"),
                  div(style = "font-size: 12px; color: #777; margin-top: 6px;",
                      "Snapshot: Average rating by price range")),
              
              # Section 3
              div(style = "font-family: Arial, sans-serif; font-weight: bold; font-size: 18px; 
              margin-top: 25px;",
                  "The Quiet Power of Atmosphere"),
              
              p(style = "font-family: 'Georgia', serif; font-size: 16px; line-height: 1.7;",
                "Beyond food and price, atmosphere quietly shapes customer satisfaction. 
     Restaurants labeled as 'Quiet' or 'Moderate' in noise level earn higher 
     ratings than their louder counterparts. Amenities matter too: outdoor 
     seating, wheelchair accessibility, and good-for-groups tags all correlate 
     with higher customer satisfaction."),
              
              # Snapshot 4 — Noise level boxplot
              div(style = "text-align:center; margin: 25px 0;",
                  tags$img(src = "snapshot_noise.png", style = "max-width: 90%; border-radius: 12px;"),
                  div(style = "font-size: 12px; color: #777; margin-top: 6px;",
                      "Snapshot: Rating distribution by noise level")),
              
              # Section 4
              div(style = "font-family: Arial, sans-serif; font-weight: bold; font-size: 18px; 
              margin-top: 25px;",
                  "Mapping the Pulse of the City"),
              
              p(style = "font-family: 'Georgia', serif; font-size: 16px; line-height: 1.7;",
                "Using Yelp check-in data, we visualized when and where Philadelphians go out. 
     The city’s heartbeat peaks on Friday and Saturday evenings, with pockets of 
     weekday lunch activity in Center City. These patterns help explain why some 
     restaurants thrive: success is as much about timing and location as it is 
     about cuisine."),
              
              # Snapshot 5 — Heatmap
              div(style = "text-align:center; margin: 25px 0;",
                  tags$img(src = "snapshot_heatmap.png", style = "max-width: 90%; border-radius: 12px;"),
                  div(style = "font-size: 12px; color: #777; margin-top: 6px;",
                      "Snapshot: Day/hour check-in heatmap")),
              
              # Conclusion
              div(style = "font-family: Arial, sans-serif; font-weight: bold; font-size: 18px; 
              margin-top: 25px;",
                  "What This Means for Philadelphia Diners"),
              
              p(style = "font-family: 'Georgia', serif; font-size: 16px; line-height: 1.7;",
                "Taken together, the data paints a clear picture: Philadelphia’s best dining 
     experiences aren’t defined by price or prestige. They’re shaped by value, 
     atmosphere, and the rhythms of the city itself. Whether you’re a local or a 
     visitor, the dashboard ahead lets you explore these patterns — and maybe 
     discover your next favorite spot."),
              
              # Button
              div(style = "text-align: center; margin-top: 35px; padding-top: 20px;",
                  modalButton("Enter Dashboard", icon = icon("utensils")))
          )
          

      ),
      footer = NULL 
    ))
  })
  
  selected_rating_group <- reactiveVal(NULL)
  selected_rating_trace <- reactiveVal(NULL)
  highlight_mode <- reactiveVal(FALSE)
  snapshot_rect <- reactiveVal(NULL)
  excluded_map_groups <- reactiveVal(character(0))
  pretty_analysis_label <- function(x) {
    dplyr::case_when(
      x == "major_cuisine" ~ "Cuisine",
      x == "dominant_vibe" ~ "Vibe",
      x == "alcohol" ~ "Price Range",
      x == "NoiseLevel" ~ "Noise Level",
      x == "alcohol_type" ~ "Alcohol Type",
      x == "category" ~ "Category comparison",
      x == "attribute" ~ "Attribute comparison",
      x == "Bar" ~ "Bar",
      x == "Boxplot" ~ "Boxplot",
      TRUE ~ gsub("_", " ", x)
    )
  }
  
  pretty_attribute_label <- function(x) {
    label_map <- c(
      trendy = "Trendy",
      romantic = "Romantic",
      intimate = "Intimate",
      touristy = "Touristy",
      hipster = "Hipster",
      divey = "Divey",
      classy = "Classy",
      upscale = "Upscale",
      casual = "Casual",
      breakfast = "Breakfast",
      lunch = "Lunch",
      dinner = "Dinner",
      latenight = "Late Night",
      dessert = "Dessert",
      brunch = "Brunch",
      RestaurantsTakeOut = "Takeout",
      RestaurantsReservations = "Reservations",
      Caters = "Catering",
      DriveThru = "Drive-thru",
      HappyHour = "Happy Hour",
      RestaurantsGoodForGroups = "Good for groups",
      GoodForKids = "Good for kids",
      DogsAllowed = "Dogs allowed",
      CoatCheck = "Coat Check",
      OutdoorSeating = "Outdoor seating",
      Open24Hours = "Open 24 Hours",
      HasTV = "Has TV",
      WheelchairAccessible = "Wheelchair Accessible",
      background_music = "Background Music",
      jukebox = "Jukebox",
      live = "Live Music",
      video = "Video",
      karaoke = "Karaoke"
    )
    
    ifelse(x %in% names(label_map), unname(label_map[x]), gsub("_", " ", x))
  }
  make_filter_chip <- function(id, label) {
    tags$div(
      class = "filter-chip",
      span(label),
      actionButton(
        inputId = id,
        label = "×",
        class = "filter-chip-remove"
      )
    )
  }
  busyness_cache <- reactivePoll(
    intervalMillis = 60000,
    session = session,
    checkFunc = function() {
      paste(as.character(weekdays(Sys.time())), format(Sys.time(), "%H"))
    },
    valueFunc = function() {
      current_day <- as.character(weekdays(Sys.time()))
      current_hour <- as.integer(format(Sys.time(), "%H"))
      
      checkins_by_business_day_hour %>%
        filter(day == current_day, hour == current_hour) %>%
        transmute(
          business_id,
          busyness = case_when(
            n_checkins >= 8 ~ "Busy",
            n_checkins >= 3 ~ "Moderate",
            n_checkins > 0 ~ "Quiet",
            TRUE ~ "No recorded activity"
          )
        )
    }
  )
  
  output$kpi_busy_badge <- renderUI({
    df <- snapshot_data()
    
    if (nrow(df) == 0) {
      return(div(class = "status-badge status-na", "No data"))
    }
    
    current_day <- as.character(weekdays(Sys.time()))
    current_hour <- as.integer(format(Sys.time(), "%H"))
    
    current_checkins <- checkins %>%
      mutate(day = as.character(day)) %>%
      filter(
        business_id %in% df$business_id,
        day == current_day,
        hour == current_hour
      )
    
    n_checkins <- nrow(current_checkins)
    
    badge_class <- dplyr::case_when(
      n_checkins >= 40 ~ "status-badge status-busy",
      n_checkins >= 15 ~ "status-badge status-moderate",
      n_checkins > 0 ~ "status-badge status-quiet",
      TRUE ~ "status-badge status-na"
    )
    
    badge_label <- dplyr::case_when(
      n_checkins >= 40 ~ "Busy",
      n_checkins >= 15 ~ "Moderate",
      n_checkins > 0 ~ "Quiet",
      TRUE ~ "No recorded activity"
    )
    
    div(class = badge_class, badge_label)
  })
  output$active_filters_ui <- renderUI({
    chips <- list()
    
    # ---------------- regular map/sidebar filters ----------------
    if (length(input$cuisine) > 0) {
      for (val in input$cuisine) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_cuisine_", make.names(val)),
          paste("Cuisine:", val)
        )))
      }
    }
    
    if (length(input$price) > 0) {
      for (val in input$price) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_price_", make.names(val)),
          paste("Price:", val)
        )))
      }
    }
    
    if (length(input$noise) > 0) {
      for (val in input$noise) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_noise_", make.names(val)),
          paste("Noise:", val)
        )))
      }
    }
    
    if (length(input$alcohol) > 0) {
      for (val in input$alcohol) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_alcohol_", make.names(val)),
          paste("Alcohol:", val)
        )))
      }
    }
    
    if (length(input$vibe) > 0) {
      for (val in input$vibe) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_vibe_", make.names(val)),
          paste("Vibe:", pretty_attribute_label(val))
        )))
      }
    }
    
    if (length(input$meal) > 0) {
      for (val in input$meal) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_meal_", make.names(val)),
          paste("Meal:", pretty_attribute_label(val))
        )))
      }
    }
    
    if (length(input$service) > 0) {
      for (val in input$service) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_service_", make.names(val)),
          paste("Service:", pretty_attribute_label(val))
        )))
      }
    }
    
    if (length(input$optional) > 0) {
      for (val in input$optional) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_optional_", make.names(val)),
          paste("Option:", pretty_attribute_label(val))
        )))
      }
    }
    
    if (length(input$music) > 0) {
      for (val in input$music) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_music_", make.names(val)),
          paste("Music:", pretty_attribute_label(val))
        )))
      }
    }
    
    if (length(input$busyness_filter) > 0) {
      for (val in input$busyness_filter) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_busyness_", make.names(val)),
          paste("Activity:", val)
        )))
      }
    }
    
    if (!is.null(input$min_rating) && input$min_rating > 1) {
      chips <- append(chips, list(
        tags$div(class = "filter-chip", paste("Min rating:", input$min_rating))
      ))
    }
    
    if (!is.null(input$min_reviews) && input$min_reviews > 0) {
      chips <- append(chips, list(
        tags$div(class = "filter-chip", paste("Min reviews:", input$min_reviews))
      ))
    }
    
    if (length(chips) == 0) {
      return(div(class = "filter-chip-empty", "No filters applied"))
    }
    
    div(class = "filter-chip-wrap", chips)
  })
  output$analysis_filters_ui <- renderUI({
    chips <- list()
    
    if (!is.null(input$analysis_mode)) {
      chips <- append(chips, list(make_filter_chip(
        "remove_analysis_mode",
        paste("Analysis:", pretty_analysis_label(input$analysis_mode))
      )))
    }
    
    if (!is.null(input$rating_feature) && input$analysis_mode == "category") {
      chips <- append(chips, list(make_filter_chip(
        "remove_rating_feature",
        paste("Compare by:", pretty_analysis_label(input$rating_feature))
      )))
    }
    
    if (!is.null(input$viz_type) && input$analysis_mode == "category") {
      chips <- append(chips, list(make_filter_chip(
        "remove_viz_type",
        paste("Chart:", input$viz_type)
      )))
    }
    
    if (!is.null(input$rating_star_focus) &&
        input$analysis_mode == "category" &&
        input$rating_star_focus != "all") {
      chips <- append(chips, list(make_filter_chip(
        "remove_star_focus",
        paste("Stars:", input$rating_star_focus)
      )))
    }
    
    if (isTRUE(input$compare_mode)) {
      chips <- append(chips, list(make_filter_chip(
        "remove_compare_mode",
        "Compare Mode ON"
      )))
    }
    
    if (length(input$compare_groups) > 0) {
      for (val in input$compare_groups) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_compare_group_", make.names(val)),
          paste("Group:", val)
        )))
      }
    }
    
    if (input$analysis_mode == "attribute") {
      attrs <- unique(c(
        input$boxplot_vibes,
        input$boxplot_meals,
        input$boxplot_services,
        input$boxplot_amenities,
        input$boxplot_music
      ))
      
      for (val in attrs) {
        chips <- append(chips, list(make_filter_chip(
          paste0("remove_attr_", make.names(val)),
          paste("Attribute:", pretty_attribute_label(val))
        )))
      }
    }
    
    if (length(chips) == 0) {
      return(div(class = "filter-chip-empty", "No analysis filters applied"))
    }
    
    div(class = "filter-chip-wrap", chips)
  })
  observeEvent(input$remove_analysis_mode, {
    updateRadioButtons(session, "analysis_mode", selected = "category")
  }, ignoreInit = TRUE)
  
  observeEvent(input$remove_rating_feature, {
    updateSelectInput(session, "rating_feature", selected = "major_cuisine")
  }, ignoreInit = TRUE)
  
  observeEvent(input$remove_viz_type, {
    updateRadioButtons(session, "viz_type", selected = "Bar")
  }, ignoreInit = TRUE)
  
  observeEvent(input$remove_star_focus, {
    updateSelectInput(session, "rating_star_focus", selected = "all")
  }, ignoreInit = TRUE)
  
  observeEvent(input$remove_compare_mode, {
    updateCheckboxInput(session, "compare_mode", value = FALSE)
    updateSelectizeInput(session, "compare_groups", selected = character(0))
  }, ignoreInit = TRUE)
  
  observe({
    lapply(input$compare_groups, function(val) {
      observeEvent(input[[paste0("remove_compare_group_", make.names(val))]], {
        updateSelectizeInput(
          session, "compare_groups",
          selected = setdiff(input$compare_groups, val)
        )
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    analysis_attrs <- unique(c(
      input$boxplot_vibes,
      input$boxplot_meals,
      input$boxplot_services,
      input$boxplot_amenities,
      input$boxplot_music
    ))
    
    lapply(analysis_attrs, function(val) {
      observeEvent(input[[paste0("remove_attr_", make.names(val))]],  {
        
        if (val %in% input$boxplot_vibes) {
          updateSelectizeInput(session, "boxplot_vibes",
                               selected = setdiff(input$boxplot_vibes, val))
        }
        if (val %in% input$boxplot_meals) {
          updateSelectizeInput(session, "boxplot_meals",
                               selected = setdiff(input$boxplot_meals, val))
        }
        if (val %in% input$boxplot_services) {
          updateSelectizeInput(session, "boxplot_services",
                               selected = setdiff(input$boxplot_services, val))
        }
        if (val %in% input$boxplot_amenities) {
          updateSelectizeInput(session, "boxplot_amenities",
                               selected = setdiff(input$boxplot_amenities, val))
        }
        if (val %in% input$boxplot_music) {
          updateSelectizeInput(session, "boxplot_music",
                               selected = setdiff(input$boxplot_music, val))
        }
        
      }, ignoreInit = TRUE)
    })
  })
  observeEvent(input$map_legend_click, {
    g <- input$map_legend_click
    req(g)
    
    current_excluded <- excluded_map_groups()
    
    if (g %in% current_excluded) {
      excluded_map_groups(setdiff(current_excluded, g))
    } else {
      excluded_map_groups(c(current_excluded, g))
    }
  })
  output$kpi_busy_hour_detail <- renderText({
    df <- snapshot_data()
    
    if (nrow(df) == 0) return("No visible restaurants")
    
    current_day <- as.character(weekdays(Sys.time()))
    current_hour <- as.integer(format(Sys.time(), "%H"))
    next_hour <- (current_hour + 1) %% 24
    
    start_label <- format(strptime(as.character(current_hour), format = "%H"), "%I %p")
    end_label   <- format(strptime(as.character(next_hour), format = "%H"), "%I %p")
    
    paste0(current_day, " ", start_label, "–", end_label)
  })
  
  output$kpi_visible_restaurants <- renderText({
    df <- snapshot_data()
    scales::comma(nrow(df))
  })
  
  
  output$kpi_avg_rating <- renderText({
    df <- snapshot_data()
    
    if (nrow(df) == 0 || all(is.na(df$stars))) {
      return("N/A")
    }
    
    round(mean(df$stars, na.rm = TRUE), 2)
  })
  
  output$kpi_avg_reviews <- renderText({
    df <- snapshot_data()
    
    if (nrow(df) == 0 || all(is.na(df$review_count))) {
      return("N/A")
    }
    
    round(mean(df$review_count, na.rm = TRUE), 0)
  })
  
  output$kpi_busy_hour <- renderText({
    df <- snapshot_data()
    
    if (nrow(df) == 0) {
      return("N/A")
    }
    
    counts <- checkins %>%
      filter(business_id %in% df$business_id) %>%
      filter(!is.na(day), !is.na(hour)) %>%
      count(day, hour, sort = TRUE)
    
    if (nrow(counts) == 0) {
      return("N/A")
    }
    
    top_day <- counts$day[1]
    top_hour <- counts$hour[1]
    next_hour <- (top_hour + 1) %% 24
    
    start_label <- format(strptime(top_hour, format = "%H"), "%I %p")
    end_label   <- format(strptime(next_hour, format = "%H"), "%I %p")
    
    paste0(top_day, ", ", start_label, "–", end_label)
  })
  
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    req(feature)
    
    if (feature$geometry$type == "Polygon") {
      coords <- feature$geometry$coordinates[[1]]
      
      lngs <- sapply(coords, function(x) x[[1]])
      lats <- sapply(coords, function(x) x[[2]])
      
      snapshot_rect(list(
        west = min(lngs, na.rm = TRUE),
        east = max(lngs, na.rm = TRUE),
        south = min(lats, na.rm = TRUE),
        north = max(lats, na.rm = TRUE)
      ))
    }
  })
  observeEvent(input$map_draw_edited_features, {
    feats <- input$map_draw_edited_features$features
    req(length(feats) > 0)
    
    feature <- feats[[1]]
    
    if (feature$geometry$type == "Polygon") {
      coords <- feature$geometry$coordinates[[1]]
      
      lngs <- sapply(coords, function(x) x[[1]])
      lats <- sapply(coords, function(x) x[[2]])
      
      snapshot_rect(list(
        west = min(lngs, na.rm = TRUE),
        east = max(lngs, na.rm = TRUE),
        south = min(lats, na.rm = TRUE),
        north = max(lats, na.rm = TRUE)
      ))
    }
  })
  observeEvent(input$clear_rating_analysis, {
    selected_rating_group(NULL)
    selected_rating_trace(NULL)
    highlight_mode(FALSE)
    
    updateRadioButtons(session, "analysis_mode", selected = "category")
    updateSelectInput(session, "rating_feature", selected = "major_cuisine")
    updateRadioButtons(session, "viz_type", selected = "Bar")
    updateCheckboxInput(session, "compare_mode", value = FALSE)
    updateSelectizeInput(session, "compare_groups", selected = character(0))
    
    updateSelectizeInput(session, "boxplot_vibes", selected = character(0))
    updateSelectizeInput(session, "boxplot_meals", selected = character(0))
    updateSelectizeInput(session, "boxplot_services", selected = character(0))
    updateSelectizeInput(session, "boxplot_amenities", selected = character(0))
    updateSelectizeInput(session, "boxplot_music", selected = character(0))
  })
  observeEvent(input$map_draw_deleted_features, {
    snapshot_rect(NULL)
  })
  # add a simple dominant vibe column for analysis plot
  analysis_df <- reactive({
    df <- map_df
    
    vibe_cols <- intersect(
      c("trendy", "romantic", "intimate", "touristy", "hipster",
        "divey", "classy", "upscale", "casual"),
      names(df)
    )
    
    df$dominant_vibe <- NA_character_
    
    if (length(vibe_cols) > 0) {
      for (v in vibe_cols) {
        idx <- is.na(df$dominant_vibe) & !is.na(df[[v]]) & df[[v]] == TRUE
        df$dominant_vibe[idx] <- v
      }
    }
    
    df
  })
  
  
  # sidebar filters only
  filtered_data <- reactive({
    df <- analysis_df()
    if (!is.null(input$min_rating)) df <- df %>% filter(stars >= input$min_rating)
    if (!is.null(input$min_reviews)) df <- df %>% filter(review_count >= input$min_reviews)
    
    if (length(input$cuisine) > 0) {
      df <- df %>% filter(major_cuisine %in% input$cuisine)
    }
    
    if (length(input$price) > 0) {
      df <- df %>% filter(RestaurantsPriceRange2 %in% input$price)
    }
    
    if (length(input$vibe) > 0) {
      valid_vibe_cols <- intersect(input$vibe, names(df))
      if (length(valid_vibe_cols) > 0) {
        df <- df %>% filter(if_any(all_of(valid_vibe_cols), ~ .))
      }
    }
    
    if (length(input$noise) > 0) {
      df <- df %>% filter(NoiseLevel %in% input$noise)
    }
    
    if (length(input$alcohol) > 0) {
      df <- df %>% filter(Alcohol_type %in% input$alcohol)
    }
    
    if (length(input$service) > 0) {
      valid_service_cols <- intersect(input$service, names(df))
      if (length(valid_service_cols) > 0) {
        df <- df %>% filter(if_any(all_of(valid_service_cols), ~ .))
      }
    }
    
    if (length(input$meal) > 0) {
      valid_meal_cols <- intersect(input$meal, names(df))
      if (length(valid_meal_cols) > 0) {
        df <- df %>% filter(if_any(all_of(valid_meal_cols), ~ .))
      }
    }
    
    if (length(input$optional) > 0) {
      valid_optional_cols <- intersect(input$optional, names(df))
      if (length(valid_optional_cols) > 0) {
        df <- df %>% filter(if_any(all_of(valid_optional_cols), ~ .))
      }
    }
    
    if (length(input$music) > 0) {
      valid_music_cols <- intersect(input$music, names(df))
      if (length(valid_music_cols) > 0) {
        df <- df %>% filter(if_any(all_of(valid_music_cols), ~ .))
      }
    }
    
    df
  })
  snapshot_data <- reactive({
    df <- filtered_data()
    rect <- snapshot_rect()
    
    # if no rectangle → return full filtered data (no jumping)
    if (is.null(rect)) {
      return(df)
    }
    
    df %>%
      filter(
        !is.na(latitude), !is.na(longitude),
        latitude >= rect$south,
        latitude <= rect$north,
        longitude >= rect$west,
        longitude <= rect$east
      )
  })
  
  # apply current visible map bounds on top of sidebar filters
  visible_data <- reactive({
    df <- filtered_data()
    rect <- snapshot_rect()
    
    # if user drew a region → use it
    if (!is.null(rect)) {
      df <- df %>%
        filter(
          !is.na(latitude), !is.na(longitude),
          latitude >= rect$south,
          latitude <= rect$north,
          longitude >= rect$west,
          longitude <= rect$east
        )
    }
    
    df <- df %>%
      left_join(busyness_cache(), by = "business_id") %>%
      mutate(busyness = ifelse(is.na(busyness), "No recorded activity", busyness))
    if (length(input$busyness_filter) > 0) {
      df <- df %>% filter(busyness %in% input$busyness_filter)
    }
    df
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = -75.1652, lat = 39.9526, zoom = 11) %>%
      addDrawToolbar(
        targetGroup = "snapshot",
        rectangleOptions = drawRectangleOptions(),
        polygonOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polylineOptions = FALSE,
        editOptions = editToolbarOptions()
      )
  })
  observe({
    lapply(input$cuisine, function(val) {
      observeEvent(input[[paste0("remove_cuisine_", make.names(val))]], {
        updateSelectizeInput(
          session, "cuisine",
          selected = setdiff(input$cuisine, val)
        )
      }, ignoreInit = TRUE)
    })
    
    lapply(input$price, function(val) {
      observeEvent(input[[paste0("remove_price_", make.names(val))]], {
        updateSelectizeInput(
          session, "price",
          selected = setdiff(input$price, val)
        )
      }, ignoreInit = TRUE)
    })
    
    lapply(input$noise, function(val) {
      observeEvent(input[[paste0("remove_noise_", make.names(val))]], {
        updateSelectizeInput(
          session, "noise",
          selected = setdiff(input$noise, val)
        )
      }, ignoreInit = TRUE)
    })
    
    lapply(input$alcohol, function(val) {
      observeEvent(input[[paste0("remove_alcohol_", make.names(val))]], {
        updateSelectizeInput(
          session, "alcohol",
          selected = setdiff(input$alcohol, val)
        )
      }, ignoreInit = TRUE)
    })
    
    lapply(input$vibe, function(val) {
      observeEvent(input[[paste0("remove_vibe_", make.names(val))]], {
        updateSelectizeInput(
          session, "vibe",
          selected = setdiff(input$vibe, val)
        )
      }, ignoreInit = TRUE)
    })
    
    lapply(input$meal, function(val) {
      observeEvent(input[[paste0("remove_meal_", make.names(val))]], {
        updateSelectizeInput(
          session, "meal",
          selected = setdiff(input$meal, val)
        )
      }, ignoreInit = TRUE)
    })
    
    lapply(input$service, function(val) {
      observeEvent(input[[paste0("remove_service_", make.names(val))]], {
        updateSelectizeInput(
          session, "service",
          selected = setdiff(input$service, val)
        )
      }, ignoreInit = TRUE)
    })
    
    lapply(input$optional, function(val) {
      observeEvent(input[[paste0("remove_optional_", make.names(val))]], {
        updateSelectizeInput(
          session, "optional",
          selected = setdiff(input$optional, val)
        )
      }, ignoreInit = TRUE)
    })
    
    lapply(input$music, function(val) {
      observeEvent(input[[paste0("remove_music_", make.names(val))]], {
        updateSelectizeInput(
          session, "music",
          selected = setdiff(input$music, val)
        )
      }, ignoreInit = TRUE)
    })
    lapply(input$busyness_filter, function(val) {
      observeEvent(input[[paste0("remove_busyness_", make.names(val))]], {
        updateSelectizeInput(
          session, "busyness_filter",
          selected = setdiff(input$busyness_filter, val)
        )
      }, ignoreInit = TRUE)
    })
  })
  observeEvent(input$clear_all_filters, {
    updateSelectizeInput(session, "cuisine", selected = character(0))
    updateSelectizeInput(session, "price", selected = character(0))
    updateSelectizeInput(session, "alcohol", selected = character(0))
    updateSelectizeInput(session, "noise", selected = character(0))
    updateSelectizeInput(session, "vibe", selected = character(0))
    updateSelectizeInput(session, "meal", selected = character(0))
    updateSelectizeInput(session, "service", selected = character(0))
    updateSelectizeInput(session, "optional", selected = character(0))
    updateSelectizeInput(session, "music", selected = character(0))
    updateSelectizeInput(session, "busyness_filter", selected = character(0))
    
    updateSliderInput(session, "min_rating", value = 1)
    updateSliderInput(session, "min_reviews", value = 0)
    
    excluded_map_groups(character(0))
    snapshot_rect(NULL)
  })
  
  observe({
    df <- visible_data()
    
    leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      clearControls()
    
    req(nrow(df) > 0)
    
    color_var <- input$color_by
    df$color_group <- df[[color_var]]
    excluded <- excluded_map_groups()
    
    if (length(excluded) > 0) {
      df <- df %>%
        filter(!(as.character(color_group) %in% excluded))
    }
    df$marker_radius <- if ("review_count" %in% names(df)) {
      scales::rescale(
        df$review_count,
        to = c(5, 11),
        from = range(df$review_count, na.rm = TRUE)
      )
    } else {
      rep(6, nrow(df))
    }
    df$marker_radius[is.na(df$marker_radius)] <- 6
    
    if (color_var %in% c("major_cuisine", "RestaurantsPriceRange2", "NoiseLevel", "Alcohol_type")) {
      color_values <- get_fixed_palette(color_var)
      
      fill_pal <- colorFactor(
        palette = unname(color_values),
        domain = names(color_values),
        na.color = "#CCCCCC"
      )
      
    } else if (color_var == "busyness") {
      fill_pal <- colorFactor(
        palette = c(
          "Busy" = "#d32323",
          "Moderate" = "#f59e0b",
          "Quiet" = "#16a34a",
          "No recorded activity" = "#9ca3af"
        ),
        domain = c("Busy", "Moderate", "Quiet", "No recorded activity"),
        na.color = "#CCCCCC"
      )
    }
    
    busyness_pal <- colorFactor(
      palette = c(
        "Busy" = "#d32323",
        "Moderate" = "#f59e0b",
        "Quiet" = "#16a34a",
        "No recorded activity" = "#9ca3af"
      ),
      domain = c("Busy", "Moderate", "Quiet", "No recorded activity"),
      na.color = "#CCCCCC"
    )
    
    proxy <- leafletProxy("map", data = df)
    
    proxy %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~marker_radius,
        stroke = TRUE,
        color = FALSE,
        weight = 2,
        fillColor = ~fill_pal(color_group),
        fillOpacity = 0.85,
        layerId = ~as.character(business_id),
        label = ~name,
        popup = ~paste0(
          "<b>", name, "</b><br>",
          "<b>Live Activity:</b> ", busyness, "<br>",
          ifelse(is.na(categories), "", paste0(categories, "<br>")),
          ifelse(is.na(address), "", paste0(address, "<br>")),
          ifelse(is.na(stars), "", paste0("Stars: ", stars, "<br>")),
          ifelse(is.na(review_count), "", paste0("Reviews: ", review_count))
        )
      )
    
    legend_title <- dplyr::case_when(
      color_var == "major_cuisine" ~ "Cuisine",
      color_var == "RestaurantsPriceRange2" ~ "Price Range",
      color_var == "busyness" ~ "Live Activity",
      TRUE ~ "Legend"
    )
    
    legend_groups <- unique(na.omit(as.character(filtered_data()[[color_var]])))
    
    if (color_var == "RestaurantsPriceRange2") {
      legend_groups <- intersect(c("Under 10 per person", "$11-$30 per person", "$31-$60 per person", "Over $61 per person", "Unknown"), legend_groups)
    } else if (color_var == "Alcohol_type") {
      legend_groups <- intersect(c("none", "beer_and_wine", "full_bar"), legend_groups)
    } else if (color_var == "NoiseLevel") {
      legend_groups <- intersect(c("Quiet", "Moderate", "Loud", "Very Loud"), legend_groups)
    } else if (color_var == "busyness") {
      legend_groups <- intersect(c("Busy", "Moderate", "Quiet", "No recorded activity"), legend_groups)
    } else {
      legend_groups <- sort(legend_groups)
    }
    
    if (color_var %in% c("major_cuisine", "RestaurantsPriceRange2", "NoiseLevel", "Alcohol_type")) {
      legend_colors <- get_feature_colors(legend_groups, color_var)
    } else if (color_var == "busyness") {
      legend_colors <- c(
        "Busy" = "#d32323",
        "Moderate" = "#f59e0b",
        "Quiet" = "#16a34a",
        "No recorded activity" = "#9ca3af"
      )
      legend_colors <- legend_colors[legend_groups]
    } else {
      legend_colors <- stats::setNames(rep("#999999", length(legend_groups)), legend_groups)
    }
    
    legend_html <- build_clickable_legend_html(
      groups = legend_groups,
      color_values = legend_colors,
      excluded_groups = excluded_map_groups(),
      title = legend_title
    )
    
    proxy %>%
      addControl(
        html = legend_html,
        position = "bottomright",
        layerId = "clickable_legend"
      )
  })
  
  output$heatmap <- renderPlotly({
    df <- snapshot_data()
    
    validate(
      need(nrow(df) > 0, "No restaurants are visible in the current map window.")
    )
    
    counts <- checkins_day_hour_summary %>%
      filter(business_id %in% df$business_id) %>%
      group_by(day, hour) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      complete(day, hour = 0:23, fill = list(n = 0))
    
    validate(
      need(nrow(counts) > 0, "No check-in data for the currently visible restaurants.")
    )
    
    counts <- classify_traffic(counts)
    
    counts$day <- factor(
      counts$day,
      levels = rev(c("Sunday", "Monday", "Tuesday", "Wednesday",
                     "Thursday", "Friday", "Saturday"))
    )
    
    counts$traffic_label <- factor(
      counts$traffic_label,
      levels = c("Quiet", "Moderate", "Busy")
    )
    
    p <- ggplot(
      counts,
      aes(
        x = hour,
        y = day,
        fill = traffic_label,
        text = paste0(
          "Day: ", day,
          "<br>Hour: ", hour, ":00",
          "<br>Check-ins: ", n
        )
      )
    ) +
      geom_tile(color = "white") +
      scale_fill_manual(
        values = c(
          "Quiet" = "#edf8fb",
          "Moderate" = "#9ebcda",
          "Busy" = "#8856a7"
        ),
        drop = FALSE
      ) +
      scale_x_continuous(
        breaks = seq(0, 23, 4),
        labels = paste0(seq(0, 23, 4), ":00")
      ) +
      labs(x = "Hour of Day", y = "", fill = "Traffic") +
      theme_minimal()
    
    ggplotly(p) %>%
      style(
        hovertemplate = paste(
          "<div style='padding:8px 10px;'>",
          "<span style='font-size:13px; font-weight:700; color:#1f1f1f;'>%{y}</span><br>",
          "<span style='font-size:12px; color:#444;'>🕒 %{x}:00</span><br><br>",
          "<span style='font-size:11px; color:#888;'>Check-ins</span><br>",
          "<span style='font-size:16px; font-weight:700; color:#6366f1;'>%{z}</span>",
          "</div>",
          "<extra></extra>"
        )
      ) %>%
      layout(
        hoverlabel = premium_hoverlabel) %>%
      config(displayModeBar = FALSE)
  })
  
  
  selected_boxplot_attributes <- reactive({
    unique(c(
      input$boxplot_vibes,
      input$boxplot_meals,
      input$boxplot_services,
      input$boxplot_amenities,
      input$boxplot_music
    ))
  })
  observeEvent(input$clear_boxplot_filters, {
    updateSelectizeInput(session, "boxplot_vibes", selected = character(0))
    updateSelectizeInput(session, "boxplot_meals", selected = character(0))
    updateSelectizeInput(session, "boxplot_services", selected = character(0))
    updateSelectizeInput(session, "boxplot_amenities", selected = character(0))
    updateSelectizeInput(session, "boxplot_music", selected = character(0))
    updateSelectizeInput(session, "busyness_filter", selected = character(0))
    updateSliderInput(session, "min_rating", value = 1)
    updateSliderInput(session, "min_reviews", value = 0)
  })
  observeEvent(input$rating_feature, {
    if (input$rating_feature %in% c("NoiseLevel", "RestaurantsPriceRange2", "Alcohol_type")) {
      updateRadioButtons(session, "viz_type", selected = "Boxplot")
    } else {
      updateRadioButtons(session, "viz_type", selected = "Bar")
    }
  })
  observeEvent(input$remove_min_rating, {
    updateSliderInput(session, "min_rating", value = 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$remove_min_reviews, {
    updateSliderInput(session, "min_reviews", value = 0)
  }, ignoreInit = TRUE)
  
  observeEvent(event_data("plotly_click", source = "rating_plot_src"), {
    click <- event_data("plotly_click", source = "rating_plot_src")
    req(click)
    
    feature_col <- input$rating_feature
    
    clicked_group <- NULL
    
    if (!is.null(click$customdata)) {
      clicked_group <- as.character(click$customdata)
    } else if (input$viz_type == "Boxplot" && !is.null(click$x)) {
      clicked_group <- as.character(click$x)
    }
    
    req(!is.null(clicked_group))
    
    selected_rating_group(clicked_group)
    
    if (input$viz_type == "Bar" && !is.null(click$x)) {
      selected_rating_trace(as.character(click$x))
    } else {
      selected_rating_trace(NULL)
    }
    
    highlight_mode(TRUE)
    
    if (feature_col == "NoiseLevel") {
      updateSelectizeInput(session, "noise", selected = clicked_group)
    } else if (feature_col == "major_cuisine") {
      updateSelectizeInput(session, "cuisine", selected = clicked_group)
    } else if (feature_col == "RestaurantsPriceRange2") {
      updateSelectizeInput(session, "price", selected = clicked_group)
    } else if (feature_col == "Alcohol_type") {
      updateSelectizeInput(session, "alcohol", selected = clicked_group)
    }
  })
  observeEvent(input$clear_interactions, {
    selected_rating_group(NULL)
    selected_rating_trace(NULL)
    highlight_mode(FALSE)
    
    updateSelectizeInput(session, "noise", selected = character(0))
    updateSelectizeInput(session, "cuisine", selected = character(0))
    updateSelectizeInput(session, "price", selected = character(0))
    updateSelectizeInput(session, "alcohol", selected = character(0))
    updateSelectizeInput(session, "compare_groups", selected = character(0))
  })
  output$insight_text <- renderText({
    df <- snapshot_data()
    req(nrow(df) > 0)
    
    if (input$analysis_mode == "category") {
      req(input$rating_feature)
      
      feature_col <- input$rating_feature
      req(feature_col %in% names(df))
      
      tmp <- df %>%
        filter(!is.na(.data[[feature_col]]), !is.na(stars)) %>%
        mutate(feature_value = as.character(.data[[feature_col]]))
      if (feature_col == "Alcohol_type") {
        tmp <- tmp %>%
          mutate(feature_value = pretty_alcohol_label(feature_value))
      }
      
      if (!is.null(input$rating_star_focus) && input$rating_star_focus != "all") {
        tmp <- tmp %>%
          filter(as.character(stars) == input$rating_star_focus)
      }
      
      tmp <- tmp %>%
        group_by(group = feature_value) %>%
        summarise(
          avg_rating = mean(stars, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) %>%
        filter(n > 3) %>%
        arrange(desc(avg_rating))
      
      if (nrow(tmp) == 0) {
        return("Not enough data in the current view to generate an insight.")
      }
      
      best <- tmp[1, ]
      
      if (!is.null(input$rating_star_focus) && input$rating_star_focus != "all") {
        paste0(
          "For the ", input$rating_star_focus, "-star bucket, ",
          best$group,
          " has the highest count in the current view with ",
          best$n,
          " restaurants."
        )
      } else {
        paste0(
          best$group,
          " has the highest average rating in the current view at ",
          round(best$avg_rating, 2),
          " stars across ",
          best$n,
          " restaurants."
        )
      }
      
    } else {
      valid_cols <- intersect(selected_boxplot_attributes(), names(df))
      
      if (length(valid_cols) == 0) {
        return("Choose one or more attributes to generate an insight.")
      }
      
      tmp <- df %>%
        select(stars, all_of(valid_cols)) %>%
        filter(!is.na(stars)) %>%
        pivot_longer(
          cols = all_of(valid_cols),
          names_to = "attribute",
          values_to = "raw_value"
        ) %>%
        mutate(
          raw_chr = tolower(trimws(as.character(raw_value))),
          present = case_when(
            is.na(raw_value) ~ NA_character_,
            raw_value %in% c(TRUE, 1) ~ "Yes",
            raw_value %in% c(FALSE, 0) ~ "No",
            raw_chr %in% c("true", "yes", "1") ~ "Yes",
            raw_chr %in% c("false", "no", "0") ~ "No",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(present == "Yes") %>%
        group_by(attribute) %>%
        summarise(
          avg_rating = mean(stars, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) %>%
        filter(n > 3) %>%
        arrange(desc(avg_rating))
      
      if (nrow(tmp) == 0) {
        return("Not enough attribute data in the current view to generate an insight.")
      }
      
      best <- tmp[1, ]
      
      paste0(
        pretty_attribute_label(best$attribute),
        " is associated with the highest average rating in the current view at ",
        round(best$avg_rating, 2),
        " stars across ",
        best$n,
        " restaurants where that attribute is present."
      )
    }
  })
  drilldown_data <- reactive({
    df <- snapshot_data()
    
    req(highlight_mode(), input$rating_feature)
    req(!is.null(selected_rating_group()))
    
    feature_col <- input$rating_feature
    
    if (input$viz_type == "Bar") {
      req(!is.null(selected_rating_trace()))
      df <- df %>%
        filter(
          as.character(.data[[feature_col]]) == as.character(selected_rating_group()),
          as.character(stars) == as.character(selected_rating_trace())
        )
    } else {
      df <- df %>%
        filter(as.character(.data[[feature_col]]) == as.character(selected_rating_group()))
    }
    
    df %>%
      select(name, major_cuisine, stars, review_count, NoiseLevel, RestaurantsPriceRange2, address)
  })
  
  output$drilldown_table <- renderTable({
    df <- drilldown_data()
    validate(need(nrow(df) > 0, "Click a bar or boxplot group to see restaurants here."))
    head(df, 12)
  })
  observe({
    req(input$rating_feature)
    
    df <- snapshot_data()
    feature_col <- input$rating_feature
    
    req(feature_col %in% names(df))
    
    choices <- sort(unique(na.omit(as.character(df[[feature_col]]))))
    updateSelectizeInput(session, "compare_groups", choices = choices, server = TRUE)
  })
  
  observeEvent(input$reset_view, {
    leafletProxy("map") %>%
      fitBounds(
        lng1 = min(map_df$longitude, na.rm = TRUE),
        lat1 = min(map_df$latitude, na.rm = TRUE),
        lng2 = max(map_df$longitude, na.rm = TRUE),
        lat2 = max(map_df$latitude, na.rm = TRUE)
      )
  })
  output$rating_analysis_plot <- renderPlotly({
    df <- snapshot_data()
    
    validate(
      need(nrow(df) > 0, "No restaurants are visible in the current map window.")
    )
    
    nice_labels <- c(
      trendy = "Trendy",
      romantic = "Romantic",
      intimate = "Intimate",
      touristy = "Touristy",
      hipster = "Hipster",
      divey = "Divey",
      classy = "Classy",
      upscale = "Upscale",
      casual = "Casual",
      breakfast = "Breakfast",
      lunch = "Lunch",
      dinner = "Dinner",
      latenight = "Late Night",
      dessert = "Dessert",
      brunch = "Brunch",
      RestaurantsTakeOut = "Takeout",
      RestaurantsReservations = "Reservations",
      Caters = "Catering",
      DriveThru = "Drive-thru",
      HappyHour = "Happy Hour",
      RestaurantsGoodForGroups = "Good for groups",
      GoodForKids = "Good for kids",
      DogsAllowed = "Dogs allowed",
      CoatCheck = "Coat Check",
      OutdoorSeating = "Outdoor seating",
      Open24Hours = "Open 24 Hours",
      HasTV = "Has TV",
      WheelchairAccessible = "Wheelchair Accessible",
      background_music = "Background Music",
      jukebox = "Jukebox",
      live = "Live Music",
      video = "Video",
      karaoke = "Karaoke"
    )
    
    req(input$analysis_mode)
    
    # ---------------- CATEGORY MODE ----------------
    if (input$analysis_mode == "category") {
      
      req(input$rating_feature, input$viz_type)
      
      feature_col <- input$rating_feature
      label_name <- pretty_analysis_label(feature_col)
      
      df$feature <- as.character(df[[feature_col]])
      
      if (feature_col == "Alcohol_type") {
        df$feature <- pretty_alcohol_label(df$feature)
        df$feature <- factor(
          df$feature,
          levels = c("No Alcohol", "Beer & Wine", "Full Bar")
        )
      }
      
      df <- df %>%
        filter(!is.na(feature), !is.na(stars))
      
      if (feature_col == "RestaurantsPriceRange2") {
        df <- df %>%
          filter(
            !feature %in% c("Unknown", "unknown", "NA", "", "N/A")
          )
      }
      
      if (isTRUE(input$compare_mode) && length(input$compare_groups) > 0) {
        df <- df %>% filter(feature %in% input$compare_groups)
      }
      
      validate(
        need(nrow(df) > 0, "No rating data available.")
      )
      
      feature_levels <- df %>%
        count(feature, sort = TRUE) %>%
        pull(feature)
      
      df$feature <- factor(df$feature, levels = feature_levels)
      
      full_palette <- get_fixed_palette(feature_col)
      
      if (length(full_palette) > 0) {
        color_values <- full_palette[feature_levels]
      } else {
        color_values <- stats::setNames(
          grDevices::hcl.colors(length(feature_levels), "Set 2"),
          feature_levels
        )
      }
      
      # -------- BAR CHART --------
      if (input$viz_type == "Bar") {
        
        plot_df <- df %>%
          mutate(stars = as.character(stars))
        
        if (!is.null(input$rating_star_focus) && input$rating_star_focus != "all") {
          plot_df <- plot_df %>% filter(stars == input$rating_star_focus)
        }
        
        plot_df <- plot_df %>%
          count(feature, stars, name = "count") %>%
          mutate(
            stars = factor(stars, levels = c("1","1.5","2","2.5","3","3.5","4","4.5","5")),
            feature = factor(feature, levels = feature_levels)
          )
        
        if (!is.null(input$rating_star_focus) && input$rating_star_focus != "all") {
          
          p <- ggplot(
            plot_df,
            aes(
              x = feature,
              y = count,
              fill = feature,
              customdata = feature,
              text = paste0(
                "<b>", label_name, ":</b> ", feature,
                "<br>Count: ", count
              )
            )
          ) +
            geom_col() +
            scale_fill_manual(values = color_values) +
            labs(x = label_name, y = "The # of Restaraunts. ") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
        } else {
          
          p <- ggplot(
            plot_df,
            aes(
              x = stars,
              y = count,
              fill = feature,
              customdata = feature,
              text = paste0(
                "<b>", label_name, ":</b> ", feature,
                "<br>Stars: ", stars,
                "<br>Count: ", count
              )
            )
          ) +
            geom_col(position = "dodge") +
            scale_fill_manual(values = color_values) +
            labs(x = "Stars", y = "Count") +
            theme_minimal()
        }
        
      } else {
        # -------- BOXPLOT --------
        p <- ggplot(
          df,
          aes(
            x = feature,
            y = stars,
            fill = feature,
            customdata = feature,
            text = paste0(
              "<b>", label_name, ":</b> ", feature,
              "<br>Rating: ", stars
            )
          )
        ) +
          geom_boxplot() +
          scale_fill_manual(values = color_values) +
          labs(x = label_name, y = "Rating") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        if (feature_col == "Alcohol_type") {
          p <- p + scale_fill_manual(
            values = c(
              "Beer & Wine" = "#9ecae1",
              "Full Bar" = "#3182bd"
            )
          )
        }
      }
      
    } else {
      
      # ---------------- ATTRIBUTE MODE ----------------
      valid_cols <- intersect(selected_boxplot_attributes(), names(df))
      
      validate(
        need(length(valid_cols) > 0, "Select attributes to compare.")
      )
      
      plot_df <- df %>%
        select(stars, all_of(valid_cols)) %>%
        pivot_longer(
          cols = all_of(valid_cols),
          names_to = "attribute",
          values_to = "value"
        ) %>%
        mutate(
          attribute = pretty_attribute_label(attribute),
          present = ifelse(value == TRUE, "Yes", "No")
        ) %>%
        filter(present == "Yes")
      
      p <- ggplot(
        plot_df,
        aes(
          x = attribute,
          y = stars,
          fill = attribute,
          customdata = attribute,
          text = paste0(
            "<b>", attribute, "</b><br>Rating: ", stars
          )
        )
      ) +
        geom_boxplot() +
        labs(x = "Attribute", y = "Rating") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    gg <- ggplotly(p, tooltip = "text", source = "rating_plot_src") %>%
      layout(hoverlabel = premium_hoverlabel) %>%
      config(displayModeBar = FALSE)
    
    event_register(gg, "plotly_click")
    
    gg
  })
  
  # one reactiveVal only
  selected_restaurant <- reactiveVal(NULL)
  
  # one click observer only
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    
    df <- visible_data()
    restaurant <- df %>%
      filter(as.character(business_id) == as.character(click$id))
    
    if (nrow(restaurant) > 0) {
      selected_restaurant(restaurant[1, , drop = FALSE])
    }
  })
  
  # one output only
  output$restaurant_info <- renderUI({
    restaurant <- selected_restaurant()
    
    
    if (is.null(restaurant) || nrow(restaurant) == 0) {
      return(
        div(
          class = "section-spacing",
          tags$p("Click a restaurant on the map to see details."),
          tags$p(style = "color:#777;", "You’ll see cuisine, pricing, rating, address, and vibe information here.")
        )
      )
    }
    busyness_label <- if ("busyness" %in% names(restaurant)) restaurant$busyness[1] else "No recorded activity"
    vibes <- c(
      "trendy", "romantic", "intimate", "touristy", "hipster",
      "divey", "classy", "upscale", "casual"
    )
    
    vibes_existing <- intersect(vibes, names(restaurant))
    
    active_vibes <- vibes_existing[
      sapply(vibes_existing, function(v) {
        val <- restaurant[[v]][1]
        !is.na(val) && isTRUE(val)
      })
    ]
    
    tagList(
      div(class = "detail-name", restaurant$name[1]),
      div(
        class = paste0(
          "status-badge ",
          dplyr::case_when(
            busyness_label == "Busy" ~ "status-busy",
            busyness_label == "Moderate" ~ "status-moderate",
            busyness_label == "Quiet" ~ "status-quiet",
            TRUE ~ "status-na"
          )
        ),
        busyness_label
      ),
      
      if (!is.na(restaurant$stars[1])) {
        div(class = "detail-pill", paste0("⭐ ", restaurant$stars[1], " stars"))
      },
      if (!is.na(restaurant$review_count[1])) {
        div(class = "detail-pill", paste0(restaurant$review_count[1], " reviews"))
      },
      if (!is.na(restaurant$RestaurantsPriceRange2[1]) && restaurant$RestaurantsPriceRange2[1] != "") {
        div(class = "detail-pill", paste0("Price: ", restaurant$RestaurantsPriceRange2[1]))
      },
      if (!is.na(restaurant$major_cuisine[1]) && restaurant$major_cuisine[1] != "") {
        div(class = "detail-pill", restaurant$major_cuisine[1])
      },
      
      div(class = "section-spacing"),
      
      if (!is.na(restaurant$categories[1]) && restaurant$categories[1] != "") {
        p(tags$strong("Cuisine: "), restaurant$categories[1])
      },
      if (!is.na(restaurant$NoiseLevel[1]) && restaurant$NoiseLevel[1] != "") {
        p(tags$strong("Noise Level: "), restaurant$NoiseLevel[1])
      },
      if (!is.na(restaurant$address[1]) && restaurant$address[1] != "") {
        p(tags$strong("Address: "), restaurant$address[1])
      },
      
      if (length(active_vibes) > 0) {
        tagList(
          p(tags$strong("Vibes:")),
          div(
            lapply(active_vibes, function(v) {
              div(class = "detail-pill", str_to_title(v))
            })
          )
        )
      }
    )
  })
  
 
}

shinyApp(ui = ui, server = server)
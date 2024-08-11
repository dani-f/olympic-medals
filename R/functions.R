# Setup -------------------------------------------------------------------
# library(httr)
# library(rvest)
# library(tidyverse)
# library(magrittr)
# library(purrr)
# library(janitor)



# Scrape the data from Wikipedia ------------------------------------------

# Create Scraping Function
scrape_and_clean_olympics_wiki <- function(selected_year, selected_country){
  
  # Pass URLs ---------------------------------------------------------------
  olympics_url <- paste0("https://en.wikipedia.org/wiki/", selected_country, "_at_the_", selected_year, "_Summer_Olympics")
  olympics_overview_url <- "https://en.wikipedia.org/wiki/List_of_Olympic_Games_host_cities"
  
  # Scrape html -------------------------------------------------------------
  olympics_html_raw <- read_html(GET(olympics_url, config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)))
  olympics_overview_html_raw <- read_html(GET(olympics_overview_url, config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)))
  
  
  # Get start and end dates -------------------------------------------------
  olympics_overview_raw <- olympics_overview_html_raw %>% 
    html_table %>%
    magrittr::extract2(2)
  
  olympics_overview_cleaned <- olympics_overview_raw %>%
    select(-1) %>% 
    clean_names() %>% 
    # Filter for summer Olympics only
    filter(summer != "") %>% 
    transmute(
      year,
      # extract all characters before the "[" character (footnotes in Wikipedia)
      opening_ceremony = str_extract(opening_ceremony, "^[^\\[]*"),
      closing_ceremony = str_extract(closing_ceremony, "^[^\\[]*"),
      start_date = as_date(opening_ceremony, format = c("%d %B %Y")),
      end_date = as_date(closing_ceremony, format = c("%d %B %Y"))) %>% 
    select(-opening_ceremony, -closing_ceremony)
  
  
  # Get Olympic medals table ------------------------------------------------
  olympics_medal_table_raw <- olympics_html_raw %>% 
    html_elements(".wikitable") %>% 
    html_table() %>%
    magrittr::extract2(1)
  
  result <- olympics_medal_table_raw %>%
    clean_names() %>% 
    mutate(year = selected_year,
           country = selected_country,
           date_cod = as_date(paste0(date,
                                     " ",
                                     # Attention: if year is 2020, it was postponed to 2021
                                     if_else(year == 2020, 2021, year)),
                                     format = c("%B %d %Y", "%d %B %Y"))) %>% 
    left_join(olympics_overview_cleaned, join_by(year))
  
  # Return the data
  result
}

# Create wrapper function to handle multiple tables at once
scrape_and_clean_olympics_wiki_multiple <- function(selected_year, selected_country) {
  
  # Create a data frame of all combinations of year and country
  combinations <- expand_grid(selected_year, selected_country)
  
  # Use purrr::map_dfr to apply the function to each combination
  result <- pmap_dfr(combinations, scrape_and_clean_olympics_wiki)
  
  # Return the data
  result
}


# Prepare Data ------------------------------------------------------------

# Create function to transform the data
transform_olympics_data_as_list <- function(olympics_medal_table) {
  
  # Create all year, country, medal and matchday combinations
  match_days <- olympics_medal_table %>% 
    distinct(year, country, medal, start_date, end_date) %>% 
    group_by(year, country, medal) %>% 
    summarise(days = as.numeric(
      # Only until today if Olympic games are ongoing
      if_else(end_date <= today(), end_date, today())
      -
        start_date)) %>% 
    group_by(year) %>% 
    complete(country, medal, days = 0 : max(days)) %>% 
    ungroup()
  
  olympics_medal_table_summarized <- olympics_medal_table %>%
    mutate(day = as.numeric(date_cod - start_date)) %>% 
    group_by(year, country, medal, day) %>%
    summarize(count_per_year_country_medal_day = n(),
              winner_disciplines_grouped = str_c(sport, collapse="\n")) %>%
    ungroup() %>% 
    # Add all combinations for plotting days with 0 medals
    right_join(match_days,
               join_by(year == "year", "country" == "country", "medal" == "medal", "day" == "days"),
               relationship = "many-to-many") %>% 
    # Replace NA with 0 medals
    mutate(count_per_year_country_medal_day = replace_na(count_per_year_country_medal_day, 0))
  
  olympics_medal_table_summarized_pivoted <- olympics_medal_table_summarized %>% 
    pivot_wider(id_cols = c(year, country, day),
                names_from = medal,
                values_from = count_per_year_country_medal_day) %>% 
    mutate(total_medal = Bronze + Gold + Silver) %>% 
    # Arrange for cumsum function
    arrange(year, country, day) %>% 
    group_by(year, country) %>% 
    mutate(running_total_year_country = cumsum(total_medal),
           running_total_gold_year_country = cumsum(Gold),
           running_total_silver_year_country = cumsum(Silver),
           running_total_bronze_year_country = cumsum(Bronze)) %>% 
    ungroup()
  
  # Compute the max days for each year
  max_days <- olympics_medal_table_summarized %>%
    group_by(year) %>%
    summarise(max_day = max(day)) %>% 
    ungroup()
  
  # Save all results in list
  olympics_list <- list("olympics_medal_table_summarized" = olympics_medal_table_summarized,
                        "olympics_medal_table_summarized_pivoted" = olympics_medal_table_summarized_pivoted,
                        "max_days" = max_days)
  
  # Return the data
  olympics_list

}



# Plot running total comparison -------------------------------------------

# Create function to plot the data
plot_olympics_data <- function(olympics_medal_table_summarized_pivoted,
                               max_days,
                               selected_country) {

  plot_data <- olympics_medal_table_summarized_pivoted %>% 
    # Works only with first input country (in case there are more)
    filter(country == selected_country[[1]]) %>%
    # Create a label column for plotting based on conditions
    mutate(label = if_else(
      # For updates during the Olympic games the condition in if_else() was set to
      # "day %in% max_days$max_day"
      (year == max_days[1, ]$year & day == max_days[1, ]$max_day) |
        (year == max_days[2, ]$year & day == max_days[2, ]$max_day),
      paste0(running_total_year_country, " Medals\n(",
             running_total_gold_year_country, " Gold\n",
             running_total_silver_year_country, " Silver\n",
             running_total_bronze_year_country, " Bronze)"),
      ""))
  
  plot <- plot_data %>% 
    ggplot(aes(x = day,
               y = running_total_year_country,
               color = factor(year))) +
    geom_line(aes(linetype = factor(year)), linewidth = 1) +
    geom_point(size = 3) +
    geom_text_repel(aes(label = label), 
                    vjust = if_else(plot_data$year == selected_year[[1]], -0.5, 0),
                    hjust = if_else(plot_data$year == selected_year[[1]], 0, 1),
                    size = 4,
                    show.legend = FALSE)
  
  # Return the plot
  plot

}



# Calculate slope ---------------------------------------------------------

# Create function for the slope
add_slope_olympics_data <- function(olympics_medal_table_summarized_pivoted) {

  fitted_model <- olympics_medal_table_summarized_pivoted %>%
    # Works only with first input country (in case there are more)
    filter(country == selected_country[[1]]) %>%
    group_by(year, country) %>%
    summarise(last_day = max(day), max_running_total_year_country = max(running_total_year_country)) %>% 
    ungroup() %>% 
    # Extract slope coefficient in new variable
    mutate(slope = coef(lm(max_running_total_year_country ~ last_day, data = .))[["last_day"]])
  
  # Return the data
  fitted_model
  
}



# Plot running total per medal comparison ---------------------------------

# Create function to plot the data
plot_olympics_data_per_medal <- function(olympics_medal_table_summarized,
                                         selected_country,
                                         with_labels = TRUE) {
  
  # Plot running total per medal comparison
  plot_data <- olympics_medal_table_summarized %>% 
    # Works only with first input country (in case there are more)
    filter(country == selected_country[[1]]) %>%
    mutate(winner_disciplines_grouped = replace_na(winner_disciplines_grouped, "")) %>% 
    # Arrange for cumsum()
    arrange(year, country, medal, day) %>%
    group_by(year, country, medal) %>% 
    mutate(running_total_medal_year_country = cumsum(count_per_year_country_medal_day)) %>% 
    ungroup() %>% 
    # Reorder medal variable for plotting
    mutate(medal = fct_relevel(medal, "Gold", "Silver", "Bronze"))
  
  plot <- plot_data %>% 
    ggplot(aes(x = day,
               y = running_total_medal_year_country,
               color = medal)) +
    geom_line(aes(linetype = factor(year)), linewidth = 1) +
    geom_point(size = 3) +
    facet_grid(medal ~ .)
  
  # Return the plot with or without labelling
  if(with_labels == TRUE) {
    plot +
      geom_text_repel(
        aes(label =
              if_else(plot_data$year == selected_year[[1]],
                      "",
                      winner_disciplines_grouped)),
        size = 4,
        nudge_y = 2.5,
        nudge_x = 0.25,
        show.legend = FALSE)
  } else {
    plot
  }
  
}

data_wrangling
================

``` r
## cleaning Castaway Details dataset: filtering-out non-US seasons, creating personality type variable
castaway_details_us = castaway_details %>% 
  filter(str_detect(castaway_id, '^US')) %>%
  mutate(
    personality_type_binary = ifelse(
      str_detect(personality_type, '^E'), "Extrovert", "Introvert")) %>%
  select(-c(castaway_id, castaway, personality_type))

## Castaways dataset: filtering-out non-US seasons, renaming variables
castaways_us = castaways %>% filter(version == "US") %>%
  select(-c(version, season_name, season, castaway_id, castaway, jury_status, original_tribe)) %>%
  rename(age_during_show = age, days_survived = day)

## joining datasets
survivor_data_final = full_join(castaway_details_us, castaways_us, by = "full_name")

# check for multiple unique names per season
contestant_count_unique_rec = survivor_data_final %>%
  group_by(version_season, full_name) %>%
  summarise(count = n()) %>%
  mutate(num = row_number())
```

    ## `summarise()` has grouped output by 'version_season'. You can override using
    ## the `.groups` argument.

``` r
## summarizing the number of contestants per season & adding to joined dataset
contestant_count_df = survivor_data_final %>%
  group_by(version_season) %>%
  summarise(contestant_count = n_distinct(full_name))

survivor_data_final = full_join(survivor_data_final, contestant_count_df, by = "version_season")

## reordering variables, create new variable ethnicity, fix personality type variable
survivor_data_final = survivor_data_final %>%
  select(c("version_season", "full_name", "age_during_show", "race", "poc", "date_of_birth", "date_of_death", "occupation", "gender", "ethnicity", "personality_type_binary", "episode", "days_survived", "order", "contestant_count", "result", "city", "state")) %>% 
    mutate(ethnicity = ifelse(
      str_detect(poc, 'White'), survivor_data_final$poc, survivor_data_final$race)) %>% 
  arrange(version_season) %>%
  mutate(personality_type_binary = as.factor(personality_type_binary))

## adding region variable
survivor_data_final = survivor_data_final %>%
mutate(region = 
    ifelse(
    state == "Connecticut" 
    | state == "Maine" 
    | state == "Massachusetts" 
    | state == "New Hampshire"
    | state == "Rhode Island"
    | state == "Vermont" 
    | state == "New Jersey" 
    | state == "New York" 
    | state == "Pennsylvania", 
    "Northeast",
    
    ifelse(
    state == "Delaware" 
    | state == "District of Columbia" 
    | state == "Florida" 
    | state == "Georgia" 
    | state == "Maryland" 
    | state == "North Carolina"
    | state == "South Carolina" 
    | state == "Virginia"
    | state == "West Virginia" 
    | state == "Alabama"
    | state == "Kentucky" 
    | state == "Mississippi"
    | state == "Tennessee" 
    | state == "Arkansas" 
    | state == "Louisiana" 
    | state == "Oklahoma" 
    | state == "Texas", 
      "South", 
      
    ifelse(
      state == "Arizona"
      | state == "Colorado"
      | state == "Idaho"
      | state == "New Mexico"
      | state == "Montana"
      | state == "Utah" 
      | state == " Nevada"
      | state == "Wyoming"
      | state == "Alaska"
      | state == "California"
      | state == "Hawaii"
      | state == "Oregon"
      | state == "Washington"
    ,  "West", "Midwest"
    )))
  ) %>%
  mutate(NE = ifelse(region == "Northeast", 1, 0)) %>%
  mutate(South = ifelse(region == "South", 1, 0)) %>%
  mutate(West = ifelse(region == "West", 1, 0)) %>%
  mutate(Midwest = ifelse(region == "Midwest", 1, 0))

survivor_data_final_including_2_41_42_43 = survivor_data_final

write.csv(survivor_data_final_including_2_41_42_43, file = "./data/survivor_data_final_including_2_41_42_43.csv")

# preparing data for survival analysis

## season 41 and 42, the longest survival time is 26 days, exclude these seasons
## season 2, the longest survival time is 42 days, exclude this season
## season 43 is incomplete, exclude this season

survivor_data_final = survivor_data_final %>%
  filter(!(version_season %in% c("US02", "US41", "US42", "US43")))

## calculating percent NAs for all variables
survivor_data_final %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
```

    ## # A tibble: 1 × 23
    ##   version_seas…¹ full_…² age_d…³ race_…⁴ poc_n…⁵ date_…⁶ date_…⁷ occup…⁸ gende…⁹
    ##            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1              0       0       0   0.728 0.00549 0.00549   0.981 0.00549 0.00549
    ## # … with 14 more variables: ethnicity_name <dbl>,
    ## #   personality_type_binary_name <dbl>, episode_name <dbl>,
    ## #   days_survived_name <dbl>, order_name <dbl>, contestant_count_name <dbl>,
    ## #   result_name <dbl>, city_name <dbl>, state_name <dbl>, region_name <dbl>,
    ## #   NE_name <dbl>, South_name <dbl>, West_name <dbl>, Midwest_name <dbl>, and
    ## #   abbreviated variable names ¹​version_season_name, ²​full_name_name,
    ## #   ³​age_during_show_name, ⁴​race_name, ⁵​poc_name, ⁶​date_of_birth_name, …

``` r
## Saving final dataset as csv file
write.csv(survivor_data_final, file = "./data/survivor_data_final.csv")
```

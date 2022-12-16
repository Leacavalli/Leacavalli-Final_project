BST260 final project
================
Léa Cavalli
2022-12-03

``` r
library(rvest)
library(readr)
library(tidyverse)
library(caret)
library(tidyverse)
library(FNN)
```

## Load Covid mortality

``` r
html_covid_rates <- read_html("https://www.statista.com/statistics/1104709/coronavirus-deaths-worldwide-per-million-inhabitants/")
tab_covid_rates_raw <- (html_covid_rates |> html_nodes("table"))[[1]] |> html_table()
df_covid_rates <- tab_covid_rates_raw |>
  mutate(Total_cases= as.numeric(gsub(",", "", `Confirmed cases (absolute)`)),
         Total_deaths=as.numeric(gsub(",", "", `Confirmed deaths (absolute)`)), 
         Pop_millions= as.numeric(gsub(",", "", `Population (in millions)`)), 
         Country=Characteristic) |>
  select(Country, Total_cases, Total_deaths, Pop_millions)

df_covid_rates
```

    ## # A tibble: 154 x 4
    ##    Country                Total_cases Total_deaths Pop_millions
    ##    <chr>                        <dbl>        <dbl>        <dbl>
    ##  1 Peru                       3691213       213714        33.0 
    ##  2 Bulgaria                   1180636        37279         6.93
    ##  3 Bosnia and Herzegovina      379982        15810         3.28
    ##  4 Hungary                    1932788        46661         9.75
    ##  5 Georgia                    1662299        16844         3.71
    ##  6 North Macedonia             316729         9337         2.07
    ##  7 Moldova                     521182        11570         2.62
    ##  8 Croatia                    1160253        16124         4.05
    ##  9 Czechia                    3943792        40334        10.7 
    ## 10 Slovakia                   2559088        20163         5.46
    ## # ... with 144 more rows

## Load data

# vaccination counts

``` r
library (RCurl)
url <- getURL("https://covid.ourworldindata.org/data/owid-covid-data.csv")
tab_vax_rates_raw <- read.csv(text = url)
df_vax_rates <- tab_vax_rates_raw |>
  select(location, date, people_fully_vaccinated, continent) |>
  filter(!is.na(people_fully_vaccinated)) |>
  group_by(location, continent) |>
  summarise(fully_vax_count= max(people_fully_vaccinated)) |>
  rename(Country =location, Continent=continent) 
```

    ## `summarise()` has grouped output by 'location'. You can override using the `.groups` argument.

# Average Cumulative number of lockdown days since the beginning of the pandemic

``` r
html_lockdown <- read_html("https://en.wikipedia.org/wiki/COVID-19_lockdowns")
tab_lockdown_raw <- html_table(html_node(html_lockdown, ".wikitable"), fill = TRUE)
colnames(tab_lockdown_raw) <- tab_lockdown_raw[1, ] 

df_lockdown <- tab_lockdown_raw[-c(1,2),] |>
  select(`Country / territory`, `Total length (days)`) |>
  rename(Country =`Country / territory`, 
         Total_days =`Total length (days)`) |>
  group_by(Country) |>
  summarise(Avg_Total_lockdown_days = mean(as.numeric(Total_days)))
```

    ## Warning in mean(as.numeric(Total_days)): NAs introduced by coercion

    ## Warning in mean(as.numeric(Total_days)): NAs introduced by coercion

    ## Warning in mean(as.numeric(Total_days)): NAs introduced by coercion

    ## Warning in mean(as.numeric(Total_days)): NAs introduced by coercion

# proportion of the population above 65

``` r
html_above_65 <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_age_structure")
tab_above_65_raw <- html_table(html_node(html_above_65, ".wikitable"), fill = T)
colnames(tab_above_65_raw) <- tab_above_65_raw[1, ] 

df_above_65<- tab_above_65_raw[-c(1,2),] |>
  select(Country, `Over 65`) |>
  rename(Percent_Over_65 =`Over 65`) |>
  mutate(Percent_Over_65= parse_number(Percent_Over_65))
```

# Population density

``` r
html_pop_density <- read_html("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population_density")
tab_pop_density_raw <- html_table(html_node(html_pop_density, ".wikitable"), fill = T)
colnames(tab_pop_density_raw) <- tab_pop_density_raw[1, ]

df_pop_density <- tab_pop_density_raw[-1,] |>
  select(`Country, territory or dependency`, `/km2`) |>
  rename(Country =`Country, territory or dependency`, km2 = `/km2`) |>
  mutate(km2= as.numeric(gsub(",", "", km2)))
```

# GDP per capita

``` r
html_GDP_per_capita <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita")
tab_GDP_per_capita_raw <- html_table(html_node(html_GDP_per_capita, ".wikitable"), fill = T)
colnames(tab_GDP_per_capita_raw) <- tab_GDP_per_capita_raw[1, ]

df_GDP_per_capita <- (tab_GDP_per_capita_raw[-1,] |>
  select(`Country/Territory`, 5) |>
  rename(Country=`Country/Territory`, GDP_per_capita = Estimate)|>
  mutate(GDP_per_capita= as.numeric(gsub(",", "", GDP_per_capita)),
         Country= str_trim(gsub("\\*", "", Country) ) ))[-1,]
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

# obesity rates

``` r
html_obesity_rates <- read_html("https://en.wikipedia.org/wiki/List_of_sovereign_states_by_obesity_rate")
tab_obesity_rates_raw <- html_table(html_node(html_obesity_rates, ".wikitable"), fill = T)

df_obesity_rates <- tab_obesity_rates_raw |>
  rename(Obesity_rate=`Obesity rate (%)`)
```

``` r
# cardiovascular disease rate
library(owidR)
tab_CVD_rate_raw <- owid("cardiovascular-disease-death-rates")
df_CVD_rate <- tab_CVD_rate_raw |>
  rename(Country =entity, cvd_death_rate="Deaths - Cardiovascular diseases - Sex: Both - Age: Age-standardized (Rate)") |>
  group_by(Country) |>
  filter(year==max(year), !is.na(cvd_death_rate)) |>
  select(Country, cvd_death_rate) 
```

``` r
# format country names 

# df_covid_rates
df_covid_rates$Country <- str_replace_all(df_covid_rates$Country, "\\p{No}", "")
df_covid_rates$Country <- gsub('USA', 'United States', df_covid_rates$Country)
df_covid_rates$Country <- gsub('Timor-Leste', 'Timor', df_covid_rates$Country)
df_covid_rates$Country <- gsub("Congo \\(Kinshasa)", "Democratic Republic of Congo", df_covid_rates$Country)
df_covid_rates$Country <- gsub('Congo \\(Brazzaville)', 'Congo', df_covid_rates$Country)
df_covid_rates$Country <- gsub("Czechia", "Czech Republic", df_covid_rates$Country)

# df_vax_rates
df_vax_rates$Country <- gsub("Czechia", "Czech Republic", df_vax_rates$Country)

# df_above_65
df_above_65$Country <- gsub('Democratic Republic of the Congo'  , 'Democratic Republic of Congo', df_above_65$Country)
df_above_65$Country <- gsub('Republic of the Congo'  , 'Congo', df_above_65$Country)
df_above_65$Country <- gsub("Timor-Leste"  , "Timor", df_above_65$Country)
df_above_65$Country <- gsub("Ivory Coast"  , "Cote d'Ivoire", df_above_65$Country)
df_above_65$Country <- gsub("Eswatini \\(Swaziland)"  , "Eswatini", df_above_65$Country)

# df_pop_density
df_pop_density$Country <- gsub('DR Congo'  , 'Democratic Republic of Congo', df_pop_density$Country)
df_pop_density$Country <- gsub('Cyprus \\[note 5]'  , 'Cyprus', df_pop_density$Country)
df_pop_density$Country <- gsub('Russia \\[note 12]'  , 'Russia', df_pop_density$Country)
df_pop_density$Country <- gsub('Ukraine \\[note 9]'  , 'Ukraine', df_pop_density$Country)
df_pop_density$Country <- gsub('Ivory Coast'  , "Cote d'Ivoire", df_pop_density$Country)
df_pop_density$Country <- gsub('East Timor'  , 'Timor', df_pop_density$Country)

# GDP per capita
df_GDP_per_capita$Country <- gsub('East Timor'  , 'Timor', df_GDP_per_capita$Country)
df_GDP_per_capita$Country <- gsub('DR Congo'  , 'Democratic Republic of Congo', df_GDP_per_capita$Country)
df_GDP_per_capita$Country <- gsub('Ivory Coast'  , "Cote d'Ivoire", df_GDP_per_capita$Country)


# obesity rates
df_obesity_rates$Country <- gsub('Timor-Leste'  , 'Timor', df_obesity_rates$Country)
df_obesity_rates$Country <- gsub('Democratic Republic of the Congo'  , 'Democratic Republic of Congo', df_obesity_rates$Country)
df_obesity_rates$Country <- gsub('Republic of the Congo'  , 'Congo', df_obesity_rates$Country)
df_obesity_rates$Country <- gsub("Ivory Coast"  , "Cote d'Ivoire", df_obesity_rates$Country)

# df_CVD_rate
df_CVD_rate$Country <- gsub("Czechia", "Czech Republic", df_CVD_rate$Country)
```

``` r
# join and transform data to create the analysis-ready dataframe 
analysis_df <- left_join(df_covid_rates, df_vax_rates) |>
  left_join(df_above_65)|>
  left_join(df_pop_density)|>
  left_join(df_GDP_per_capita)|>
  left_join(df_obesity_rates)|>
  left_join(df_CVD_rate)
```

    ## Joining, by = "Country"
    ## Joining, by = "Country"
    ## Joining, by = "Country"
    ## Joining, by = "Country"
    ## Joining, by = "Country"
    ## Joining, by = "Country"

``` r
analysis_df
```

    ## # A tibble: 154 x 11
    ##    Country                Total_cases Total_deaths Pop_millions Continent     fully_vax_count Percent_Over_65   km2 GDP_per_capita Obesity_rate cvd_death_rate
    ##    <chr>                        <dbl>        <dbl>        <dbl> <chr>                   <dbl>           <dbl> <dbl>          <dbl>        <dbl>          <dbl>
    ##  1 Peru                       3691213       213714        33.0  South America        28382375            8.05    26           6692         19.7           88.6
    ##  2 Bulgaria                   1180636        37279         6.93 Europe                2074555           20.1     62          11635         25            541. 
    ##  3 Bosnia and Herzegovina      379982        15810         3.28 Europe                 846080           16.2     64           6916         17.9          344. 
    ##  4 Hungary                    1932788        46661         9.75 Europe                6206878           20.7    104          18773         26.4          302. 
    ##  5 Georgia                    1662299        16844         3.71 Asia                  1276173           16.8     53           5042         21.7          454  
    ##  6 North Macedonia             316729         9337         2.07 Europe                 838032           14.2     82           6721         22.4          569. 
    ##  7 Moldova                     521182        11570         2.62 Europe                1073162           14.0     90           5315         18.9          405. 
    ##  8 Croatia                    1160253        16124         4.05 Europe                2250353           21.1     72          17399         24.4          266. 
    ##  9 Czech Republic             3943792        40334        10.7  Europe                6891525           20.2    133          26379         26            232. 
    ## 10 Slovakia                   2559088        20163         5.46 Europe                2577827           17.0    111          21088         20.5          299. 
    ## # ... with 144 more rows

``` r
# Exploratory 
analysis_df[which(is.na(analysis_df$Continent)),]$Continent <- c("Asia","Africa")

prop_per_continent <- analysis_df |>
  group_by(Continent)|>
  summarize(sum_cases= sum(Total_cases), 
            sum_deaths= sum(Total_deaths),
            cases_per_million = sum(Total_cases)/sum(Pop_millions), 
            deaths_per_million = sum(Total_deaths)/sum(Pop_millions))

library(wesanderson)

ggplot(prop_per_continent, aes(x=Continent, y=cases_per_million, fill=Continent))+
  geom_bar(width = 1, stat = "identity")+ 
  scale_fill_manual(values =c(wes_palette("Moonrise3")[-c(3,4)],wes_palette("Cavalcanti1")[c(2,4,5)]))+
  theme_classic()+
  ylab("Infection rate (Cases per million)")+
  ggtitle("Infection rate per continent")
```

![](Appendix_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
ggplot(prop_per_continent, aes(x=Continent, y=deaths_per_million, fill=Continent))+
  geom_bar(width = 1, stat = "identity")+ 
  scale_fill_manual(values =c(wes_palette("Moonrise3")[-c(3,4)],wes_palette("Cavalcanti1")[c(2,4,5)]))+
  theme_classic()+
  ylab("Mortality rate (deaths per million)")+
  ggtitle("Mortality rate per continent")
```

![](Appendix_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
prop_per_country <- analysis_df |>
  group_by(Country,Continent)|>
  summarize(sum_cases= sum(Total_cases), 
            sum_deaths= sum(Total_deaths),
            cases_per_million = sum(Total_cases)/sum(Pop_millions), 
            deaths_per_million = sum(Total_deaths)/sum(Pop_millions)) |>
  arrange(Continent,deaths_per_million) 
```

    ## `summarise()` has grouped output by 'Country'. You can override using the `.groups` argument.

``` r
prop_per_country$ID <- 1:nrow(prop_per_country)



prop_per_country |>
ggplot(aes(x=reorder(Country, ID), y=deaths_per_million, fill=Continent))+
  geom_bar(width = 1, stat = "identity")+ 
  scale_fill_manual(values =c(wes_palette("Moonrise3")[-c(3,4)],wes_palette("Cavalcanti1")[c(2,4,5)]))+
  theme_classic()+
  xlab("Country")+
  ylab("Mortality rate (deaths per million)")+
  ggtitle("Mortality rate per continent")+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank())
```

![](Appendix_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
# Run the analysis: linear regression
mod <- lm(Total_deaths ~ Total_cases+ fully_vax_count+ Pop_millions+Percent_Over_65 + km2+ GDP_per_capita + Obesity_rate  + cvd_death_rate, analysis_df)
summary(mod)
```

    ## 
    ## Call:
    ## lm(formula = Total_deaths ~ Total_cases + fully_vax_count + Pop_millions + 
    ##     Percent_Over_65 + km2 + GDP_per_capita + Obesity_rate + cvd_death_rate, 
    ##     data = analysis_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -172230  -14512   -1472    6423  288409 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -8.170e+03  1.507e+04  -0.542  0.58859    
    ## Total_cases      1.104e-02  5.313e-04  20.781  < 2e-16 ***
    ## fully_vax_count -2.516e-04  1.932e-04  -1.303  0.19475    
    ## Pop_millions     2.479e+02  1.590e+02   1.559  0.12121    
    ## Percent_Over_65 -6.891e+02  7.905e+02  -0.872  0.38487    
    ## km2              2.959e+00  6.612e+00   0.448  0.65519    
    ## GDP_per_capita  -8.812e-01  2.874e-01  -3.066  0.00260 ** 
    ## Obesity_rate     1.667e+03  5.099e+02   3.269  0.00136 ** 
    ## cvd_death_rate  -1.878e+01  3.424e+01  -0.548  0.58426    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 48610 on 141 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.8401, Adjusted R-squared:  0.831 
    ## F-statistic: 92.57 on 8 and 141 DF,  p-value: < 2.2e-16

``` r
# check assumptions through diagnostic plots 
par(mfrow=c(2,2)) 
plot(mod)
```

![](Appendix_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# transformation to validate assumptions
mod_sqrt <- lm(sqrt(Total_deaths) ~ Total_cases+ fully_vax_count+ Pop_millions+ Percent_Over_65 + km2+ GDP_per_capita + Obesity_rate + cvd_death_rate, analysis_df)
summary(mod_sqrt)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(Total_deaths) ~ Total_cases + fully_vax_count + 
    ##     Pop_millions + Percent_Over_65 + km2 + GDP_per_capita + Obesity_rate + 
    ##     cvd_death_rate, data = analysis_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -164.08  -44.31  -14.63   27.15  331.06 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      3.037e+01  2.394e+01   1.269   0.2065    
    ## Total_cases      1.161e-05  8.437e-07  13.758  < 2e-16 ***
    ## fully_vax_count -6.651e-07  3.068e-07  -2.168   0.0318 *  
    ## Pop_millions     6.557e-01  2.525e-01   2.597   0.0104 *  
    ## Percent_Over_65  5.057e+00  1.255e+00   4.028 9.16e-05 ***
    ## km2              7.114e-03  1.050e-02   0.677   0.4992    
    ## GDP_per_capita  -2.219e-03  4.564e-04  -4.862 3.06e-06 ***
    ## Obesity_rate     3.758e+00  8.099e-01   4.641 7.86e-06 ***
    ## cvd_death_rate  -1.199e-01  5.438e-02  -2.205   0.0291 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 77.2 on 141 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.7678, Adjusted R-squared:  0.7546 
    ## F-statistic: 58.27 on 8 and 141 DF,  p-value: < 2.2e-16

``` r
# check assumptions
par(mfrow=c(2,2)) 
plot(mod_sqrt)
```

![](Appendix_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
# Remove outliers 
analysis_df_clean <- analysis_df[-c(1,96),]


# transformation to validate assumptions
mod_sqrt_clean <- lm(sqrt(Total_deaths) ~ Total_cases+ fully_vax_count+ Pop_millions+ Percent_Over_65 + km2+ GDP_per_capita + Obesity_rate + cvd_death_rate, analysis_df_clean)
summary(mod_sqrt_clean)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(Total_deaths) ~ Total_cases + fully_vax_count + 
    ##     Pop_millions + Percent_Over_65 + km2 + GDP_per_capita + Obesity_rate + 
    ##     cvd_death_rate, data = analysis_df_clean)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -167.64  -39.78  -16.85   25.81  335.83 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      2.338e+01  2.355e+01   0.993  0.32249    
    ## Total_cases      1.158e-05  7.991e-07  14.489  < 2e-16 ***
    ## fully_vax_count -7.149e-07  2.908e-07  -2.459  0.01518 *  
    ## Pop_millions     7.013e-01  2.395e-01   2.928  0.00398 ** 
    ## Percent_Over_65  5.132e+00  1.189e+00   4.315 3.01e-05 ***
    ## km2             -2.177e-02  2.952e-02  -0.738  0.46206    
    ## GDP_per_capita  -2.079e-03  4.345e-04  -4.783 4.34e-06 ***
    ## Obesity_rate     3.630e+00  7.678e-01   4.728 5.50e-06 ***
    ## cvd_death_rate  -9.282e-02  5.256e-02  -1.766  0.07961 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 73.11 on 139 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.7878, Adjusted R-squared:  0.7755 
    ## F-statistic: 64.49 on 8 and 139 DF,  p-value: < 2.2e-16

``` r
exp(summary(mod_sqrt_clean)$coef)
```

    ##                     Estimate   Std. Error      t value Pr(>|t|)
    ## (Intercept)     1.430004e+10 1.691009e+10 2.699001e+00 1.380565
    ## Total_cases     1.000012e+00 1.000001e+00 1.960390e+06 1.000000
    ## fully_vax_count 9.999993e-01 1.000000e+00 8.555041e-02 1.015291
    ## Pop_millions    2.016334e+00 1.270594e+00 1.869567e+01 1.003992
    ## Percent_Over_65 1.693052e+02 3.284836e+00 7.480109e+01 1.000030
    ## km2             9.784643e-01 1.029960e+00 4.783076e-01 1.587342
    ## GDP_per_capita  9.979237e-01 1.000435e+00 8.366675e-03 1.000004
    ## Obesity_rate    3.771773e+01 2.154963e+00 1.130835e+02 1.000005
    ## cvd_death_rate  9.113575e-01 1.053969e+00 1.710355e-01 1.082866

``` r
# check assumptions
par(mfrow=c(2,2)) 
plot(mod_sqrt_clean)
```

![](Appendix_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
# plot predicted vs observed values to determine the fit of the model
par(mfrow=c(1,1))
plot(predict(mod_sqrt, analysis_df_clean)^2, analysis_df_clean$Total_deaths )
abline(a=0, b=1)
```

![](Appendix_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

``` r
# R^2 
0.6933
```

    ## [1] 0.6933

# compare the accuracy of a linear model to a kNN machine learning algorithm to predict the total number of covid-19 deaths

``` r
# predictions with linear regression 
set.seed(1)
rmse_linear <-replicate(1000, {
    y <- analysis_df_clean$Total_deaths
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
    train_set <- analysis_df_clean |> slice(-test_index) 
    test_set <- analysis_df_clean |> slice(test_index) 
    fit <- lm(Total_deaths ~ Total_cases+ fully_vax_count+ Pop_millions+ Percent_Over_65 + km2+ GDP_per_capita + Obesity_rate + cvd_death_rate, data = train_set) 
    y_hat <- predict(fit, test_set) 
    sqrt(mean((y_hat - test_set$Total_deaths)^2, na.rm = T)) 
})

hist(rmse_linear)
```

![](Appendix_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
mean(rmse_linear)
```

    ## [1] 90109.47

``` r
sd(rmse_linear)
```

    ## [1] 65901.46

``` r
mean(analysis_df_clean$Total_deaths)
```

    ## [1] 40016.38

``` r
# predictions with knn machine learning 
set.seed(1)
rmse_knn <-replicate(1000, {
    y <- analysis_df$Total_deaths
    indexes = createDataPartition(analysis_df_clean$Total_deaths, times = 1, p = 0.5, list = F)
    train = analysis_df_clean[indexes, ] |> select(-c(Country, Total_deaths, Continent))
    test = analysis_df_clean[-indexes, ] |> select(-c(Country, Total_deaths, Continent))
    test_outcome <- analysis_df_clean[-indexes, ]|> pull(Total_deaths)
    train_outcome <- analysis_df_clean[indexes, ]  |> pull(Total_deaths)
    reg_results <- knn.reg(train[complete.cases(train), ], test[complete.cases(test), ], train_outcome, k = 3)
    sqrt(mean((reg_results$pred - test_outcome[complete.cases(test) ])^2, na.rm = T)) 
})

hist(rmse_knn)
```

![](Appendix_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
mean(rmse_knn)
```

    ## [1] 100232.4

``` r
sd(rmse_knn)
```

    ## [1] 31906.64

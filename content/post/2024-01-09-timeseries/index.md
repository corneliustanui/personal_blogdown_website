---
title: Modelling CPI Using ARIMA(p,d,q)
author: Cornelius Tanui
date: '2024-01-09'
slug: timeseries
categories: []
tags: []
---



### 1) Why CPI is Time Series

Consumer Price Index (CPI) is defined as a measure of the weighted aggregate change in retail prices paid by consumers for a given basket of goods and services.

The CPI is a statistical indicator of changes in consumer prices experienced by citizens of a country. It is a measure of comparing the cost of a fixed basket (234 items) of goods and services purchased by consumers over time. The CPI index reflects only pure price change and is a widely used consumer prices or the rate of inflation, which is the percentage change of CPI over one year (KNBS, 2010).

CPI is a univariate non-stationary time series variable which does not have significant seasonality. Univariate because it is a single variable that predicts (or correlates to) itself based on its own history (auto-regressive) as opposed to an ordinary regression variable that is predicted by one or more variables of different nature. Again, CPI is a time series variable since observed values are indexed in time. The fact that it naturally has an upward trend, possibly due to increasing population, changing lifestyles of people, and a generally growing economy means it is not stationary. Another important characteristic of CPI is periodicity. That is, observations are made more than once a year, in this case twelve times per year. Further, this shows that CPI are monthly realizations of onward (monotone increasing) right-continuous random variable in positive real line.

The data I will be using is obtained from here and runs from March 1962 to September 2020.

### 2) Loading the Required Packages

The following packages are required to perfom this modelling; -


```r
library(readxl)
library(tidyverse)
library(forecast)
library(tseries)
library(ggTimeSeries)
library(data.table)
library(here)
```

### 3) Loading Data


```r
# import data

CPI_Data <- read_excel(here("./Data/Historical CPI series 2020.xlsx"))

# view data
head(CPI_Data, 10)
```

```
## # A tibble: 10 × 3
##    `MONTHLY CPI FROM 1962` ...2  ...3               
##    <chr>                   <chr> <chr>              
##  1 <NA>                    <NA>  <NA>               
##  2 Year                    Month Index              
##  3 1962                    Mar   0.47503449374032269
##  4 <NA>                    Jun   0.48014239152247667
##  5 <NA>                    Sep   0.48525028930463066
##  6 <NA>                    Dec   0.49035818708678469
##  7 1963                    Mar   0.48525028930463066
##  8 <NA>                    Jun   0.49035818708678469
##  9 <NA>                    Sep   0.50057398265109265
## 10 <NA>                    Dec   0.50568188043324669
```

The column names are in row 2, this can be corrected as;


```r
# assign headers 
colnames(CPI_Data) <- CPI_Data[2, ]

# drop records when Month is blank or has the string "Month"
CPI_Data_clean <- CPI_Data %>% 
  filter(!is.na(Month) & Month != "Month")

# fill in blank years
CPI_Data_clean <- CPI_Data_clean %>% 
  fill(Year, .direction = "down") %>% 
  mutate(Index = as.numeric(Index),
         Year = as.numeric(Year))

# view data
head(CPI_Data_clean, 10)
```

```
## # A tibble: 10 × 3
##     Year Month Index
##    <dbl> <chr> <dbl>
##  1  1962 Mar   0.475
##  2  1962 Jun   0.480
##  3  1962 Sep   0.485
##  4  1962 Dec   0.490
##  5  1963 Mar   0.485
##  6  1963 Jun   0.490
##  7  1963 Sep   0.501
##  8  1963 Dec   0.506
##  9  1964 Mar   0.501
## 10  1964 Jun   0.501
```

### 4) Declaring CPI as Time Series


```r
CPI_Data_ts <- CPI_Data_clean %>% 
  select(Year, Month, Index) %>% 
  ts(start = c(1962, 3), 
        end = c(2020,9), 
        frequency = 1) 

class(CPI_Data_ts)   # Check the class, technically this is a univariate ts
```

```
## [1] "mts"    "ts"     "matrix" "array"
```

### 5) Visualisation of Trend


```r
CPI_Data_clean %>% 
  # filter(Year < 1984) %>% 
  # mutate(Index = round(as.numeric(Index))) %>% 
  ggplot(aes(x = Year, y = Index, colour = "Data")) + 
  geom_line(size = 0.5) +
  geom_smooth(aes(color = "Trend"),
              method = "gam", 
              size = 0.5,
              lty = 2) +
  scale_colour_manual(values = c("dodgerblue1", "maroon")) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 110, 15)) +

  labs(x = "Year",
       y = "CPI",
       title = "CPI & It's Trend, 1962 to 2020",
       subtitle = "The Trend is Approximated using GAM",
       caption = "Figure 1") +
  
  # custom theme
      theme(plot.title = element_text(face = "bold",
                                      hjust = 0.5,
                                      size = 13.5,
                                      family = "serif",
                                      color = "black"),
            plot.subtitle = element_text(face = "italic",
                                      hjust = 0.5,
                                      size = 9.5,
                                      family = "serif",
                                      color = "black"),
            axis.title = element_text(face = "bold",
                                      size = 11.5,
                                      family = "serif",
                                      color = "black"),
            axis.text = element_text(face = "plain",
                                     size = 10,
                                     family = "serif",
                                     color = "black"),
            strip.text.x = element_text(face = "bold",
                                        size = 13.5,
                                        family = "serif",
                                        color = "black"),
            axis.text.x = element_text(angle = 0, 
                                       hjust = 1),
            
            legend.position = "top",
            legend.title = element_blank(), 
            legend.text = element_text(face = "plain",
                                       size = 10,
                                       family = "serif",
                                       color = "black"),
            legend.key = element_rect(colour = NA, 
                                      fill = NA),
            
            legend.box = "horizontal",
            legend.key.size = unit(0.5, 'cm'),
            legend.spacing.x = unit(0.3, 'cm'),
            legend.background = element_blank(),
            
            plot.background = element_rect(fill = "white",
                                           color = "black", 
                                           linewidth = 1),
            panel.grid = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            panel.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(fill = NA)))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/trend-1.png" width="672" />

Figure 1 above shows upward rise in CPI over the years, the upwardness is indicative of trend, and therefore non-stationarity. This further implies that the CPI is constituted by multiplicative components; trend, seasonality, and random effects. Since it is multiplicative, applying log-transformation makes it easy to extract the random component by differencing, which also stabilizes variance and reduces seasonality. Even though the plot is ragged, seasonality is not immediately evident.

To extract seasonality, **Seasonal Trend Loess** decomposer (`tslm()`) is invoked. The decomposer, which works better than `decompose()`, is used for periodic time series, the periodicity (seasonal window) here is 4 months.

### 6) Decomposition of CPI


```r
CPI_Data <- ts(CPI_Data_clean$Index, start = c(1962, 3), end = c(2020, 9), frequency = 4)

decompose_df <- tslm(CPI_Data ~ trend + fourier(CPI_Data, 2))

trend <- coef(decompose_df)[1] + coef(decompose_df)['trend']*seq_along(CPI_Data)

components <- cbind(
  data = CPI_Data,
  trend = trend,
  season = CPI_Data - trend - residuals(decompose_df),
  remainder = residuals(decompose_df))

autoplot(components, facet = TRUE)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/components-1.png" width="672" />

The plot above shows the individual components of CPI;

- Trend,
- Seasonality, and
- Random error.
Here, seasonal fluctuations are more evident. The trend has been smoothed, i.e. it is devoid of seasonality and random error. To explore seasonality:-


```r
adjust_df <- CPI_Data - components[, 'season']

autoplot(CPI_Data, series = "Data") +
  autolayer(adjust_df, series = "Seasonally adjusted")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/seasonality_removed-1.png" width="672" />

It is evident that the rate of consumption exponentially rose in the early ’80s to early ’90s. The sinusoidal aspect of seasonality is revealed by the plot below. Consumption goes down in April by a seasonal effect of 0.25 and goes up around July by an effect of 0.65 every year.


```r
plot(window(components[,3],
            start = c(2016,1),
            end = c(2018,1)),
     main = "Seasonlity of CPI, 2016 to 2018",
     xlab = "Months",
     ylab = "Effects",
     col = "maroon",
     lwd = 2.5)

abline(h = seq(-.7,.7,.1),
       v = seq(2016, 2018, 1/12),
       col = "grey",
       lty = 2)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/focus_on_seasonality-1.png" width="672" />

### 7) Checking Normality Pictorially
#### i) Stem and Leaf Plot


```r
stem(CPI_Data, scale = 1, width = 105)
```

```
## 
##   The decimal point is at the |
## 
##    0 | 5555555555555555555556666666666666666666677777788899
##    1 | 00001111223344445556678889
##    2 | 00334566899999
##    3 | 0011111222333444444567777777778899
##    4 | 000000111233444556777799
##    5 | 001122223466788899
##    6 | 02455788
##    7 | 0012344466
##    8 | 347
##    9 | 4555679
##   10 | 08
##   11 | 19
##   12 | 3
##   13 | 35
##   14 | 08
##   15 | 014
##   16 | 244777899
##   17 | 00111112234445678
##   18 | 1238
##   19 | 0
```

The above stem-and-leaf diagram shows the skewed distribution that CPI assumes. The skewness is to right.

#### ii) Histogram
The histogram in the figure below is skewed to the right, confirming what we found with the stem-and-leaf diagram. This skewness implies the data is not normally distributed. Furthermore, the skewness calls for log-transformation of the data.


```r
ggplot(CPI_Data, aes(x = CPI_Data)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 3, 
                 colour = "black", 
                 fill = "maroon")+
  geom_density(alpha = .2, 
               fill = "dodgerblue1")+
  labs(title = "Distribution of CPI",
       x = "CPI", 
       y = "Density")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme_classic()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/distribution-1.png" width="672" />

Histogram of Log Transformed CPI


```r
LoggedCPI <- CPI_Data %>% log()  
  
ggplot(LoggedCPI, aes(x = LoggedCPI)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1, 
                 colour = "black", 
                 fill = "maroon")+
  geom_density(alpha = .2, 
               fill = "dodgerblue1")+
  labs(title = "Logged Distribution of CPI",
       x = "CPI", 
       y = "Density")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme_classic()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/logged_distribution-1.png" width="672" />

The histogram above with a superimposed kernel density plot for CPI log-transgeomed gives a vague hint of a bimodal distribution. This needs further transformation.

#### iii) Q-Q Plot
The plot below clearly illustrates how far the CPI is from the normal distribution. QQ Plots compare observed data to standardized theoretical normal data. The closeness of the plot to straight line indicates the closeness of the observed data to being normally distributed. For our case, the CPI deviates away at the tails to form an S-curve that defies normality.


```r
ggplot(CPI_Data, aes(sample=CPI_Data))+
  stat_qq(color = "maroon")+
  labs(title = "Q-Q Plot of CPI",
       x = "Theoretical", 
       y = "Sample")+
  theme_classic()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/qq_plot-1.png" width="672" />

### 8) Checking Normality Formally
#### i) Jarque Bera Test


```r
CPI_Data %>% jarque.bera.test() # From package "tseries"
```

```
## 
## 	Jarque Bera Test
## 
## data:  .
## X-squared = 57.318, df = 2, p-value = 3.577e-13
```

The hypothesis for this test is:-

- `\(H_0\)`: CPI is normally distributed,
- `\(H_a\)`: CPI is not normally distributed.

Here, the p-value is smaller than 0.05 and thus we reject `\(H_0\)`
 and conclude at 95% level of confidence that CPI is indeed significantly skewed.

### 9) Testing Stationarity Pictorially
#### a) Lagged Plots

The figure below shows plots of lags 1 to 20. The fact that there is a strong persistence of a straight line (autocorrelation) for lags above 20 excludes the dominance of an **MA(q)** in the series and gives a strong evidence for an **AR(p)**. This persistence is due to the strong correlation (0.987) at lag one. This property is also called long memory. However, the series cannot be simply an **AR(p)** since the order of such an **AR(p)** would be too large a number and _parsimony_ (idea of _Occult’s Razor_) would be violated. Therefore, the lagged plots give evidence of an **ARMA(p,q)** being the underlying best-case scenario.


```r
CPI_Data %>% gglagplot(lag = 20, 
                       seasonal = TRUE) + 
    scale_color_viridis_d(option = "viridis") +
  theme_light()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/Lagged_Plots-1.png" width="672" />

#### b) ACF and PACF

The figure below is a correlogram of ACF and ACF of CPI for the first 80 lags. The slowly decaying property in ACF further gives evidence of the series being non-stationary and hence an **AR(p)**. Because of the trend, the observations will usually be on the same side of the series’ global mean. The terms `\((X(t+k)−μ(X))(X(t)−μ(X))\)`
 are positive, for this reason, the ACF is positive as well and is close to one, i.e. 0.987 at lag 1. The smooth, linear decline in ACF also is an indicator of the insignificance of the seasonal effect in the series as opposed to a wave-like decline for the seasonally strong series. Notice that ACF comes to zero at lag about 78, this is too persistent an effect of long term memory.

The PACF cuts off immediately after the associated lag. This is typical of non-stationary processes.


```r
CPI_Data %>% ggAcf(lag.max = 100,
                   plot = TRUE) +
  labs(title = "CPI ACF")+
  theme_classic()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/ACF_and_PACF-1.png" width="672" />


```r
CPI_Data %>% ggPacf(lag.max = 100,
                    plot = TRUE) +
  labs(title = "CPI PACF")+
  theme_classic()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/CPI_PACF-1.png" width="672" />


### 10) Testing Stationarity Formally
#### a) Augmented Dickey-Fuller Test (tests for unit root)
The Augmented Dickey-Fuller (ADF) Test has the following hypothesis:

- `\(H_0\)`: Unit root exists (non-stationary)
- `\(H_1\)`: No unit root Exists (stationary)


```r
adf.test(CPI_Data)
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  CPI_Data
## Dickey-Fuller = -0.52375, Lag order = 6, p-value = 0.9802
## alternative hypothesis: stationary
```

We fail to reject `\(H_0\)` at 95% confidence level because `0.999 > 0.05`. The conclusion is that CPI for the period under study is not stationary. It follows from this conclusion that differencing is required to render the series stationary. As we have earlier deduced the underlying model to be **ARMA(p,q)**, we now have a strong evidence for **ARIMA (p,d,q)** where **d** is the number of times required to difference to achieve stationarity.

#### b) Ljung-Box Text (tests for white noise)
Testing stationarity by checking whether the data is white noise, Ljung-Box (1978) test was made use of. The test is based on whether the sample autocorrelation is equal to zero:

- `\(H_0; ρ = 0\)` White noise
- `\(H_1; ρ ≠ 0\)` Not whote noise
This test follows a Chi-square distribution. If the p-value is less than 0.05 at lag h degrees of freedom, #H_0$ is rejected and differencing is done.

The test statistic is given as; `\(Q(h) = n(n+2)∑^h _{k=1}\frac{ρk}{(n−k)}\)`


```r
Box.test(CPI_Data, type = "Ljung-Box")
```

```
## 
## 	Box-Ljung test
## 
## data:  CPI_Data
## X-squared = 234.89, df = 1, p-value < 2.2e-16
```

Clearly, the data is not white noise, we reject `\(H_0\)` and difference the data to obtain stationarity.

### 11) Automatic Fitting of ARIMA(p,d,q)

Having established that CPI follows an **ARIMA (p,d,q)** model, we go ahead to find the best fitting model using AIC and BIC as the tools for choosing the parsimonious fit. The package `forecast` with appropriate dependencies installed, we are saved the trouble of transfoming the data to stabilize variance and seasonality before manually differencing.


```r
CPIfit <- CPI_Data %>% auto.arima(approximation = FALSE,
                                  stepwise = FALSE,
                                  trace = FALSE) # trace = TRUE will print all the possible models
summary(CPIfit)
```

```
## Series: . 
## ARIMA(2,2,0)(2,0,1)[4] 
## 
## Coefficients:
##           ar1      ar2     sar1     sar2    sma1
##       -0.7461  -0.4174  -0.8993  -0.2587  0.8043
## s.e.   0.0610   0.0596   0.0983   0.0644  0.0831
## 
## sigma^2 = 0.01867:  log likelihood = 137.05
## AIC=-262.1   AICc=-261.74   BIC=-241.29
## 
## Training set error measures:
##                       ME      RMSE        MAE        MPE     MAPE      MASE
## Training set 0.002888028 0.1346216 0.06989758 0.08317552 1.286139 0.2097828
##                     ACF1
## Training set -0.03239534
```

Notice that by setting approximation = FALSE and stepwise = FALSE a more advanced/rigorous search for a model in conducted. The best model has the smallest BIC and AIC. The non-seasonal order (2,2,0) means the series has a moving average of order 0, an autoregressive component of order 2, and the ARMA is differenced twice (d = 2) to achieve stationarity. The mathematical model is the product of `MA` and `AR` components.

A pure **AR(p)** model is one where `\(Y_t\)` depends only on its own lags. That is, `\(Y_t\)` is a function of the ‘lags of `\(Y_t\)`’:

`$$Y_t=α+β_1Y_{t−1}+β_2Y_{t−2}+...β_pY_{t−p}+ϵ_1$$`
A pure **MA(q)** is is one that `\(Y_t\)` depends only on the lagged forecast errors and is given by:

`$$Y_t=α+ϵ_t+ϕ_1ϵ_{t−1}+ϕ_2ϵ_{t−2}+...ϕ_{t−q}$$`

Overall, ARIMA becomes:

`$$Y_t=α+β_1Y_t−1+β_2Y_{t−2}+...+β_pY_{t−p}ϵ_t+ϕ_1ϵ_{t−1}+ϕ_2ϵ_{t−2}+...+ϕ_qϵ_{t−q}$$`

Substituting the coefficients, this becomes:
`$$Y_t=α-0.7461Y_{t−1}-0.4174Y_{t−2}-0.8993ϵ_{t−1}-0.2587ϵ_{t−2}+0.8043ϵ_{t−3}$$`

#### a) Check Residuals for White Noise


```r
checkresiduals(CPIfit)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/whitenoise-1.png" width="672" />

```
## 
## 	Ljung-Box test
## 
## data:  Residuals from ARIMA(2,2,0)(2,0,1)[4]
## Q* = 5.0548, df = 3, p-value = 0.1678
## 
## Model df: 5.   Total lags used: 8
```

There are a few significant spikes in the ACF, and the model fails the Ljung-Box test. The model can still be used for forecasting, but the prediction intervals may not be accurate due to the correlated residuals. Sometimes it is just not possible to find a model that passes all of the tests.

#### b) Forecasting


```r
CPIforecast <- CPIfit %>% 
  forecast(h = 125,  # Forecast 5 (each with 4 seasons) years ahead of Sept 2020
           level = c(95, 99))

# summary(CPIforecast) # Print forecasts
```

#### c) Visualising Forecasts


```r
CPIforecast %>% autoplot() +theme_bw()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/Visuals-1.png" width="672" />

The point forcasts with both 95% CI and 99% CI are contained in `summary(CPIforecast)` for 33 years (each with 4 seasons) ahead of 2020, that is upto Q2 2053.

_End_

__Note__: This post was first published by the author in [RPubs](https://rpubs.com/Cornelius_Tanui/567471) in 2019.  

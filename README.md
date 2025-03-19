---
title: "S&P 500 Data Wrangling"
output: 
  html_document:
    toc: true
    toc_depth: 2
author: Antoniya Shivarova
date: "2025-03-19"
---

## Introduction

This document demonstrates the process of wrangling stock prices data.
The original data is sourced from the EIKON database and includes daily price data for individual stocks included in the S&P 500 index from 1990 to 2018. 
Moreover, we add the index price itself for the same observation period.
The data is cleaned and aggregated to monthly frequency for further analysis.

## Workspace Setup

We begin by setting up the workspace and loading the necessary libraries.



## Load and Clean Data

### Load Data

We load the daily price data from a CSV file. The file path is dynamically defined to ensure portability.


``` r
file_path <- file.path("data", "sp500_eikon_d_011990_122018.csv")
prices_sp500_d <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE) %>% as_tibble()
prices_sp500_d |> head()
```

```
## # A tibble: 6 × 1,024
##   Name    AMP.DEAD...MERGER.54…¹ AMR.DEAD...DELIST.09…² ASARCO.DEAD...MERGER…³ AT...T.DEAD...DELIST…⁴ ABBOTT.LABORATORIES ACME.CLEVELAND.DEAD.…⁵
##   <chr>                    <dbl>                  <dbl>                  <dbl>                  <dbl>               <dbl>                  <dbl>
## 1 01.01.…                  22.25                  12.26                  29.88                  28.97              3.8037                  10.88
## 2 02.01.…                  22.75                  12.63                  30                     29.61              3.8736                  11   
## 3 03.01.…                  22.56                  12.57                  31.5                   29.37              3.8876                  11   
## 4 04.01.…                  22.38                  12.41                  32.25                  28.89              3.8806                  11.38
## 5 05.01.…                  21.81                  12.6                   32.13                  28.41              3.8387                  11.5 
## 6 08.01.…                  22.25                  12.49                  32.13                  29.13              3.8387                  11.5 
## # ℹ abbreviated names: ¹​AMP.DEAD...MERGER.545615, ²​AMR.DEAD...DELIST.09.12.13, ³​ASARCO.DEAD...MERGER.18.11.99, ⁴​AT...T.DEAD...DELIST.21.11.05,
## #   ⁵​ACME.CLEVELAND.DEAD...MERGER.912941
## # ℹ 1,017 more variables: ADVANCED.MICRO.DEVICES <dbl>, AEROQUIP.VICKERS.DEAD...MERGER.903749 <dbl>, AETNA.DEAD...SEE.DS.255956 <dbl>,
## #   AHMANSON.HF.DEAD...MERGER.702406 <dbl>, AIR.PRDS...CHEMS. <dbl>, ALBERTO.CULVER.DEAD...DELIST.11.05.11 <dbl>,
## #   ALBERTSONS.DEAD...DELIST.07.07.06 <dbl>, ALCAN.DEAD...DELIST.16.11.07 <dbl>, ARCONIC <dbl>, ALEXANDER...ALEX..DEAD...MERGER.922817 <dbl>,
## #   AMAX.MERGER.INTO.951020 <dbl>, AMDAHL.DEAD...MERGER.133227 <dbl>, HESS <dbl>, AMER.CYANAMID.MERGER.WITH.906151 <dbl>, AMER.ELEC.PWR. <dbl>,
## #   AMERICAN.EXPRESS <dbl>, AMER.GENERAL.DEAD...MERGER.916305 <dbl>, WYETH.DEAD...DELIST.15.10.09 <dbl>, AMERICAN.INTL.GP. <dbl>, …
```

### Format Data
We rename the date column, convert it to a `Date` format, and filter the data to include only records from 1999 onwards 
to insure a period consistency and avoid any potential data quality issues, especially with older data. Moreover, we remove
columns with error flags from the EIKON database, labeled as `X.ERROR`.


``` r
prices_sp500_d <- prices_sp500_d %>%
  rename(Date = Name) %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  filter(Date >= as.Date("1999-01-01")) %>%
  select(-starts_with("X.ERROR"))
```

### Remove NYSE No-Trade Dates

We identify and remove rows corresponding to NYSE holidays to ensure the data only includes trading days.


``` r
nyse_holidays <- timeDate::holidayNYSE(as.numeric(unique(format(prices_sp500_d$Date, "%Y")))) %>%
  as.Date()

prices_sp500_d <- prices_sp500_d %>%
  filter(!Date %in% nyse_holidays)
```

### Handle Missing Values

We identify columns with missing values, remove columns with more than 10% missing values,
and fill the remaining missing values using the last observation carried forward using the `zoo` package.

First, let's count the missing values for each column and analyze the results.

``` r
# count missing values for each column
nas_n <- prices_sp500_d %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  unlist() %>%
  sort(decreasing = TRUE) %>%
  .[. != 0]

# analyze missing values
head(nas_n)
```

```
##                                         XYLEM                            MARATHON.PETROLEUM   MOTOROLA.MOBILITY.HDG..DEAD...MERGER.29026M 
##                                          3216                                          3139                                          3009 
##                                 QEP.RESOURCES             CAREFUSION.DEAD...DELIST.17.03.15 MEAD.JOHNSON.NUTRITION.DEAD...DELIST.15.06.17 
##                                          2880                                          2675                                          2542
```

``` r
tail(nas_n)
```

```
##                             CNX.RESOURCES PEPSI.BOTTLING.GP..DEAD...DELIST.11.03.10                          BOOKING.HOLDINGS 
##                                        81                                        60                                        59 
##   LIFE.TECHNOLOGIES.DEAD...ACQD.BY.906394             DELPHI.DEAD...DELIST.07.10.09                                    NVIDIA 
##                                        37                                        23                                        13
```

``` r
length(nas_n) / ncol(prices_sp500_d)
```

```
## [1] 0.09626719
```

``` r
barplot(nas_n / nrow(prices_sp500_d), names.arg = rep("", length(nas_n)), xlab = "")
```

![plot of chunk handle-missing-values](figure/handle-missing-values-1.png)

Next, we remove columns with more than 10% missing values and fill the remaining missing values using the last observation carried forward using the `zoo` package.

``` r
prices_sp500_d <- prices_sp500_d %>%
  select(-all_of(names(nas_n)[nas_n > nrow(prices_sp500_d) * 0.1])) %>%
  mutate(across(-1, ~ zoo::na.locf0(.)))
```

Finally, we check for and remove any remaining missing values, especially leading NAs that are not handled by `na.locf0`.

``` r
leading_nas_n <- sapply(prices_sp500_d, function(col) sum(cumprod(is.na(col))))
max(leading_nas_n)
```

```
## [1] 484
```

``` r
prices_sp500_d <- prices_sp500_d[-seq_len(max(leading_nas_n, na.rm = TRUE)), ]
```

### Handle Repeated Values

We identify and remove stocks with more than 10 repeated price values, as these are likely invalid data and could skew the analysis, 
especially the calculation of returns and their moments.

First, we check and analyze the repeated values.

``` r
rep_vals_len <- sapply(prices_sp500_d[, -1], function(col) {
  rle_out <- rle(col)$lengths
})
str(rep_vals_len[1:10])
```

```
## List of 10
##  $ AMP.DEAD...MERGER.545615             : int 4548
##  $ AMR.DEAD...DELIST.09.12.13           : int [1:3144] 1 1 1 1 1 1 1 1 1 1 ...
##  $ ASARCO.DEAD...MERGER.18.11.99        : int 4548
##  $ AT...T.DEAD...DELIST.21.11.05        : int [1:1227] 1 1 1 1 1 1 1 1 1 1 ...
##  $ ABBOTT.LABORATORIES                  : int [1:4497] 1 1 1 1 1 1 1 1 1 1 ...
##  $ ACME.CLEVELAND.DEAD...MERGER.912941  : int 4548
##  $ ADVANCED.MICRO.DEVICES               : int [1:4440] 1 1 1 1 1 1 1 1 1 1 ...
##  $ AEROQUIP.VICKERS.DEAD...MERGER.903749: int 4548
##  $ AETNA.DEAD...SEE.DS.255956           : int [1:8] 1 1 1 1 1 1 1 4541
##  $ AHMANSON.HF.DEAD...MERGER.702406     : int 4548
```

``` r
rep_vals_len_summary <- data.frame(
  Name = names(rep_vals_len),
  Length = sapply(rep_vals_len, length),
  Min = sapply(rep_vals_len, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA),
  Max = sapply(rep_vals_len, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA),
  Mean = sapply(rep_vals_len, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA)
)

ggplot(
  melt(rep_vals_len_summary, id.vars = "Name"),
  aes(x = Name, y = value, fill = variable)
) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(title = "Summary Statistics of List Entries", x = "List Element", y = "Value") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
```

![plot of chunk handle-repeated-values](figure/handle-repeated-values-1.png)

Then, we define a function `find_rep_vals` to identify columns with more than 10 repeated values. 
After a detailed investigation and quality testing, we remove stocks with more than 10 repeated prices. 

``` r
# function to find columns with more than maxrep repeated values
find_rep_vals <- function(df, maxrep = 10) {
  sapply(df, function(col) {
    if (!is.numeric(col)) {
      return(NA)
    }
    rle_out <- rle(col)
    max(rle_out$lengths, na.rm = TRUE) >= maxrep
  }) |> which()
}

# check out the stocks with more than 10 repeated prices
rep_vals_names <- names(find_rep_vals(prices_sp500_d, maxrep = 10))
rep_vals_names |> head()
```

```
## [1] "AMP.DEAD...MERGER.545615"              "AMR.DEAD...DELIST.09.12.13"            "ASARCO.DEAD...MERGER.18.11.99"        
## [4] "AT...T.DEAD...DELIST.21.11.05"         "ACME.CLEVELAND.DEAD...MERGER.912941"   "AEROQUIP.VICKERS.DEAD...MERGER.903749"
```

``` r
# proportion of stocks with more than 10 repeated prices
length(rep_vals_names) / ncol(prices_sp500_d)
```

```
## [1] 0.539916
```

``` r
# check how many are marked as DEAD, MERGER, or DELIST
dead_or_merger <- grep("DEAD|MERGER|DELIST", rep_vals_names, value = TRUE)
dead_or_merger |> head()
```

```
## [1] "AMP.DEAD...MERGER.545615"              "AMR.DEAD...DELIST.09.12.13"            "ASARCO.DEAD...MERGER.18.11.99"        
## [4] "AT...T.DEAD...DELIST.21.11.05"         "ACME.CLEVELAND.DEAD...MERGER.912941"   "AEROQUIP.VICKERS.DEAD...MERGER.903749"
```

``` r
length(dead_or_merger) / ncol(prices_sp500_d)
```

```
## [1] 0.519958
```

``` r
# check the rest
not_dead_or_merger <- rep_vals_names[!rep_vals_names %in% dead_or_merger]
prices_sp500_d[, not_dead_or_merger] |> head()
```

```
## # A tibble: 6 × 19
##   COMBUSTION.ENGR. CORROON...BLACK NEWMARKET GT.NTHN.NEKOOSA HARCOURT.BRACE   MCA   NCR NOXELL..B. PHILIPS.INDS. RAMADA SANTA.FE.PACIFIC
##              <dbl>           <dbl>     <dbl>           <dbl>          <dbl> <dbl> <dbl>      <dbl>         <dbl>  <dbl>            <dbl>
## 1            39.88            32.5    7.1875           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 2            39.88            32.5    7.1875           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 3            39.88            32.5    7.1875           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 4            39.88            32.5    7.1875           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 5            39.88            32.5    7.1875           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 6            39.88            32.5    7.1875           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## # ℹ 8 more variables: SQUARE.D <dbl>, TELE.COMMUNICATIONS.TCI.GROUP..A. <dbl>, TONKA <dbl>, USX <dbl>, UNION.PACIFIC.RESOURCES.GROUP <dbl>,
## #   MANOR.CARE <dbl>, AT.T.CAP.CORP.SR.PINES.8.25.... <dbl>, TWENTY.FIRST.CENTURY.FOX.CDI..B. <dbl>
```

``` r
prices_sp500_d[, not_dead_or_merger] |> tail()
```

```
## # A tibble: 6 × 19
##   COMBUSTION.ENGR. CORROON...BLACK NEWMARKET GT.NTHN.NEKOOSA HARCOURT.BRACE   MCA   NCR NOXELL..B. PHILIPS.INDS. RAMADA SANTA.FE.PACIFIC
##              <dbl>           <dbl>     <dbl>           <dbl>          <dbl> <dbl> <dbl>      <dbl>         <dbl>  <dbl>            <dbl>
## 1            39.88            32.5   383.690           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 2            39.88            32.5   378.910           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 3            39.88            32.5   396.79            65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 4            39.88            32.5   404.02            65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 5            39.88            32.5   405.930           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## 6            39.88            32.5   412.090           65.69           0.63 69.25   108      35.13         18.25    8.5            29.13
## # ℹ 8 more variables: SQUARE.D <dbl>, TELE.COMMUNICATIONS.TCI.GROUP..A. <dbl>, TONKA <dbl>, USX <dbl>, UNION.PACIFIC.RESOURCES.GROUP <dbl>,
## #   MANOR.CARE <dbl>, AT.T.CAP.CORP.SR.PINES.8.25.... <dbl>, TWENTY.FIRST.CENTURY.FOX.CDI..B. <dbl>
```

``` r
# remove stocks with more than 10 repeated prices
prices_sp500_d <- prices_sp500_d %>%
  select(-all_of(find_rep_vals(prices_sp500_d, maxrep = 10)))
```

### Turn into a dataframe

Finally, we turn the tibble into a time series dataframe and sort by column names.

``` r
prices_sp500_d <- prices_sp500_d %>%
  column_to_rownames("Date") %>%
  select(sort(names(.)))
```

## Download Daily Prices for the S&P 500 Index

We download the daily S&P 500 index prices for the same date range as the daily data and save them for later use.


``` r
prices_spx_d <- quantmod::getSymbols(
  "^GSPC",
  auto.assign = FALSE,
  from = rownames(prices_sp500_d)[1],
  to = as.character(as.Date(rownames(prices_sp500_d)[nrow(prices_sp500_d)]) + 1),
  periodicity = "daily"
)

prices_spx_d <- data.frame(date = as.Date(index(prices_spx_d)), coredata(prices_spx_d))
prices_spx_d <- prices_spx_d[, c("date", "GSPC.Adjusted")]
rownames(prices_spx_d) <- prices_spx_d$date
prices_spx_d <- prices_spx_d[, "GSPC.Adjusted", drop = FALSE]
```

### Check Overlapping Dates

We check if the date ranges of the S&P 500 index and the individual stock prices overlap and if not, we remove the non-overlapping dates.

``` r
dim(prices_sp500_d)
```

```
## [1] 4548  437
```

``` r
dim(prices_spx_d)
```

```
## [1] 4547    1
```

``` r
row_diff <- setdiff(rownames(prices_sp500_d), rownames(prices_spx_d))
row_diff
```

```
## [1] "2018-12-05"
```

``` r
row_diff_indx <- match(row_diff, rownames(prices_sp500_d))
prices_sp500_d[(row_diff_indx - 2):(row_diff_indx + 2), 1:10]
```

```
##            ABBOTT.LABORATORIES ABERCROMBIE...FITCH..A. ADOBE..NAS. ADTALEM.GLOBAL.EDUCATION ADVANCED.MICRO.DEVICES   AES AFLAC AGILENT.TECHS.
## 2018-12-03               74.27                   19.59      255.26                    56.37                  23.71 15.60 46.07          74.67
## 2018-12-04               71.50                   18.87      245.82                    54.63                  21.12 15.46 44.29          72.91
## 2018-12-05               71.50                   18.87      245.82                    54.63                  21.12 15.46 44.29          72.91
## 2018-12-06               71.54                   19.32      250.63                    54.50                  21.30 15.45 43.55          71.91
## 2018-12-07               69.95                   18.76      238.00                    52.76                  19.46 15.53 43.25          70.25
##            AIR.PRDS...CHEMS. AK.STEEL.HLDG.
## 2018-12-03            166.62           3.26
## 2018-12-04            163.92           3.09
## 2018-12-05            163.92           3.09
## 2018-12-06            161.37           2.95
## 2018-12-07            157.16           2.84
```

``` r
# it seems that the values are all equal to the previous day so we can delete them
prices_sp500_d <- prices_sp500_d[-row_diff_indx, ]
```

## Convert to Monthly Data

We aggregate the daily data into monthly data for both the S&P 500 index and individual stocks.


``` r
prices_sp500_m_xts <- xts::to.monthly(xts::xts(prices_sp500_d, order.by = as.Date(rownames(prices_sp500_d))),
  indexAt = "last", OHLC = FALSE
)
prices_sp500_m <- data.frame(Date = index(prices_sp500_m_xts), coredata(prices_sp500_m_xts))
rownames(prices_sp500_m) <- prices_sp500_m$Date
prices_sp500_m <- prices_sp500_m[, -1]

prices_spx_m_xts <- xts::to.monthly(
  xts::xts(prices_spx_d, order.by = as.Date(rownames(prices_spx_d))),
  indexAt = "last", OHLC = FALSE
)
prices_spx_m <- data.frame(Date = index(prices_spx_m_xts), coredata(prices_spx_m_xts))
rownames(prices_spx_m) <- prices_spx_m$Date
prices_spx_m <- prices_spx_m[, -1, drop = FALSE]
```

## Calculate Returns

We calculate the daily and monthly log returns for both the S&P 500 index and individual stocks.


``` r
# define function for calculating returns from dataframe
calc_rets <- function(prices) {
  log(prices / lag(prices)) %>% slice(-1)
}
rets_sp500_d <- calc_rets(prices_sp500_d)
rets_sp500_m <- calc_rets(prices_sp500_m)
rets_spx_d <- calc_rets(prices_spx_d)
rets_spx_m <- calc_rets(prices_spx_m)
```

## Save Data

Finally, we save all the data to an `.RData` file for future use and as a `.rda` file for including in packages.

``` r
save(prices_sp500_d, prices_sp500_m, rets_sp500_d, rets_sp500_m, prices_spx_d, prices_spx_m, rets_spx_d, rets_spx_m,
  file = paste0(
    "data/sp500_prices_d_m_",
    rownames(prices_sp500_d)[1], "_", rownames(prices_sp500_d)[nrow(prices_sp500_d)], ".RData"
  )
)
save(prices_sp500_d, prices_sp500_m, rets_sp500_d, rets_sp500_m, prices_spx_d, prices_spx_m, rets_spx_d, rets_spx_m,
  file = "data/sp500_data.rda"
)
```


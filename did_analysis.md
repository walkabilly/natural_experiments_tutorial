---
title: "Difference in Differences Analysis"
author: "Daniel Fuller"
date: "11/05/2019"
output:
      html_document:
        keep_md: true
---

### Hello

## Do Medical Marijuana Laws Increase Marijuana Use? Replication Study and Extension

Sam Harper, Erin C Strumpf, and Jay S Kaufman. Do Medical Marijuana Laws Increase Marijuana Use? Replication Study and Extension. Annals of Epidemiology. 2012;22(4):207–212. https://doi.org/10.1016/j.annepidem.2011.12.002

### Set up and read in data

```r
options(scipen = 1, digits=2)
library(tidyverse)
```

```
## Registered S3 methods overwritten by 'ggplot2':
##   method         from 
##   [.quosures     rlang
##   c.quosures     rlang
##   print.quosures rlang
```

```
## Registered S3 method overwritten by 'rvest':
##   method            from
##   read_xml.response xml2
```

```
## ── Attaching packages ─────────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.1.1       ✔ purrr   0.3.2  
## ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
## ✔ readr   1.3.1       ✔ forcats 0.4.0
```

```
## ── Conflicts ────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(car)
```

```
## Loading required package: carData
```

```
## 
## Attaching package: 'car'
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

```
## The following object is masked from 'package:purrr':
## 
##     some
```

```r
library(nlme)
```

```
## 
## Attaching package: 'nlme'
```

```
## The following object is masked from 'package:dplyr':
## 
##     collapse
```


```r
mml_data <- read_csv("mml_data.csv")
```

```
## Parsed with column specification:
## cols(
##   state = col_character(),
##   year = col_double(),
##   age = col_character(),
##   stname = col_character(),
##   pmu = col_double(),
##   pru = col_double(),
##   pmuse = col_double(),
##   pruse = col_double(),
##   year0 = col_character(),
##   mmlf = col_character(),
##   mmll = col_character(),
##   mml_1 = col_double(),
##   mml0 = col_double(),
##   mml1 = col_double(),
##   mml2 = col_double(),
##   treated = col_character(),
##   treatex = col_character(),
##   mmlwall = col_character(),
##   wall = col_character()
## )
```

### What is the impact of legalizing medical marijuana on recreational marijuana use?

Some states do and some states do not have medical marijuana laws (MML). As time passes more states have medical marijuana laws. We want to estimate the impact of implementing these laws on recreational marijuana use.

A key confounder we are trying to control is the fact that states that pass MMLs may differ from those that do not in ways that may also be correlated with marijuana use. For example, if states passing laws tend to have more liberal social norms about drug use, this could be mistaken as the effect of the policy.

This is a very similar problem to the residential self-selection problem in active living research. Refrased, we could say that neighbourhoods that implement different active living friend policies (i.e., parks, trafffic calming, cycling infrastructure) may differ from those that do not in ways that may also be correlated with active living.
Replication of past research

Wall, M., Poh, E., Cerdá, M., Keyes, K. M., Galea, S., Hasin, D. S. Adolescent marijuana use from 2002 to 2008: higher in states with medical marijuana laws, cause still unclear. Annals of Epidemiology. 2011;21(9):714–716.

Subset the data to only include 2002 to 2007 data

```r
mml_data <- subset(mml_data, mml_data$year >= 2002 & mml_data$year <= 2007)
```


```r
mml_data_12to17 <- subset(mml_data, mml_data$age == "12-17y")
```


```r
mean_mml_data <- mml_data %>%
                  group_by(age, treatex) %>%
                    summarize(
                      m_pmu = mean(pmu),
                      sd_pmu = sd(pmu)
                    )
head(mean_mml_data)
```

```
## # A tibble: 6 x 4
## # Groups:   age [3]
##   age    treatex m_pmu sd_pmu
##   <chr>  <chr>   <dbl>  <dbl>
## 1 12-17y no       6.94   1.21
## 2 12-17y yes      8.68   1.67
## 3 12+y   no       5.75   1.11
## 4 12+y   yes      7.46   1.48
## 5 18-25y no      15.9    3.62
## 6 18-25y yes     20.1    4.17
```


```r
table(mml_data$state, mml_data$mmlf)
```

```
##                       
##                        no yes
##   Alabama              24   0
##   Alaska                0  24
##   Arizona              24   0
##   Arkansas             24   0
##   California            0  24
##   Colorado              0  24
##   Connecticut          24   0
##   Delaware             24   0
##   District of Columbia 24   0
##   Florida              24   0
##   Georgia              24   0
##   Hawaii                0  24
##   Idaho                24   0
##   Illinois             24   0
##   Indiana              24   0
##   Iowa                 24   0
##   Kansas               24   0
##   Kentucky             24   0
##   Louisiana            24   0
##   Maine                 0  24
##   Maryland             24   0
##   Massachusetts        24   0
##   Michigan             20   4
##   Minnesota            24   0
##   Mississippi          24   0
##   Missouri             24   0
##   Montana               4  20
##   Nebraska             24   0
##   Nevada                0  24
##   New Hampshire        24   0
##   New Jersey           24   0
##   New Mexico           16   8
##   New York             24   0
##   North Carolina       24   0
##   North Dakota         24   0
##   Ohio                 24   0
##   Oklahoma             24   0
##   Oregon                0  24
##   Pennsylvania         24   0
##   Rhode Island         12  12
##   South Carolina       24   0
##   South Dakota         24   0
##   Tennessee            24   0
##   Texas                24   0
##   Utah                 24   0
##   Vermont               4  20
##   Virginia             24   0
##   Washington            0  24
##   West Virginia        24   0
##   Wisconsin            24   0
##   Wyoming              24   0
```

### Regression equation

$$ Y_{st} = β_0 + β_1MML_{st} + γ_t + δ_s + ε_{st} $$ 

* $Y_{st}$ = Marijuana use in state s in year t
* $β_1MML_{st}$ = A dummy variable indicating whether or not a state had a MML in place in year t
* $γ_t$ = A fixed effect for each year
* $δ_s$ = A fixed effect for each state
* $ε_{st}$ = A state-year-specific error term.

Year fixed effects control for any secular trend affecting marijuana use that is common to all states (not constrained to be linear).

State fixed effects control for any time-invariant characteristics of states. States that passed laws are our treatment group, and we use states that did not pass laws as a control group to estimate the counterfactual trend that treatment states would have demonstrated, had we been able to observe them.

Under this specification, the effect of the law is identified by comparing within-state changes in marijuana use before and after the passage of a law in states passing laws to states whose law status does not change.

### Regression ages 12 to 17

```r
did_12_17 <-lm(pmu ~ factor(mmlf) + year0 + factor(state), data = mml_data_12to17,
                na.action=na.exclude)
summary(did_12_17)
```

```
## 
## Call:
## lm(formula = pmu ~ factor(mmlf) + year0 + factor(state), data = mml_data_12to17, 
##     na.action = na.exclude)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5749 -0.3538 -0.0177  0.3005  1.9493 
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                         7.0887     0.2584   27.43  < 2e-16 ***
## factor(mmlf)yes                    -0.5937     0.2689   -2.21  0.02816 *  
## year02003-4                        -0.4463     0.1199   -3.72  0.00024 ***
## year02004-5                        -1.0271     0.1199   -8.57  1.1e-15 ***
## year02005-6                        -1.4410     0.1205  -11.96  < 2e-16 ***
## year02006-7                        -1.5607     0.1213  -12.87  < 2e-16 ***
## year02007-8                        -1.5771     0.1223  -12.89  < 2e-16 ***
## factor(state)Alaska                 4.0220     0.4400    9.14  < 2e-16 ***
## factor(state)Arizona                1.2650     0.3482    3.63  0.00034 ***
## factor(state)Arkansas               1.3117     0.3482    3.77  0.00021 ***
## factor(state)California             1.8270     0.4400    4.15  4.5e-05 ***
## factor(state)Colorado               3.4437     0.4400    7.83  1.4e-13 ***
## factor(state)Connecticut            2.3267     0.3482    6.68  1.5e-10 ***
## factor(state)Delaware               1.4983     0.3482    4.30  2.4e-05 ***
## factor(state)District of Columbia   0.7600     0.3482    2.18  0.03002 *  
## factor(state)Florida                1.1617     0.3482    3.34  0.00098 ***
## factor(state)Georgia               -0.0383     0.3482   -0.11  0.91244    
## factor(state)Hawaii                 2.6120     0.4400    5.94  9.7e-09 ***
## factor(state)Idaho                  0.6600     0.3482    1.90  0.05922 .  
## factor(state)Illinois               0.4833     0.3482    1.39  0.16641    
## factor(state)Indiana                1.0200     0.3482    2.93  0.00372 ** 
## factor(state)Iowa                   0.0200     0.3482    0.06  0.95425    
## factor(state)Kansas                 0.8467     0.3482    2.43  0.01575 *  
## factor(state)Kentucky               1.3717     0.3482    3.94  0.00011 ***
## factor(state)Louisiana             -0.1200     0.3482   -0.34  0.73070    
## factor(state)Maine                  5.2470     0.4400   11.93  < 2e-16 ***
## factor(state)Maryland               0.5600     0.3482    1.61  0.10909    
## factor(state)Massachusetts          3.2617     0.3482    9.37  < 2e-16 ***
## factor(state)Michigan               1.9739     0.3511    5.62  5.1e-08 ***
## factor(state)Minnesota              1.5950     0.3482    4.58  7.3e-06 ***
## factor(state)Mississippi           -0.7483     0.3482   -2.15  0.03261 *  
## factor(state)Missouri               0.9667     0.3482    2.78  0.00592 ** 
## factor(state)Montana                4.4547     0.4141   10.76  < 2e-16 ***
## factor(state)Nebraska               0.5333     0.3482    1.53  0.12692    
## factor(state)Nevada                 2.2537     0.4400    5.12  6.0e-07 ***
## factor(state)New Hampshire          3.4167     0.3482    9.81  < 2e-16 ***
## factor(state)New Jersey             0.3450     0.3482    0.99  0.32281    
## factor(state)New Mexico             3.4646     0.3596    9.63  < 2e-16 ***
## factor(state)New York               2.0217     0.3482    5.81  1.9e-08 ***
## factor(state)North Carolina         1.2100     0.3482    3.47  0.00060 ***
## factor(state)North Dakota          -0.1233     0.3482   -0.35  0.72352    
## factor(state)Ohio                   1.5967     0.3482    4.58  7.2e-06 ***
## factor(state)Oklahoma               0.8317     0.3482    2.39  0.01768 *  
## factor(state)Oregon                 3.3070     0.4400    7.52  1.0e-12 ***
## factor(state)Pennsylvania           0.9150     0.3482    2.63  0.00914 ** 
## factor(state)Rhode Island           4.4818     0.3733   12.01  < 2e-16 ***
## factor(state)South Carolina         0.1167     0.3482    0.34  0.73790    
## factor(state)South Dakota           0.8583     0.3482    2.46  0.01439 *  
## factor(state)Tennessee              0.0183     0.3482    0.05  0.95806    
## factor(state)Texas                 -0.2683     0.3482   -0.77  0.44172    
## factor(state)Utah                  -1.0383     0.3482   -2.98  0.00315 ** 
## factor(state)Vermont                5.5630     0.4141   13.43  < 2e-16 ***
## factor(state)Virginia               0.8233     0.3482    2.36  0.01884 *  
## factor(state)Washington             2.2087     0.4400    5.02  9.8e-07 ***
## factor(state)West Virginia          1.0567     0.3482    3.03  0.00267 ** 
## factor(state)Wisconsin              1.4367     0.3482    4.13  5.0e-05 ***
## factor(state)Wyoming                1.2350     0.3482    3.55  0.00047 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6 on 249 degrees of freedom
## Multiple R-squared:  0.882,	Adjusted R-squared:  0.856 
## F-statistic: 33.3 on 56 and 249 DF,  p-value: <2e-16
```

### Regression all ages controlling for age

```r
did_all_ages <-lm(pmu ~ factor(mmlf) + factor(year) + factor(state) + factor(age), data=mml_data,
                na.action=na.exclude)
summary(did_all_ages)
```

```
## 
## Call:
## lm(formula = pmu ~ factor(mmlf) + factor(year) + factor(state) + 
##     factor(age), data = mml_data, na.action = na.exclude)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.932 -0.758  0.040  0.694  8.323 
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                          5.793      0.345   16.81  < 2e-16 ***
## factor(mmlf)yes                      0.184      0.349    0.53  0.59785    
## factor(year)2003                    -0.391      0.156   -2.51  0.01215 *  
## factor(year)2004                    -0.632      0.156   -4.05  5.4e-05 ***
## factor(year)2005                    -0.640      0.157   -4.09  4.6e-05 ***
## factor(year)2006                    -0.669      0.158   -4.25  2.3e-05 ***
## factor(year)2007                    -0.644      0.159   -4.06  5.3e-05 ***
## factor(state)Alaska                  4.965      0.572    8.69  < 2e-16 ***
## factor(state)Arizona                 1.202      0.452    2.66  0.00801 ** 
## factor(state)Arkansas                1.418      0.452    3.13  0.00176 ** 
## factor(state)California              2.009      0.572    3.52  0.00046 ***
## factor(state)Colorado                4.224      0.572    7.39  2.8e-13 ***
## factor(state)Connecticut             3.949      0.452    8.73  < 2e-16 ***
## factor(state)Delaware                2.950      0.452    6.52  1.0e-10 ***
## factor(state)District of Columbia    4.226      0.452    9.34  < 2e-16 ***
## factor(state)Florida                 1.950      0.452    4.31  1.8e-05 ***
## factor(state)Georgia                 0.763      0.452    1.69  0.09179 .  
## factor(state)Hawaii                  1.971      0.572    3.45  0.00058 ***
## factor(state)Idaho                   0.526      0.452    1.16  0.24493    
## factor(state)Illinois                1.262      0.452    2.79  0.00536 ** 
## factor(state)Indiana                 1.501      0.452    3.32  0.00093 ***
## factor(state)Iowa                   -0.459      0.452   -1.02  0.31030    
## factor(state)Kansas                  0.782      0.452    1.73  0.08410 .  
## factor(state)Kentucky                1.474      0.452    3.26  0.00115 ** 
## factor(state)Louisiana               0.799      0.452    1.77  0.07770 .  
## factor(state)Maine                   5.589      0.572    9.78  < 2e-16 ***
## factor(state)Maryland                1.219      0.452    2.69  0.00716 ** 
## factor(state)Massachusetts           4.951      0.452   10.95  < 2e-16 ***
## factor(state)Michigan                2.888      0.456    6.33  3.4e-10 ***
## factor(state)Minnesota               2.697      0.452    5.96  3.3e-09 ***
## factor(state)Mississippi            -0.379      0.452   -0.84  0.40261    
## factor(state)Missouri                1.500      0.452    3.32  0.00094 ***
## factor(state)Montana                 4.771      0.538    8.87  < 2e-16 ***
## factor(state)Nebraska                0.795      0.452    1.76  0.07926 .  
## factor(state)Nevada                  1.968      0.572    3.44  0.00060 ***
## factor(state)New Hampshire           5.702      0.452   12.60  < 2e-16 ***
## factor(state)New Jersey              1.102      0.452    2.44  0.01499 *  
## factor(state)New Mexico              3.114      0.467    6.67  4.1e-11 ***
## factor(state)New York                3.595      0.452    7.95  4.5e-15 ***
## factor(state)North Carolina          1.517      0.452    3.35  0.00082 ***
## factor(state)North Dakota            0.106      0.452    0.23  0.81506    
## factor(state)Ohio                    2.074      0.452    4.59  5.0e-06 ***
## factor(state)Oklahoma                0.702      0.452    1.55  0.12115    
## factor(state)Oregon                  4.109      0.572    7.19  1.2e-12 ***
## factor(state)Pennsylvania            1.555      0.452    3.44  0.00061 ***
## factor(state)Rhode Island            6.892      0.485   14.21  < 2e-16 ***
## factor(state)South Carolina          0.889      0.452    1.96  0.04969 *  
## factor(state)South Dakota            0.681      0.452    1.51  0.13258    
## factor(state)Tennessee               1.045      0.452    2.31  0.02101 *  
## factor(state)Texas                  -0.281      0.452   -0.62  0.53424    
## factor(state)Utah                   -1.176      0.452   -2.60  0.00946 ** 
## factor(state)Vermont                 6.974      0.538   12.96  < 2e-16 ***
## factor(state)Virginia                1.591      0.452    3.52  0.00045 ***
## factor(state)Washington              2.984      0.572    5.22  2.1e-07 ***
## factor(state)West Virginia           1.442      0.452    3.19  0.00147 ** 
## factor(state)Wisconsin               1.767      0.452    3.91  9.9e-05 ***
## factor(state)Wyoming                 1.782      0.452    3.94  8.6e-05 ***
## factor(age)12+y                     -1.198      0.127   -9.45  < 2e-16 ***
## factor(age)18-25y                    9.763      0.127   77.06  < 2e-16 ***
## factor(age)26+y                     -3.272      0.127  -25.83  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.6 on 1164 degrees of freedom
## Multiple R-squared:  0.925,	Adjusted R-squared:  0.921 
## F-statistic:  243 on 59 and 1164 DF,  p-value: <2e-16
```

Additional notes:

* An in depth understanding of the model specification is critical.
* There are lots of really good examples out there in the literature. Mostly these are in economics and some in epidemiology.


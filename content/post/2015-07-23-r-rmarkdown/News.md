---
title: "The Effects of Low Advertising Revenues on News Content and Prices"
author: "Deen Xu"
date: "2021-12-22"
output:
  html_document:
    df_print: paged
subtitle: Topic C
---

You can find the output of this report as a pdf file in this website https://1drv.ms/b/s!AkWfBn3mQ8o6pnSP1ThLkDQv70DI


```{r, include=FALSE}
library(kableExtra)
library(tidyverse)
library(haven)
library(ggplot2)
data1 <- read_dta("C:/Users/jxude/Desktop/STA304/Final/116438-V1/data/dta/Angelucci_Cage_AEJMicro_dataset.dta")
data2 <- read_dta("C:/Users/jxude/Desktop/STA304/Final/116438-V1/data/dta/Angelucci_Cage_AEJMicro_Descriptive_evidence_US.dta") 
data1_new = na.omit(data1 
                    %>% select(id_news, after_national, nb_journ, ln_ps_cst, ads_p4_cst, year) 
                    %>% mutate_at(vars(id_news, after_national), ~as.factor(.)) 
                    %>% mutate(year = as.integer(year)))
```
## Abstract
This paper will reproduce the paper 'Newspapers And Advertising Revenues' by using the same dataset about French daily newspapers and French television from 1960 and 1974 to compute difference in differences estimates of the different effects of low advertising revenues caused by introduction of advertising on television on local and national News content and prices, and local Newspaper will be control group, national newspapers will be treatment group. The result shows the effect on national newspapers is more significant than on local newspapers.

## Key Words
Economy, Newspaper Industry, Empirical Analysis, Difference in Difference


## Introduction

Due to major newspaper companies’ continuously decreasing in employment of journalists, smaller newsrooms, fewer investigative reporters, and increasing dependence on purchasing news from wire services, people have been concerning about if the newspaper industry can produce high quality news (Angelucci & Cagé, 2019). 

```{r,echo=FALSE}
n = "#69b3a2"
journ = "#CD5C5C"
data2 %>% ggplot(aes(x=year)) + 
  geom_line( aes(y=newspapers,colour="Newspapers' advertising revenues"), size=2) + 
  geom_line( aes(y=nb_journ/1.3,colour='Number of daily newspaper journalists'), size=2) + 
  scale_y_continuous(sec.axis = sec_axis(~.*1.3, name = "Number of daily newspaper journalists")) + 
  scale_colour_manual(values=c(n,journ))+ 
  labs(y = "Million US$", x = "Years", colour = element_blank()) +
  theme(legend.position = c(0.5, 0.2)) + 
  ggtitle('Figure 1. Newspaper Advertising Revenues (in dollars) and Number of Journalists in the United States') + 
  theme(plot.title = element_text(size = 8, face = "bold"))

```

The trends in Figure 1 shows that from 1980 to 2015 in the United States, the average number of journalists per newspaper decreased while US newspaper advertising revenues decreased. One fact that causes newspaper advertising revenues to decrease is that the television advertisement becomes more popular and competitive. Moreover, increasing use of the internet could also cause the decreasing average number of journalists per newspaper. This paper aims to find out the effect of low advertising revenues on newspapers caused by advertisements on television, which can help us disentangle other factors that affect newspaper's advertising revenues such as new technology like the smartphone.

To reproducing the paper Newspapers in Times of Low Advertising Revenues written by Angelucci and Cag Cagé, this paper will use difference in differences method to examine the effect of television advertisement on newspaper by using a dataset of French newspapers between 1960 and 1974 provided by Angelucci and Cag Cagé. In the Methodology part, I will introduce the data and model I use to analyze Then, I will show and give a summary about the result of my model in results and discussion parts.

## Methodology

The data I used is provided by Angelucci and Cagé, it includes annual data on local and national newspapers between 1960 and 1974. The dataset includes various variables about the newspaper such as its sale price, news hole, year published, and number of pages. But I choose some of them to fit my model. After my selection, it includes information about if the newspaper is both national and published after 1967, the number of journalists it published, its subscription price and listed advertisement price in Euros, and its publish year. 

```{r, echo=FALSE}
National = summary(na.omit(data1 %>% filter(national == 1) %>% select(nb_journ, ln_ps_cst, ads_p4_cst) %>% rename( 'Number of Journalists'=nb_journ ,  'Subscription Price' = ln_ps_cst ,  'Listed ad Price' = ads_p4_cst)))
kable(National, caption = "National Newspaper") %>% kable_styling(bootstrap_options = "striped",
                full_width = F)
Local = summary(na.omit(data1 %>% filter(national != 1) %>% select(nb_journ, ln_ps_cst, ads_p4_cst) %>% rename( 'Number of Journalists'=nb_journ ,  'Subscription Price' = ln_ps_cst ,  'Listed ad Price' = ads_p4_cst)))
kable(Local, caption = "Local Newspaper") %>% kable_styling(bootstrap_options = "striped",
                full_width = F)
```
The two tables above show the characteristics of national and local newspapers' number of journalists, subscription price and listed advertising price.


In 1967, the French government stated it would relax regulations on prohibiting television advertising for a long time, which means the newspaper's advertising revenue would go down after then. Also, since more people would choose publishing national advertising on television and national newspaper, the effect of the government announcement would have different effect on national and local newspapers.Then I want to use difference in differences method to find the different effect of reduced revenue caused by the government announcement on local and national newspaper content, sale price and advertising price. Next, I choose to use the logarithms of the dependent variable to fit linear regression models. The model is 
$$ y_{n,t} = \alpha + \beta_1(D_{After} * D_{National}) + \lambda_n + \gamma_t + \epsilon_{n,t}$$
The n and t means newspapers code and years from 1960 to 1974. And $\lambda_n$ and $\gamma_t$ are fixed effects. This approach prevents cross-sectional variations from driving our results (Angelucci & Cagé, 2019). $\epsilon_{n,t}$ is a newspaper-year shock. 
The y is our response variable, I will choose the number of journalists, listed ad price and subscription price as response variables to find the effects on news content, ad price and sale price. The $D_{After} * D_{National}$ is the dummy variable, it is 1 when the newspaper was both national and published after 1967. Also, the coefficient $\beta_1$ represents the annual effect of the government announcement on national newspaper's response variables such as sale price compared to local newspapers. The $\alpha$ is the interpret variable.

## Results
```{r, include=FALSE }
journ = lm(log(nb_journ)~after_national + id_news + year, data = data1_new)
summary(journ)
ad_price = lm(log(ads_p4_cst)~after_national + id_news + year, data = data1_new)
summary(ad_price)
sub_price = lm(ln_ps_cst~after_national + id_news + year, data = data1_new)
summary(sub_price)
```

```{r, echo = FALSE}
y = c("Number of Journalists", "Listed Ad Price", "Subscription Price")
e = c('-0.151891***','-0.305949***','-0.03884.')
year = c('0.038164***','0.049120***','0.04878***')
r2 = c('0.9826','0.8777','0.8057')
info = tibble("Response variable"= y, 'Estimator of after national' = e, 'Estimator of year' = year, 'R2'= r2)

kable(info, caption = "Table 2   / *** p < 0.001; ** p < 0.01; * p < 0.05; . p > 0.05") %>% kable_styling(bootstrap_options = "striped",
                full_width = F)

```
The Table 2 summary shows the results of three linear regression models. From the table, we can see that compared to local newspapers, national newspaper's number of journalists, listed ad price and subscription price decrease by 15%, 31% and 4% after the government announcement respectively. 

Also, according to the model, we can estimate that for one unit  increase in year, and hold all other variables constant, the newspaper's number of journalists, listed ad price and subscription price increase by 0.038, 0.049 and 0.049 respectively. 

Finally, the r square values for these three models are 0.9826, 0.8777, and 0.8057.

## Discussion

The results of three linear relation models indicate that the effect of television advertising in France has a larger impact on national newspaper content and listed ad price compared to local newspaper, and similar impact on sale price.

According to the models, the p-value of the predicted variable for both the number of journalists and listed ad price model are all smaller than 0.05, then we can see there is strong evidence against the hypothesis that the estimators are zero. But the p-value of the estimator for subscription is bigger than 0.05, then we do not have strong evidence against the hypothesis that the estimator is zero. Therefore, we can say the effects on the number of journalists and listed ad prices are different for national and local newspapers, but have similar effect on sale price. And p-values for the year in three models are all smaller than 0.05, then we have strong evidence against the hypothesis that the estimators are zero in these three models.

Moreover, the r square values indicate all three response variables have strong positive linear relationship with predictor variables.

This analysis bases on a dataset about French from 1960 to 1974, which could be a outdated dataset, which we could not apply for nowadays situations since there are more influential variables such as technology improvement. To improve this, we can investigate the technology effect on newspaper's content and price. We can collect a new dataset from newspaper, television companies, and smart phone companies, which includes data about newspapers after the first smartphone published. Then we can use similar difference in differences method to analyze the effect of technology improvement on both local and national newspaper.

## References

Angelucci, C., & Cagé, J. (2019). Newspapers in Times of Low Advertising 
  Revenues. American Economic Journal: Microeconomics, 11(3), 319–364. 
  https://doi.org/10.1257/mic.20170306

Alexander, R. (2020, November). Difference in differences. Telling Stories With Data.    https://www.tellingstorieswithdata.com/

  
  

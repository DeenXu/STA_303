---
title: "STA 304 Problem Set 3"
author: "Deen Xu, Yinghan Zhang"
date: 2020-11-02

output:
  pdf_document: default
  html_document:
    df_print: paged


---
# Predict the Overall Popular Vote of the 2020 American Federal Election
```{r setup, include=FALSE}
library(tidyverse)

# Loading in the cleaned survey Data
survey_data <- read_csv("C:/Users/jxude/Desktop/STA304/PS3/outputs/survey_data.csv")

# Loading in the cleaned census Data
census_data <- read_csv("C:/Users/jxude/Desktop/STA304/PS3/outputs/census_data.csv")

survey_new = survey_data %>% filter(vote_2020 != "I am not sure/don't know")%>%
  filter(vote_2020 !=  'I would not vote')
census_new = census_data %>% filter(age >= 18)
```

# Model
```{r, include=FALSE}
library(lme4)

vote = glm(as.factor(vote_trump) ~ age , family = 'binomial', data = survey_new)
summary(vote)
```



We are using survey data and census data retrieved from Democracy Fund Voter Study 
Group and IPUMS USA to predict vote outcome of the 2020 American federal election.

We use logistic regression and post-stratification in this analysis. We first 
fit the survey data into logistic regression model by using participant's
age to predict if they will vote for Trump. Logistic regression model
is preformed because voting outcome is a binary response variable and we are
interested in the probability of voting for Donald Trump


Then we use the census data which contains information about number of survey
participants from 18 years old to 97 years old to post-stratify our previous model, where each 
age voting outcome are weighted by the proportion of the electorate
and then aggregated to the national level. Each age is considered as a cell,
there are 79 cells in total. This is where post-stratification is
used, it allow us to "re-weight" by the proportion of electorate in each cell,
which gives a weighted voting outcome by proportion of survey respondents in each 
age group.

In the following sub-sections I will describe the model specifics and the
post-stratification calculation.


## Model Specifics
We first fit the survey data into logistic regression model by using participant's
age to  model the proportion of voters who will vote for Donald Trump. Logistic 
regression model is preformed because voting outcome is a binary response variable
and we are interested in the probability of voting for Donald Trump.

We will only be using age as the predictor variable,
which is recorded as a numerical variable, to model the probability of voting
for Donald Trump, the logistic regression model we are using is:


$$ log(\frac{{p}}{1-{p}}) = \beta_0+\beta_1  x_{age} + \epsilon$$

Where ${p}$ represents the probability of survey respondents who
will vote for Donald Trump. Similarly, $\frac{{p}}{1-{p}}$ represents the 
odds of voting for Donald Trump. $\beta_0$ represents the intercept of the 
model, and is the odds of voting for Donald Trump at age 0, which is -0.879078. 
Additionally, $\beta_1$ is the slope parameter of the model, it represents change
in log odds for every unit change in age. So, for everyone one unit increase 
in age, we expect a 0.015086 increase in the log odds of voting for Donald Trump.

## Post-Stratification 
In order to estimate the proportion of voters who will vote for Donald Trump, we
perform a post-stratification analysis. We create cells by age. Based on the
logistic model we predict estimate log odds for each cell, and estimate the weighted
proportion of voters in each age cell by using population for each age cell divide 
by total population. Then we calculate total log odds for voting Trump by summing 
weighted log odds estimates. As a final step, we transformed total weighted log 
odds into probability($\hat{p}$) of voting for Trump by using $log(\frac{{p}}{1-{p}})$,
and get $\hat{y}^{ps}$.


We exclude participants with age from 1 to 17 because only Americans older than 18 years old have 
the right to vote. We also exclude survey participants with responses "I am not 
sure/don't know" and "I would not vote" for the accuracy of the analysis.

```{r, include=FALSE}

# Here I will perform the post-stratification calculation
census_new$estimate = vote %>% predict(newdata = census_new)
trump_vote_predcit = census_new %>% mutate(cell_prop = n/sum(n)) %>% mutate(age_predict_prop = estimate*cell_prop) %>% summarise(vote_predict = sum(age_predict_prop))

exp(trump_vote_predcit)/(1+exp(trump_vote_predcit))
```



# Results
Based off our post-stratification analysis of the proportion of voters in favor 
of Donald Trump modeled by logistic regression model, which accounted for age,
we have an estimate of $\hat{y}^{ps}$ of 0.4703041, representing the estimated 
proportion of voters in favor of voting for Donald Trump to be 0.4703041.



# Discussion
We performed logistic regression and post-stratification in this analysis to 
predict the Overall Popular Vote of the 2020 American Federal Election. Based off
the two techniques, we get an estimate of 0.4703041 indicating the estimated 
proportion of voters that are going to vote for Donald Trump is 0.4703041

In conclusion, through Nationscape Data Set and U.S. Census Data, we explored 
voting outcome for the 2020 American Federal Election by focusing on age. It was
observed that supports rate of Donald Trump is going to be 47.03%, and based off 
the estimated proportion of voters in favor of voting for Donald Trump being 0.4703041,
we predict that Joe Biden will win the 2020 American Federal Election.

## Weaknesses
The strength of this analysis is that it used weighted proportion of voters in
the process of estimating overall voting preferences, which improve on the accuracy
of this outcome. The limitation of this analysis is its shortcomings in data 
selection. The census data may not fit perfectly to the voting data, since for
every age, there contain cells that are not voting, and we did not remove
that portion of voters from the census data. Additionally, survey study such as
this one creates bias, respondents who choose to respond to this survey may be 
different from those who chose not to responds and respondents may not feel 
encouraged to provide accyrate answers.


## Next Steps
Some future steps might be to compare the actual election result with this 
prediction analysis, and perform more analysis such as a multilevel regression 
analysis or Bayesian logistic regression. A future survey can also be conducted
asking each respondents who they really vote for in the election.

# Appendix

Code and data supporting this analysis is available at
https://sta-304-assignment-2.netlify.app/2020/11/02/sta-304-problem-set-3/


# References
1. Nationscape Data Set. (2020, September). Democracy Fund Voter Study Group.                 
    https://www.voterstudygroup.org/publication/nationscape-data-set

2. U.S. CENSUS DATA FOR SOCIAL, ECONOMIC, AND HEALTH RESEARCH. IPUMS USA. 
    Retrieved November 2, 2020, from https://usa.ipums.org/usa/index.shtml
    
3. Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686,
    https://doi.org/10.21105/joss.01686


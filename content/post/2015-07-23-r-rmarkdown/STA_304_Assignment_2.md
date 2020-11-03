---
title: "STA 304 Problem Set 2"
author: "Deen Xu, Yinghan Zhang"
date: 2020-10-19

output:
  pdf_document: default
  html_document:
    df_print: paged


---
##Do Young and Highly Educated People Tend to Give Birth at an Older Age?
*Code and data supporting this analysis is available at* 
*https://sta-304-assignment-2.netlify.app/*
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(ggplot2)
library(tidyverse)

raw_data <- read_csv("gss.csv") 
# From gss_cleaning code provided by Professor Rohan
```

```{r, include=FALSE}
new_data = raw_data %>% select(age, age_at_first_birth, education) %>% filter(!is.na(age),!is.na(age_at_first_birth),!is.na(education))

age = new_data$age
age_at_first_birth = new_data$age_at_first_birth
education = new_data$education


plot(new_data$age, new_data$age_at_first_birth, 
                main = "Age at First Birth with Respect to Age", 
                xlab = "Age", 
                ylab = "Age at First Birth")


nms <- c("Above Bachelor", "Bachelor", "University Diploma below Bachelor", 
         "College", "Trade Diploma", 
         "High School Diploma", "Less than High School")

Edu_Box = new_data %>% 
  ggplot(aes(x=reorder(education,age_at_first_birth, FUN = median),
                        y=age_at_first_birth)) + 
  scale_x_discrete(labels=nms) +
  geom_boxplot() + 
  coord_flip() + 
  ylab("Education Level") + 
  xlab("Age at First Birth") + 
  ggtitle("Age at First Birth with Respect to Education Level (Figure 2)")


summary(lm(age_at_first_birth ~ as.numeric(age) + 
                 as.factor(education)))

```

## Abstract
This analysis is performed to investigate if age and education Level can be 
used to predict age at first Birth. Data used are from General Social Survey
(GSS) collected in 2017 on the Family. Multiple linear regression estimation 
is used to illustrate the relationship between them, the result shows there are 
negative linear relationship between age and age at first birth, and a higher 
education level leads to higher age at first birth. Therefore we can have a 
insight on factors that might affect the Canadian's perspective on what age to 
give a birth.



## Introduction
We are trying to see if older people tend to give birth at a younger age and 
with a lower education level. We chose age and education level as explanatory 
variables and age at first birth to be the response variable.

We used multiple linear regression estimation assuming there is a linear 
relationship between age, education level and age at first birth. The result 
below will prove the validity of the previous assumption and we will interpret 
the results. Other variants, weakness and potential next steps will be discussed 
at the end of this analysis.


## Data
Data of age at first birth, age and education level are retrieved from GSS 
collected in 2017. The target population is all non-institutionalized persons 
living in the ten provinces of Canada, and are aged above 15. We chose them 
because they are the most updated data on GSS.

There are 12533 observations in the data that have no null value for age at 
first birth, age, and education level.

According to GSS user's guide for this data, respondents has an unequal 
probability of being selected, thus data sample was not a good representative of 
the target population, results might be over or under-represented and biases are
involved.
```{r, echo=FALSE}
new_data %>% ggplot(aes(x = age)) + geom_histogram(bins = 50)
```
From the above graph, it is noticeable that there is a significant number of 
observations at the age of 80. This indicates it might be a biased sample which 
could potentially affect the result.




## Model
Assuming there is linear relationship between age, education level and age at
first birth. The methodology conducted in this analysis is regression estimation.
We are trying to estimate the model 
$y_{i}$ = ${\beta_0}$ + $\sum_{j=1}^7{\beta_j}x_{ji}$ + ${\epsilon_{i}}$,
where $y_{i}$ is age at first birth for ith individual selected, $x_{1i}$ and 
$x_{2i}$ to $x_{7i}$ are age and six education levels and a reference category for
the ith individual selected. 

${\beta_0}$ is the intercept parameter and ${\beta_j}$ being the slope parameters
for age and education level respectively. ${\epsilon_{i}}$ is the random error 
for ith individual selected.

In this particular model, education level is a categorical variable, also known
as the dummy variable, with bachelor education level being the reference category.
An alternative model could be Bayesian or regression estimation with finite 
population correction. Bayesian model are more flexible, whereas linear regression
has the assumption that there is a linear relationship between the predictor 
variables and response variable.


## Results
```{r, echo=FALSE}
plot(new_data$age, new_data$age_at_first_birth, 
                main = "Age at First Birth with Respect to Age (Figure 1)", 
                xlab = "Age", 
                ylab = "Age at First Birth")

```
From the above graph above (G1), we can find as age increases, more people choose
to give birth at a younger age. Note that the upward hypotenuse does not shows 
their relationship, it is formed because people who are at the age of 20 can 
only give birth before 20, people aged 30 can only give birth at an age younger
than age 30 and so on.
```{r, echo=FALSE}
Edu_Box
```
This graph demonstrates people with higher education level usually give birth at
an older age compare to people with a lower education level.

We denote Above Bachelor, Bachelor, University Diploma below Bachelor, College, 
Trade Diploma, High School Diploma, Less than High School as rank one to seven
respectively.

By using regression estimation, we have

$\hat{y}_{i}$ = 31.405 - 0.038$x_{age}$ + 1.004$x_{rank1}$ - 1.633$x_{rank3}$ 
- 2.441$x_{rank4}$ - 2.64$x_{rank5}$ - 3.711$x_{rank6}$ - 4.876$x_{rank7}$ 

Intercepts and parameters all have p-value less than 0.0001.


## Discussion
In this analysis, we use predictor variables age and education level, response
variable age at first birth, age and age at first birth are numeric variables,
education level is categorical and is treated as dummy variable with seven ranks
in the regression estimation. We chose multiple linear regression estimation
because there are two predictor variable associated.

By applying multiple linear regression estimation (lm function) on Rstudio, we
get estimates for parameters with extreme small p-value, which reject that
age, education level have no linear relationship with age at first birth. Thus 
we could use this function to predict age at first birth by age and education 
level. Thus we found age and education level might be factors that affect age
at first birth.
                            
Respondents in GSS survey has an unequal probability of being selected, 
thus the sample we used might not be a good representative of the target
population, results might be over or under-represented and biases are
involved.


# Weaknesses
Data used in this analysis is collected using stratified design with unequal 
fractions of strata when sampling, thus data might be over-represented or
under-represented.
Multiple linear regression model may have problem with over-fitting, model may
fail to fit additional data or not accurate when predicting future observations.


# Next Steps
We could rescale weights of sample to solve the unequal 
probabilities of selection. In addition, we could use Bayesian approach to 
estimate the parameter since it has more flexibility than linear regression
approach, we could get a better result.

We could conduct a follow-up survey to ask respondents about their income level
at the time they had their first child and conduct an subsequent study on assessing
factors that affect their first birth and contribute to our result.


## References

Alexander, R. Telling Stories With Data. Telling Stories With Data. Retrieved 
    October 18, 2020, from https://www.tellingstorieswithdata.com/

University of Toronto Data Library Service: Files for analysis and subsetting. 
    Retrieved October 18, 2020, from https://sda-artsci-utoronto-ca.myaccess.
    library.utoronto.ca/cgi-bin/sda/hsda?harcsda4+gss31

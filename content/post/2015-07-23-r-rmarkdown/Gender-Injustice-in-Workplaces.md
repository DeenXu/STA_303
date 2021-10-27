---
title: "Gender Injustice in Workplaces"
author: "Mingyang Li
Zecheng Wu
Deen Xu
Yinghan Zhang
"
date: 2021-05-01
---

You can find all the visualizations we mentioned in this report in this website https://deenxu.github.io/visualization/


Overview: 
In the human history, discrimination and inequality have always been a issue in several dimensions. For the past few decades, news about people being treated harshly due to their gender or race has brought more attention to our communities. These negative impressions are locked in our memory and become a stereotype. Even though many people believe that it is morally wrong to judge people based on their appearance, the stereotypes are unintentionally influencing their decisions, especially in the job market. Now we live in the 21th century, do people still treat others differently due to their gender? This is the answer that we are looking for today and we will be specifically looking at the gender injustice in Canada’s workplaces. Our goal for this project is to give Canadian citizens an idea of how our society is treating people differently by gender. We want more people to know whether these stereotypes will make companies pay people differently or we live in a society that everyone is being treated equally.

Data:
Different region and income :
Data link : https://open.canada.ca/data/en/dataset/04da95e7-30b4-4837-8aa3-67d89c8356a9
Data Dimension:
Year (2012 - 2018)
Province (Ontario, Quebec, B.C, Alberta, Manitoba, Saskatchewan, Nova Scotia, New Brunswick, Newfoundland and Labrador, PEI )
Sex (male, female)
Work Activity (number of full time workers and other workers)
Annual Median Income
 
This dataset shows the distribution of employment income of individeals by sex and work activity, Canada, provinces and selected census metropolitan areas. The first visualization used year, province, sex, and work activity to meausure the injustice in number of full time workers and other workers between sex within different provinces. This allows the users to compare which province is more likely to give male or female full-time jobs. The second visualization used year, province, sex and annual income to compare the injustice in annual income. This allows users to compare which gender will get treated better in different provinces. 

Average and median gender pay ratio in annual income at different position:
Data link : https://open.canada.ca/data/en/dataset/e15f2c98-b09a-4713-b957-aa440dc0f026
Data dimension: 
Year (2012 - 2018)
Sex (male, female)
Annual Median Income
10 National Occupational Classifications
Median Gender Pay Ratio

This dataset shows the median income and median gender pay ratio in annual wages, salaries and commissions, and is available by National Occupational Classification (NOC).  Figure 3.1 used median gender pay ratio and NOC to measure the job position that has the strongest prejudice towards a specific gender. This allows the users to locate median gender pay ratio extreme and identify the corresponding position. Figure 3.2 used median gender pay ratio and year to compare median gender pay ratio across years at different job positions and see if gender equality has improved. This allows the users to compare trends at the ten national occupational classifications. Figure 3.3 used median employment income and NOC to compare annual income between male and female from 2012 to 2018. This allows the users to discover if higher median employment income is associated with more severe gender injustice at the workplace.

Analytical Questions:
Which provinces and cities demonstrate better employee diversity in terms of gender? 
This question can be answered with Figure 1 where users choose a year and select all the provinces they want to compare. For any province that has the orange line that overlaps with the blue line are considered having a good employee diversity
How is gender inequality shown in income distribution?
Figure 2 is used to solve this problem where users choose a year and select all the provinces they want to compare. The solid circle demonstrates the income for full-time workers and the hollow circle demonstrates the income for other workers. If the circles with different colors are not close to each other, that means we have an inequality shown in the income distribution
Which job position has the strongest prejudice towards men or women? 
Figure 3.1 is used to answer this question where users choose a year to compare across job positions.The job position with the smallest median gender pay ratio suggests the strongest prejudice towards women.
Has gender equality in Canadian workplaces improved in recent years?
Figure 3.2 is used to visualize the change in gender injustice at workplace overtime by comparing median gender pay ratio. 
Is gender injustice associated with median employment income?
Figure 3.3 shows the median employment income in decreasing order. Larger gap between median employment income for females and males indicates a more serious gender injustice.

Visualization:
To visualize the difference between the number of male and female workers, we use a line chart in figure 1. The two contrasting color hues help viewers to identify the gender: blue for male and orange for female. By comparing the positions of the points on scale, viewers can learn whether men or women employees are the dominant population in each province. In addition, viewers can compare the values in different years where the visualization will display multiple graphs if the users decide to select more than one year to compare. Figure 1 also provides extra details to filter worker types.
We choose position on a common scale as the magnitude channel to illustrate the data in figure 2. This maintains the same contrasting color theme as figure 1: blue and orange. Thus, viewers can follow the story coherently from the first figure. Users can select the years and provinces they want to compare. This allows them to see the difference in median between different worker types and also their gender. 
Figure 1 and 2 are closely related to each other where the viewers can see the number of male workers are slightly higher than female workers. Next we look at figure 2, most of the blue circles are having a higher value compared to the orange circles. This means companies tend to treat males better in terms of employment rate as well as the annual income.
Figure 3.1 illustrates the income inequality between male and female employers. The magnitude channel in this graph is the length of gender pay ratio in annual income at the horizontal location, it is sorted in decreasing order so the position closer to baseline 1 means income is more equally distributed to men and women. Besides, NOCs that grow from the left side of the base value 1 indicate women are underpaid compared to men, we also assign two contrasting color hues to better distinguish positions having a median gender ratio below or above 1. We add the specific ratio beside each bin for the ease of reading the graph. We place this graph at the top because it gives a general idea of how each occupation does at gender equality in terms of income.
We also place figure 3.2 at the top of this visualization, this figure supports temporal reasoning. Rather than having the viewer click on every single year, the viewer is able to retain information on the change of gender injustice at different job positions from 2012 to 2018 all in this graph. We expect users to compare across years using this graph, and retain information with regard to job position by scrolling the timeline controller.
Figure 3.3 is an explanation for the above graph, the magnitude channel is the average employment income and the identity channel is gender and job position. It is also sorted in decreasing order by the average income between female and male workers. We hope readers can realize that not high paid positions are necessarily doing better at maintaining gender justice at the workplace, however differences in income are lessened between male and female workers as the average income increases. We use two contracting colors to better distinguish female workers from male workers, we use yellow for female and purple for male, this is to avoid the stereotype of pink representing female and blue representing male.
Figure 3.1, 3.2 and 3.3  are connected by identity channel job position. Notice both 3.1 and 3.3 place NOC in the y-axis so they are in the same orientation. If you click on a bin in the upper graph, it highlights information regarding this specific position. For example in Figure 3.1, if you click on management occupations, it would highlight this bin and in the other graphs, income and gender pay ratio for management occupations for both gender is highlighted as well. This interactive component helps the reader navigate through the visualization.

Reflection:
	We initially came up with the idea of using a pyramid graph in Figure 1. The issue is that viewers can’t really tell whether male or female has a higher number of workers. By switching to a line graph, views can tell male have a higher number directly by looking at the magnitude of the lines. One of the major issues we encountered is that our graphs for figure 1 and 2 couldn’t compare in different years. Finally, we discovered a solution and implemented this feature in both figure 1 and 2. There are also some minor fixes to the title, labels and additional features for convenience. At first we only have figure 3.1 and 3.3, however it can only show data for a specific year, so we decided to add a new line graph (figure 3.2) which can illustrate the trend of  median gender pay ratio from 2012 to 2018. A major challenge in our graphs for figure 3.1 is that we used 0 as our baseline value, which is misleading, since for every one dollar earned by men, women should get one dollar as well. After some research and group discussion, we managed to change the baseline value from 0 to 1, which is more clear to demonstrate the differences of every dollar earned by male and female. Also, we decided to switch the x-axis and y-axis of Figure 3.3, so that it has a better match with Figure 3.1. Lastly, we came out with a new question that can be answered by Figure 3.3 that is gender injustice associated with median employment income. 
Overall, limitations in this visualization include gender are classified only into male and females, including “other” in gender would be an asset; Also, data are not up-to-date, this visualization only includes data range from year 2012 to 2018, this visualization can be improved once more data are released. Figure 3.3 could be a bit richer, such as showing the variance and distribution of the annual employment income, but our data does not contain income data on individual employers, only median employment income is provided.

Team Assessment:
We split into groups of two and each group provides a few visualizations that can answer two of our analytical questions. One member in each group will do the data manipulation in R and the other person will do the visualization in tableau. We got our data from the government of Canada and imported them into R. The library we used includes dplyr which allows us to use R functions such as filter and select. After making the datasets clean, we then import them into tableau for final completion. The website we used to display our final project is tableau public, where users can interact with the models that we have designed. For the writing part, everyone has contributed to the part where they know the most. We have weekly meetings to gather everyone together and complete this project little by little. We have noticed the importance of teamwork and we really enjoyed working together. 
Mingyang Li created the line chart and the scatter plot in Figure 1 and 2. Zecheng Wu is the other member who helped manipulate the dataset and assisted Mingyang Li throughout the design process. Deen Xu and Yinghan Zhang worked together and created the bar charts in Figure 3.1 to 3.3. We have all participated in the discussion of choosing which kind of graph is the best to explain our analytical questions. In addition, we also considered the simplicity for users to understand our graphs and graphs that are interesting and not too common. 
Everyone is being responsible in the group and no one missed any meeting. We took the feedback from our mock design and tried to conduct a better solution for our final visualizations as well as our analysis. We share our findings and ideas together to produce our final report for this project.


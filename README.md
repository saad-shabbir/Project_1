---
title: "Project1"
author: "John, Saad"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
editor_options: 
  markdown: 
    wrap: 72
---

#Data Cleaning

```{r}
library(readr)     # For read_csv
library(dplyr)     # For data manipulation verbs
library(ggplot2)   # For plotting
library(lubridate) # For date manipulation
library(tidyr)     # For pivoting

age <- read_csv("../data/Data Science Project 1 Tables - age.csv")
edu <- read_csv("../data/Data Science Project 1 Tables - edu.csv")
income <- read_csv("../data/Data Science Project 1 Tables - income.csv")
race <- read_csv("../data/Data Science Project 1 Tables - race.csv")
urban <- read_csv("../data/Data Science Project 1 Tables - urban.csv")
vax <- read_csv("../data/Data Science Project 1 Tables - vax.csv")
poll <- read_csv("../data/va_gov_poll.csv")
likely_voter <- read_csv("../data/Data Science Project 1 Tables - likely_voter.csv")
shift <- read_csv("../data/Data Science Project 1 Tables - shift.csv")

edu <- edu |>
  mutate(count_pop = pct_pop * 8642000)

urban <- urban |>
  mutate(count_pop = pct_pop * 8642000)

poll <- poll |>
  mutate(Terry_count = case_when(
  vote_intent == "(60075) Terry McAuliffe" ~ 1,
  TRUE ~ 0  # Any other value in vote_intent
  )) |>
  mutate(Glenn_count = case_when(
  vote_intent == "(69739) Glenn Youngkin" ~ 1,
  TRUE ~ 0  # Any other value in vote_intent
  )) |>
  mutate(Princess_count = case_when(
  vote_intent == "(69738) Princess Blanding" ~ 1,
  TRUE ~ 0  # Any other value in vote_intent
  )) |>
  filter(vote_intent != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(likely_voter != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(party != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(presvote_2020 != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(biden_approve != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(age != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(parent != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(education != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(evangelical != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(race != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(urban != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(vax != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(income != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(gender != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)") |>
  filter(survey_mode != "(99) DON'T KNOW/SKIPPED ON WEB/REFUSED (VOL)")

Terry_count <- poll |>
  filter(vote_intent == "(60075) Terry McAuliffe") |>
  nrow() 
Terry_count
Glenn_count <- poll |>
  filter(vote_intent == "(69739) Glenn Youngkin") |>
  nrow() 
Glenn_count

```

For the age data, the data was collected in 2023. We assumed that the
age group in the original data set for 20-24 year olds was
representative for 18-24 year olds.
<https://www.neilsberg.com/insights/virginia-population-by-age/>

For the education data, the data is collected in 2022. Fields in orginal
data sheet "less than 9th grade", "9th to 12th grade, no diploma", and
"High school graduate" are combined under "(1) High school or less".
"Associate's degree" and "Bachelor's degree" are combined under "(3)
College graduate".
<https://www.statista.com/statistics/588136/educational-attainment-virginia/>

For income, the data was collected in 2023. We used 3,358,303 as total
household population in Virginia.
<https://www.ghrconnects.org/demographicdata?id=49&sectionId=936>

For the race data, we assumed the poll data classified people who were
American Indian, Alaskan Native, Native Hawaiian, Pacific Islander, and
Two more races as Other. We used the population cenus data of 2020
8,631,393 as the total count of population.
<https://www.census.gov/quickfacts/fact/table/VA/POP010220#POP010220>

For the urban data, the data was collected in 2023. We used 3,358,303 as
total household population in
Virginia.https://www.ghrconnects.org/demographicdata?id=49&sectionId=936

For the vaccination data, the data is collect as of Dec.30, 2021. "(1)
Yes" means the person has recieved at least one dose of COVID-19
vaccine, while "(2) No" means the person did not recieve any COVID-19
vaccine. The total population on 2021 was 8532087.
<https://usafacts.org/visualizations/covid-vaccine-tracker-states/state/virginia/>

For data processing purpose, we used 2021 VA population (8642000) as our
total VA population. Also, we ignored every row that contain "(99) DON'T
KNOW/SKIPPED ON WEB/REFUSED (VOL)", meaning the participants did not
want to answer a question.

We did not find data for gender, so we did not include it in our
analysis. We did not collect data for parent, biden_approve, and party,
as we did not think it will help with representing the VA population.

#Best estimate

```{r}
race_adjusted <- poll |> 
  left_join(race, by = "race") |> 
  group_by(race) |> 
  mutate(e_weight = count_pop / n()) |>
  summarise(Terry_Pct = sum(Terry_count * e_weight)/sum(e_weight),
            Glenn_Pct = sum(Glenn_count * e_weight)/sum(e_weight)
            ) 
race_result <- race |>
  left_join(race_adjusted, by = "race") |>
  summarize(Terry = sum(pct_pop*Terry_Pct),
            Glenn = sum(pct_pop*Glenn_Pct),
            )


edu_adjusted <- poll |> 
  left_join(edu, by = "education") |> 
  group_by(education) |> 
  mutate(e_weight = count_pop / n()) |>
  summarise(Terry_Pct = sum(Terry_count * e_weight)/sum(e_weight),
            Glenn_Pct = sum(Glenn_count * e_weight)/sum(e_weight),
            )
edu_result <- edu |>
  left_join(edu_adjusted, by = "education") |>
  summarize(Terry = sum(pct_pop*Terry_Pct),
            Glenn = sum(pct_pop*Glenn_Pct),
            )


age_adjusted <- poll |> 
  left_join(age, by = "age") |> 
  group_by(age) |> 
  mutate(e_weight = count_pop / n()) |>
  summarise(Terry_Pct = sum(Terry_count * e_weight)/sum(e_weight),
            Glenn_Pct = sum(Glenn_count * e_weight)/sum(e_weight),
            ) 

age_result <- age |>
  left_join(age_adjusted, by = "age") |>
  summarize(Terry = sum(pct_pop*Terry_Pct),
            Glenn = sum(pct_pop*Glenn_Pct),
            )

income_adjusted <- poll |> 
  left_join(income, by = "income") |> 
  group_by(income) |> 
  mutate(e_weight = count_pop / n()) |>
  summarise(Terry_Pct = sum(Terry_count * e_weight)/sum(e_weight),
            Glenn_Pct = sum(Glenn_count * e_weight)/sum(e_weight),
            ) 

income_result <- income |>
  left_join(income_adjusted, by = "income") |>
  summarize(Terry = sum(pct_pop*Terry_Pct),
            Glenn = sum(pct_pop*Glenn_Pct),
            )

vax_adjusted <- poll |> 
  left_join(vax, by = "vax") |> 
  group_by(vax) |> 
  mutate(e_weight = count_pop / n()) |>
  summarise(Terry_Pct = sum(Terry_count * e_weight)/sum(e_weight),
            Glenn_Pct = sum(Glenn_count * e_weight)/sum(e_weight),
            ) 

vax_result <- vax |>
  left_join(vax_adjusted, by = "vax") |>
  summarize(Terry = sum(pct_pop*Terry_Pct),
            Glenn = sum(pct_pop*Glenn_Pct),
            )

urban_adjusted <- poll |> 
  left_join(urban, by = "urban") |> 
  group_by(urban) |> 
  mutate(e_weight = count_pop / n()) |>
  summarise(Terry_Pct = sum(Terry_count * e_weight)/sum(e_weight),
            Glenn_Pct = sum(Glenn_count * e_weight)/sum(e_weight),
            )
urban_result <- urban |>
  left_join(urban_adjusted, by = "urban") |>
  summarize(Terry = sum(pct_pop * Terry_Pct),
            Glenn = sum(pct_pop * Glenn_Pct),
            )

combined_df <- rbind(race_result, edu_result, age_result, income_result, vax_result, urban_result)

estimation_1 <- colMeans(combined_df, na.rm = TRUE)
estimation_1
```

We ignored votes for Princess because there were significantly less
votes (116) when comparing him to Glen (1636) and Youngkin (1631). We
assumed that the following 6 demographic variable can effectively
represent the VA population: race, education, age, income, vaccinations,
and urban. We generated weights by dividing the total number of rows
with the number of Virginia population represented by the demographic
group variable in the poll dataset. We then combine the weights and the
vote counts for each candidate to generate the possibility of winning
under each variable. Finally, we generated the final estimation result
by averaging the 6 probabilities under each variable.

This was a good election forecast because it took into account for 6
different variables, and took the average from them. This approach can
better represent the VA population by weighing different demographic
variables.

#Second Estimate

```{r}
likely_voter_adjusted_poll <- poll |>
  left_join(likely_voter, by = "likely_voter") |>
  mutate(Terry_count = Terry_count * weight) |>
  mutate(Glenn_count = Glenn_count * weight) |>
  summarise(Terry_Pct = sum(Terry_count, na.rm = TRUE)/n(),
            Glenn_Pct = sum(Glenn_count, na.rm = TRUE)/n(),
            ) |>
  summarise(Terry = Terry_Pct/Glenn_Pct,
            Glenn = Glenn_Pct/Terry_Pct
            )
estimation_2 <- likely_voter_adjusted_poll * estimation_1
estimation_2

```

For the second estimate we assumed that if candidates responded 'I
already voted' or 'Definitely will vote', they were most likely to
actually cast a ballot. If they responded 'Definitely will not vote' or
'Probably will not vote', they were not likely to cast a ballot. So we
assigned weights based on their response as follow:

Definitely will vote = 0.8 Probably will vote = 0.6 Probably will not
vote = 0.4 Definitely will not vote = 0.2 I already voted = 1

Then we multiplied the weight of their response by the count of votes
for each candidate and divided by the total number of votes. We then
normalized the result and multiplied it by the mean values.

As we manually input weights for the responses and there is not enough
information to be certain whether or not they will cast a ballot based
on their response, we believe the best estimate is the most plausible.

#Third Estimate

```{r}
shift_adjusted_poll <- poll |>
  left_join(shift, by = c("party","vote_intent")) |>
  mutate(Terry_count = Terry_count * weight) |>
  mutate(Glenn_count = Glenn_count * weight) |>
  summarise(Terry_Pct = sum(Terry_count, na.rm = TRUE)/n(),
            Glenn_Pct = sum(Glenn_count, na.rm = TRUE)/n(),
            ) |>
  summarise(Terry = Terry_Pct/Glenn_Pct,
            Glenn = Glenn_Pct/Terry_Pct
            )
estimation_3 <- shift_adjusted_poll * estimation_1
estimation_3
```

For the third estimate, we assumed that if the voter's party was aligned
with their vote intention's candidate's party, they were less likely to
shift their vote (higher weights). If the voter's party was the opposite
of the candidate's party, they were more likely to change their vote
(lower weights). Since Glenn is from Republican party and Terry is from
Democrat party, we assigned weights accordingly:

Voter's party is Democrat and intent to vote is Glenn Youngkin = 0.4
Voter's party is Republican and intent to vote is Glenn Youngkin = 0.6
Voter's party is Neither and intent to vote is Glenn Youngkin = 0.5
Voter's party is Democrat and intent to vote is Terry McAuliffe = 0.6
Voter's party is Republican and intent to vote is Terry McAuliffe = 0.4

Then we multiplied the weight of their response by the count of votes
for each candidate and divided by the total number of votes. We then
normalized the result and multiplied it by the mean values.

As we manually input weights for the responses and there is not enough
information to be certain whether they will change their vote, we
believe the best estimate is the most plausible.

#Confidence

We are confident about our election forecast, as the poll data we relied
on was collected from 3493 registered voters, which was representitive
for the VA population. Also, while we thought the sample was randomly
collected, it was collected during October 27 to November 2, 2021, which
was just before the election. This made us more confident with our
estimation.

```{r}
bt_estimates <- rep(NA, 1000)

for(i in 1:1000){
  bt_estimates[i] <- poll |> 
    slice_sample(prop = 1, replace = TRUE) |> 
    summarize(Terry = mean(Terry_count)) |> 
    pull(Terry)
}

sd(bt_estimates)

for(i in 1:1000){
  bt_estimates[i] <- poll |> 
    slice_sample(prop = 1, replace = TRUE) |> 
    summarize(Glenn = mean(Glenn_count)) |> 
    pull(Glenn)
}
sd(bt_estimates)
```

Quantitative analysis: Due to the extremely low standard deviation
(below 0.009) after conducting bootstrapping sample 1000 times, the poll
was very likely to be random, which led to high reliability of our
estimation.

```{r}
poll |> 
  group_by(race) |> 
  summarise(count_sample = n()) |> 
  mutate(pct_sample = count_sample / sum(count_sample)) |> 
  left_join(race, by = "race") |> 
  ggplot() +
    geom_segment(aes(y = race, yend = race, x = pct_sample, xend = pct_pop), 
                 arrow = arrow(length = unit(0.30, "cm"))) +
    xlim(0, 1) +
    labs(x = "Sample to population proportion")

poll |> 
  group_by(education) |> 
  summarise(count_sample = n()) |> 
  mutate(pct_sample = count_sample / sum(count_sample)) |> 
  left_join(edu, by = "education") |> 
  ggplot() +
    geom_segment(aes(y = education, yend = education, x = pct_sample, xend = pct_pop), 
                 arrow = arrow(length = unit(0.30, "cm"))) +
    xlim(0, 1) +
    labs(x = "Sample to population proportion")

poll |> 
  group_by(age) |> 
  summarise(count_sample = n()) |> 
  mutate(pct_sample = count_sample / sum(count_sample)) |> 
  left_join(age, by = "age") |> 
  ggplot() +
    geom_segment(aes(y = age, yend = age, x = pct_sample, xend = pct_pop), 
                 arrow = arrow(length = unit(0.30, "cm"))) +
    xlim(0, 1) +
    labs(x = "Sample to population proportion")

poll |> 
  group_by(income) |> 
  summarise(count_sample = n()) |> 
  mutate(pct_sample = count_sample / sum(count_sample)) |> 
  left_join(income, by = "income") |> 
  ggplot() +
    geom_segment(aes(y = income, yend = income, x = pct_sample, xend = pct_pop), 
                 arrow = arrow(length = unit(0.30, "cm"))) +
    xlim(0, 1) +
    labs(x = "Sample to population proportion")

poll |> 
  group_by(urban) |> 
  summarise(count_sample = n()) |> 
  mutate(pct_sample = count_sample / sum(count_sample)) |> 
  left_join(urban, by = "urban") |> 
  ggplot() +
    geom_segment(aes(y = urban, yend = urban, x = pct_sample, xend = pct_pop), 
                 arrow = arrow(length = unit(0.30, "cm"))) +
    xlim(0, 1) +
    labs(x = "Sample to population proportion")

poll |> 
  group_by(vax) |> 
  summarise(count_sample = n()) |> 
  mutate(pct_sample = count_sample / sum(count_sample)) |> 
  left_join(vax, by = "vax") |> 
  ggplot() +
    geom_segment(aes(y = vax, yend = vax, x = pct_sample, xend = pct_pop), 
                 arrow = arrow(length = unit(0.30, "cm"))) +
    xlim(0, 1) +
    labs(x = "Sample to population proportion")


```

Qualitative Analysis: In our best guess, we made adjustment for 6
variables: race, income group, urban, age, vaccine, and education. While
the distribution of most attributes align with the actual population,
there were some attributes that deviated. The distribution of people who
are "high school or less" is less than that of the actual VA population,
while the distribution of people who have "some college degrees" are
more than that of the actual VA population. The distribution of people
who earn more than "\$100,000" per year is less than that of the actual
VA population, same for the distribution of people who are from "urban"
area. Finally, the distribution of people who are "65+" is more than
that of the actual VA population. Overall, since our best guess adjusted
for the above deviations and the data we relied on was inclusive and
reliable for the most part, we are confident with our best guess.

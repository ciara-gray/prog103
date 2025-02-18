library(marinecs100b)


# Review: write a function ------------------------------------------------

# P1 Describe succinctly what the following code does. This should be a
# high-level, one-sentence description, not a line-by-line breakdown.

site <- "Nuka_Pass"
season <- "Late winter"
n_cold <- sum(kefj_site == site &
                kefj_season == season &
                kefj_temperature <= -4 &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_cold <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_cold_per_day <- hours_cold / days_total
hours_cold_per_day

# Takes data from a particular site and season and returns the number of hours
# per day that the temperature is below -4

# P2 Let's turn that code chunk into a function. What would you call that
# function? How many parameters should it take and what would you call them?

# hr_cold_per_day
# Should have site, season

# P3 Write a function to encapsulate the code chunk above. Check that it
# contains all five parts of a function.

hr_cold_per_day <- function(site, season) {
  n_cold <- sum(kefj_site == site &
                  kefj_season == season &
                  kefj_temperature <= -4 &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_cold <- n_cold * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  hours_cold_per_day <- hours_cold / days_total
  return(hours_cold_per_day)
}

hr_cold_per_day("Nuka_Pass", "Late winter")

# Make an extreme choice --------------------------------------------------

# P4 Fill in the code below to create a logical vector indicating extreme
# temperatures.

extreme_type <- "cold"
if (extreme_type == "cold") {
  is_extreme <- kefj_temperature <= -4
} else if (extreme_type == "hot") {
  is_extreme <- kefj_temperature >= 25
}

is_extreme

# P5 Copy-paste the code from P1 and edit it to incorporate the is_extreme
# vector into the extreme temperature exposure procedure.

site <- "Nuka_Pass"
season <- "Late winter"
extreme_type <- "hot"
if (extreme_type == "cold") {
  is_extreme <- kefj_temperature <= -4
} else if (extreme_type == "hot") {
  is_extreme <- kefj_temperature >= 25
}
n_extreme <- sum(kefj_site == site &
                kefj_season == season &
                is_extreme &
                kefj_exposure == "air" & is_extreme)
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_extreme <- n_extreme * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_extreme_per_day <- hours_extreme / days_total
hours_extreme_per_day


# P6 Copy-paste the function you wrote in P3 and edit it to add a parameter that
# lets you switch between extreme heat and cold exposure.

hr_extreme_per_day <- function(site, season, extreme_type) {
  if (extreme_type == "cold") {
    n_extreme <- sum(kefj_site == site &
                       kefj_season == season &
                       kefj_temperature <= -4 &
                       kefj_exposure == "air")
  } else if (extreme_type == "hot") {
    n_extreme <- sum(kefj_site == site &
                       kefj_season == season &
                       kefj_temperature >= 25 &
                       kefj_exposure == "air")
  }
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_extreme <- n_extreme * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  hours_extreme_per_day <- hours_extreme / days_total
  return(hours_extreme_per_day)
}

hr_extreme_per_day("Nuka_Pass", "Late winter", "cold")
hr_extreme_per_day("Nuka_Pass", "Summer", "hot")

# Season to taste ---------------------------------------------------------

# P7 What seasons are in the kefj dataset? What function would you use to
# identify them?

kefj_season

# Late winter Spring Summer Fall Early winter
# ran kefj_season

# P8 Fill in the blanks below to make a for loop that prints the extreme hot and
# cold exposure across seasons at site Aialik.

seasons <- c("Late winter", "Spring", "Summer", "Fall", "Early winter")
  for (season in seasons) {
    heat_exposure <- hr_extreme_per_day("Aialik", season, "hot")
    cold_exposure <- hr_extreme_per_day("Aialik", season, "cold")
    print(paste("Aialik", season, heat_exposure, cold_exposure))
}

# P9 Copy-paste your answer to P8 and add a nested for loop to iterate across
# sites as well as seasons.

seasons <- c("Late winter", "Spring", "Summer", "Fall", "Early winter")
sites <- c("Aialik", "Harris", "McCarty", "Nuka_Bay", "Nuka_Pass")
for (season in seasons) {
  for(site in sites) {
  heat_exposure <- hr_extreme_per_day(site, season, "hot")
  cold_exposure <- hr_extreme_per_day(site, season, "cold")
  print(paste(site, season, heat_exposure, cold_exposure))
  }
}

unique(kefj_site)

# P10 Examine your results from P9. You should find two outputs where both
# extreme heat and cold exposure were 0. What season were they in?

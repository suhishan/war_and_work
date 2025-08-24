# Plan of Action.

# [] Merge Household and Individual Data for NLFS 1
# [] Merge Household and Individual Data for NLFS 2


# Testing out left_merge

d1 %>% group_by(years_of_edu_all) %>% count()
d1 %>% filter(is.na(years_of_edu_all)) %>% group_by(age) %>% count()
d2 %>% group_by(dname) %>% count()
d1 %>% filter(total_hours == 0) %>% count()
d1 %>% count(is.na(selfemp_hours))
hist(d1$total_hours)
x <- tibble(
  a = rep(c(1, 2, 3, NA), 100)
)

x <-  x %>% mutate(
  yes = if_else(a !=2, 0, 1, missing = NA)
)

a <- "Suhishan"

str_sub(str_to_lower(a), start = 1, end = 4)

c <- cbind(x1 = c(1:3, NA), x2 = c(NA, 2, 3, NA))
c <- tibble(
  x1 = c(1:3, NA),
  x2 = c(NA, 2, 3, NA)
)

c %>% mutate(
  sum  = rowSums(select(., x1,x2), na.rm = T)
)


tibble(x = c(1, 2, 3, 4, 5)) %>% mutate(i = ifelse(x %in% c(1:4), 1, 0))

## For d2

d2 %>% group_by(q111) %>% count()
d1_select %>% group_by(brahmin_chhetri) %>% count()

d2 %>% filter(is.na(not_perma_resident)) %>% group_by(age) %>% count() %>% print(n = 100)
d2 %>% count(is.na(perma_resident))



d2 %>% group_by(work_hours_outside) %>% count() %>% print(n = 100)
d2 %>% count(is.na(q87))

d2 %>% group_by(q85) %>% count()


d2 %>% group_by(total_hours) %>% count()
d2 %>% filter(is.na(q30)) %>% group_by(age) %>% count() %>% print(n = 102)

d2 %>% count(is.na(q40))


#9721 NAs and out of them, 
# 8251 are children under 5
# Exactly 1200 are people over the age of 5.


# Educational Stuff

d2 %>% group_by(ever_school) %>% count()


# After limiting people to age sample 15-59, let's check for NAs and what not.

# Counting NAs
d1 %>% count(is.na(years_of_edu_all))
d1 %>% count(is.na(q20))

d1 %>% filter(is.na(years_of_edu_all)) %>% 
  group_by(age) %>% count()
# Tabulating Stuff
d1 %>% 
  group_by(can_write) %>% count() %>% print(n = 100)

d1 %>% 
  group_by(wage_hours) %>% count() %>% print(n = 100)



# Checking the properties of rowSums:
a <- tibble(
  col1 = c(1,2,3, NA, NA),
  col2 = c(4, 5, 6, 7, NA)
)

a %>% mutate(sum = rowSums( select (., col1, col2), na.rm = T))

d1 %>% count(currently_unemp == 1 & currently_underemp == 1)

d1 %>% count(can_read == 1 & can_write == 0)


# Let's now look at d2 i.e. NLFS 2
# Counting NAs
d2 %>% count(is.na(q28))
# Tabulating Stuff

d2 %>% group_by(can_read) %>% count() %>% print(n = 100)


d2 %>% filter(is.na(q28)) %>% group_by(age) %>% count() %>% print(n = 100)
d1 %>% count(is.na(ever_school))

d2 %>% group_by(age, q29) %>% count() %>% print(n = 150)


# Conflict Dataset.

glimpse(c)

d2 %>% group_by(district_abbrev) %>% count() %>% print(n = 100)

c %>% filter(where_prec %in% c(1)) %>% 
  select(where_coordinates, best_est) %>% print(n = 100)

c %>% filter(where_prec == 5) %>% select(adm_2)

c %>% group_by(adm_2) %>% count() %>% print(n = 100)

c %>% filter(year %in% c(2007, 2008)) %>% group_by(side_b) %>% count()
c %>% group_by(side_b) %>% count()

c %>% group_by(side_a, side_b) %>% count()
c %>% summarize(civ = sum(deaths_civilians),
                overall = sum(best_est),
                unknown = sum(deaths_unknown))
a = tibble(
  hello = c("h district", "i district", "j district")
)

a %>% mutate(
  hello2 = str_remove(hello, " district")
)

glimpse(c)


c %>% group_by(district_name) %>% 
  summarize(
    deaths_total = sum(best_est),
    deaths_side_a = sum(deaths_a),
    deaths_side_b = sum(deaths_b),
    deaths_total
  )
c %>% count(active_year)
c %>% filter(active_year ==FALSE) %>% View()

c %>% group_by(district_name) %>% 
  summarize(
    across(deaths_a:low_est, \(x) sum(x, na.rm = T))
  )
  

c %>% count(low_est)
c %>% count(district_name, year) %>% 
  pivot_wider(
    names_from = year,
    values_from = n,
    names_prefix = "deaths_",
    values_fill = 0
  ) %>% View()


c_collapsed_2 %>% count(is.na(popn_2001))

hist(c_small$cd_per_10000)
c_small$cd_per_10000

nlfs %>% count(district_abbrev) %>% print(n = 100)

d1 %>% count(district_name, district_abbrev) %>% print(n = 80)
d2 %>% count(district_name, district_abbrev) %>% print(n = 80)


# Notes on similarities and differences in district names:

# Conflict Dataset.

# Dhankuta - dhak, Sindhupalchowk - sinp, 
# Dadeldhura - dade, dolakha - dola
# Parvat - parb
# Terhathum - terh.

c_small_1 %>% summarize_all(~ sum(is.na(.))) %>% 
  pivot_longer(everything()) %>% print(n = 100)


c_merged %>% count(district_abbrev, poverty_rate, treatment_s)%>% 
  arrange(desc(poverty_rate)) %>% print(n=40)

c_small %>% group_by(treatment_s) %>% summarize(mean(pov_rate, na.rm = T))


# Sample size classification

c_merged %>% group_by(treatment_s, post) %>% count()



# IPW with different cut-off rates.

ipwdid(yname = "work_hours_outside", tname = "post", idname = "id",
             dname = "treatment_s",
             xformla = ~  age + hindu + brahmin_chhetri + 
             + urbrur + sex + years_of_edu_all + poverty_rate,
             data = c_merged, panel = FALSE, boot = F, nboot = 199,
       trim.level = 0.9)



c_merged_98 %>% 
  filter(pscore < quantile(pscore, 0.75)) %>% 
  group_by(treatment_s) %>% 
  count()

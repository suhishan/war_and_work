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


nlfs %>% group_by(district_abbrev, nlfs_year) %>% 
  summarize(
   across(
     .cols = -c(district_name, psu),
     .fns = list(
       mean = ~ round(mean(.x, na.rm = T), 4)
     ),
     .names = "{.col}"
   ),
   sample_w = n()
  ) %>% 
  ungroup() %>% View()

nlfs_grouped_c %>% summarize(
  across(everything(), ~ sum(is.na(.)))
) %>% pivot_longer(everything()) %>% 
  print(n = 100)

# War and Work District Level Analysis Rough

d %>% filter(post == 1) %>% 
  ggplot(aes(x = conflict_deaths, y = work_hours_outside))+
  geom_point()+
  theme_minimal()



#
a <- d %>% 
  select(nlfs_year, work_hours_outside, best_est, district_name) %>% 
  pivot_wider(names_from = nlfs_year, 
              values_from = work_hours_outside,
              names_prefix = "wh_") %>% 
  mutate(wh_diff = wh_2008 - wh_1998 ) 

summary(lm(wh_diff ~ best_est, data = a))

a %>% 
  ggplot(aes(x = best_est, y = wh_diff))+
  geom_point()



nlfs %>% count(district_abbrev, post)

dj %>% filter(is.na(best_est))

View(dj)


# Rukum Rolpa Pyuthan Baglung dataset.

c_merged_4 %>% count(district_abbrev, deaths_until_98, best_est,
                     norm_forest, pov_rate)

c_merged_4 %>% group_by(district_abbrev, nlfs_year) %>% 
  summarize(
    mean(work_hours_outside), mean(hindu), mean(sex), mean(years_of_edu_all)
  )


# Simple interaction OLS between Gulmi and Arghakhanchi

m1 <- lm(usually_emp ~ post + treatment_s + post*treatment_s, data = c_merged_3)
summary(m1)

# What is the actual difference in conflict deaths between gulmi and Argakhanchi

c_merged_3 %>% group_by(treatment_s, post) %>% summarize(mean(usually_emp))

# Summary Statistics for conflict deaths and conflict deaths per 10,000 people.

c_small %>% summarize(mean(best_est),sd(best_est), min(best_est), max(best_est))
c_small %>% summarize(mean(cd_per_10000),sd(cd_per_10000),
                      min(cd_per_10000), max(cd_per_10000))


# Coefficient Plots for the first three bernoulli variables.
model_1_1 <- model_1_out[1:3, ]
model_1_1$Outcome <- c("Usually Employed",
                         "Currently Employed",
                         "Currently Self Employed")


model_1_1 %>%
  ggplot(aes(x = as.numeric(ATT), y = as.factor(Outcome)))+
  geom_point(size = 3)+
  geom_errorbarh(aes(xmin = as.numeric(lci),
                     xmax = as.numeric(uci)),
                 height = 0.2,
                 col = "black",
                 linewidth = 1.1)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x = "ATT", title = "Doubly Robust DiD Estimates",
       subtitle = "The coefficient plot shows 95% confidence intervals.",
       y = "")+
  theme_minimal()


?stat_pointinterval


# Work Hours Coefficient Plots.

model_1_2 <- model_1_out[c(4, 6),]
model_1_2$Outcomes <- c("Work Hours", "Self Employment Hours")

model_1_2 %>% 
  ggplot(aes(x = as.numeric(ATT), y = Outcomes))+
  geom_point(size = 2)+
  geom_errorbarh(aes(xmin = as.numeric(lci), xmax = as.numeric(uci)),
                 height = 0.2, color = "black", linewidth = 1.1)+
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "ATT", y = "", 
       title = "Doubly Robust DiD Estimates",
       subtitle = "The coefficient plot shows 95% confidence intervals")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )


# Overall Plot (1)
overall_1 <- rbind(model_1_1, model_2_1)
overall_1 <- overall_1 %>% 
  mutate(
    migration = factor(rep(c(0,1), each = 3), labels = c("No", "Yes"))
  )


overall_1 %>% 
  ggplot(aes(x = as.numeric(ATT), y = Outcome))+
  geom_point(size = 2)+
  geom_errorbarh(aes(xmin = as.numeric(lci), xmax = as.numeric(uci),
                     color = migration),
                 height = 0.2, linewidth = 1.1)+
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "ATT", y = "", 
       title = "Doubly Robust DiD Estimates",
       subtitle = "The coefficient plot shows 95% confidence intervals")+
  scale_color_manual(
    values = c("No" = "red", "Yes" = "blue"), 
    name = "Controlled for Migration"
  )+
  theme_minimal(base_size = 16)





# Overall Plot (2)

overall_2 <- rbind(model_1_2, model_2_2)
overall_2 <- overall_2 %>% 
  mutate(
    migration = factor(rep(c(0,1), each = 2), labels = c("No", "Yes"))
  )


overall_2 %>% 
  ggplot(aes(x = as.numeric(ATT), y = Outcomes))+
  geom_point(size = 2)+
  geom_errorbarh(aes(xmin = as.numeric(lci), xmax = as.numeric(uci),
                     color = migration),
                 height = 0.2, linewidth = 1.1)+
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "ATT", y = "", 
       title = "Doubly Robust DiD Estimates",
       subtitle = "The coefficient plot shows 95% confidence intervals")+
  scale_color_manual(
    values = c("No" = "red", "Yes" = "blue"), 
    name = "Controlled for Migration"
  )+
  theme_minimal(base_size = 16)


## Group d1 summary statistics by season.

d1_select %>% group_by(season) %>% 
  summarize(mean(usually_emp))

# How much c_merged_3 is 0?

c_merged_3 %>% 
  filter(age >=21) %>% 
  mutate(is_zero = work_hours_outside == 0) %>% 
  count(is_zero) %>% 
  mutate(prop = n/sum(n))





# Playing around with log-normal
c_merged_3 <- c_merged_3 %>% mutate(
  wh = ifelse(work_hours_outside == 0, 0, work_hours_outside),
  log_wh = log1p(work_hours_outside),
  is_zero = work_hours_outside == 0
)

c_merged_3 %>% 
  ggplot(aes(x = log_wh))+
  geom_histogram(binwidth = 0.1)+
  theme_minimal()

model_log_1 <- brm(
  data = c_merged_3,
  family = hurdle_lognormal,
  bf( wh ~ 1,
      hu ~ 1),
  prior = c(
    prior(lognormal(3, 0.5), class = Intercept, lb = 0),
    prior(normal(0, 1), dpar = "hu", class = Intercept)
  ),
  iter = 2000, warmup = 1000, chains = 4, cores = 4,
  file = "fits/model_log_1"
)


model_log_2 <- brm(
  data = c_merged_3,
  family = hurdle_lognormal,
  bf(wh ~ 0 + group,
      hu ~ 0 + group),
  prior = c(
    prior(lognormal(3, 0.5), class = b, lb = 0),
    prior(normal(0, 1), dpar = "hu", class = b)
  ),
  iter = 2000, warmup = 1000, chains = 4, cores = 4,
  file = "fits/model_log_2"
)


tibble(x = c("Hello", "Meh", "Hello")) %>%
  mutate(
    f = grepl("Hello", x)
  )

a <- lm(work_hours_outside ~ 1 + post + treatment_s + post*treatment_s,
        data = c_merged_3)
summary(a)

post_log_2 %>% ggplot(aes(x = exp(b_group2)))+
  geom_density(adjust = 0.7)

post_log_2 %>%
  select(contains("b_group") | contains("sigma")) %>% 
  set_names("1998 Gulmi", "2008 Gulmi",
            "1998 Arghakhanchi", "2008 Arghakhanchi", "Sigma") %>% 
  pivot_longer(1:4) %>% 
  mutate(
    work_hours = exp(value + (Sigma^2)/2),
    treatment = factor(name, levels = names),
    low_conflict = ifelse( grepl("Gulmi", treatment), 
                           "Low Conflict","High Conflict")
  ) %>% 
   ggplot(aes(x = treatment, y = work_hours)) +
  stat_pointinterval(
    aes(color = low_conflict),
    .width = .95,
    linewidth = 5,
    point_size = 5
  ) +
  scale_color_manual(
    values = c("Low Conflict" = "seagreen", "High Conflict" = "red3"),
    name = ""
  )+
  labs( y = "", x = "", subtitle = "Work Hours")+
  theme_minimal(base_size = 18)+
            theme(panel.grid = element_blank(),
                  legend.position = "bottom")
  


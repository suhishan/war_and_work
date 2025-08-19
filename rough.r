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

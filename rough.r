# Plan of Action.

# [] Merge Household and Individual Data for NLFS 1
# [] Merge Household and Individual Data for NLFS 2


# Testing out left_merge

d1 %>% group_by(usually_active) %>% count()
d1 %>% filter(is.na(usually_emp)) %>% group_by(age) %>% count()
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

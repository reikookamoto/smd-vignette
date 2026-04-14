library(smd)
library(dplyr)

set.seed(1234)

x <- rnorm(500)
g <- rep(1:2, each = 250)

smd(x, g, std.error = TRUE)

df <- data.frame(x, g)
x_1 <- df |>
  dplyr::filter(g == 1) |>
  pull(x)
x_2 <- df |>
  dplyr::filter(g == 2) |>
  pull(x)
x_1_bar <- mean(x_1)
x_2_bar <- mean(x_2)
s_1 <- var(x_1)
s_2 <- var(x_2)
num <- x_1_bar - x_2_bar
denom <- sqrt((s_1 + s_2) / 2)
# standardize difference in means using square root of
# average of group-specific sample variances
d <- num / denom

# binary categorical baseline variable
a <- sample(c(0, 1), size = 500, replace = TRUE)
b <- rep(1:2, each = 250)
c <- data.frame(a, b)
p_1 <- c |>
  filter(b == 1) |>
  pull(a) |>
  mean()
p_2 <- c |>
  filter(b == 2) |>
  pull(a) |>
  mean()
numerator <- (p_1 - p_2)
denominator <- sqrt((p_1 * (1 - p_1) + p_2 * (1 - p_2)) / 2)
numerator / denominator
smd(a, b, std.error = TRUE)

# categorical variables
# think of categorical variable with k categories as a k-dimensional vector of proportions

blood_type <- sample(c("A", "B", "AB", "O"), size = 1000, replace = TRUE)
blood_type <- factor(blood_type)
group <- rep(1:2, each = 500)
df <- data.frame(blood_type, group)
treat_prop <- df |>
  dplyr::filter(group == 1) |>
  dplyr::count(blood_type) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::filter(blood_type != "O") |>
  dplyr::pull(prop)
control_prop <- df |>
  dplyr::filter(group == 2) |>
  dplyr::count(blood_type) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::filter(blood_type != "O") |>
  dplyr::pull(prop)

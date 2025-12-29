set.seed(123)


# Sample datasets -----------------------------

sample1 <- data.frame(
  Subgroup = 1:25,
  X1 = rnorm(25, 10, 0.3),
  X2 = rnorm(25, 10, 0.3),
  X3 = rnorm(25, 10, 0.3),
  X4 = rnorm(25, 10, 0.3),
  X5 = rnorm(25, 10, 0.3)
)

sample1[5, 2]  <- sample1[5, 2] + 3
sample1[12, 6] <- sample1[12, 6] - 2.2

sample2 <- data.frame(
  Subgroup = 1:20,
  X1 = seq(9.5, 10.8, length.out = 20) + rnorm(20, 0, 0.15),
  X2 = seq(9.6, 10.9, length.out = 20) + rnorm(20, 0, 0.15),
  X3 = seq(9.7, 11.0, length.out = 20) + rnorm(20, 0, 0.15),
  X4 = seq(9.6, 10.8, length.out = 20) + rnorm(20, 0, 0.15),
  X5 = seq(9.5, 10.7, length.out = 20) + rnorm(20, 0, 0.15)
)

sample3 <- data.frame(
  Subgroup = 1:20,
  X1 = rnorm(20, 10, 0.4),
  X2 = rnorm(20, 10, 0.4),
  X3 = rnorm(20, 10, 0.4),
  X4 = rnorm(20, 10, 0.4),
  X5 = rnorm(20, 10, 0.4)
)

sample_list <- list(
  "Sample 1: Out-of-control points" = sample1,
  "Sample 2: Trend pattern" = sample2,
  "Sample 3: Random variation" = sample3
)








set.seed(123)


# Sample datasets -----------------------------

sample1 <- data.frame(
  Subgroup = 1:15,
  X1 = rnorm(15, 10, 0.3),
  X2 = rnorm(15, 10, 0.3),
  X3 = rnorm(15, 10, 0.3),
  X4 = rnorm(15, 10, 0.3),
  X5 = rnorm(15, 10, 0.3)
)

sample1[5, 2]  <- sample1[5, 2] + 3
sample1[12, 6] <- sample1[12, 6] - 2.2

sample2 <- data.frame(
  Subgroup = 1:20,
  X1 = seq(9.5, 10.8, length.out = 20) + rnorm(20, 0, 0.15),
  X2 = seq(9.6, 10.9, length.out = 20) + rnorm(20, 0, 0.15),
  X3 = seq(9.7, 11.0, length.out = 20) + rnorm(20, 0, 0.15),
  X4 = seq(9.6, 10.8, length.out = 20) + rnorm(20, 0, 0.15),
  X5 = seq(9.5, 10.7, length.out = 20) + rnorm(20, 0, 0.15)
)

sample3 <- data.frame(
  Subgroup = 1:18,
  X1 = rnorm(18, 10, 0.4),
  X2 = rnorm(18, 10, 0.4),
  X3 = rnorm(18, 10, 0.4),
  X4 = rnorm(18, 10, 0.4),
  X5 = rnorm(18, 10, 0.4)
)

sample_list <- list(
  "Sample 1: Out-of-control points" = sample1,
  "Sample 2: Trend pattern" = sample2,
  "Sample 3: Random variation" = sample3
)

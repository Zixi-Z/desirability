# Load necessary packages
library(ggplot2)
library(dplyr)
library(gridExtra)

# Define the desirability function
desirability_central <- function(y_hat, L, T1, T2, U, s) {
  if (y_hat < L) {
    return(0)
  } else if (y_hat >= L & y_hat < T1) {
    return(((y_hat - L) / (T1 - L)) ^ s)
  } else if (T1 <= y_hat & y_hat <= T2) {
    return(1)
  } else if (y_hat > T2 & y_hat < U) {
    return(((y_hat - U) / (T2 - U)) ^ s)
  } else {
    return(0)
  }
}

# Create a dataset with the desirability values for different values of y_hat and s:
y_hat_values <- seq(0, 10, length.out = 1000)
s_values <- c(0.5, 1, 2)

# Since t is not used, we don't need to expand the grid with t or create an interaction term
df <- expand.grid(y_hat = y_hat_values, s = s_values)

# Define the constants L, T1, T2, U
L <- 2
T1 <- 4
T2 <- 6
U <- 8

# Apply the desirability function to the dataset
df$desirability <- mapply(desirability_central, df$y_hat, L, T1, T2, U, df$s)

# Plot the desirability function using ggplot2
plot1 <- ggplot(df,
                aes(
                  x = y_hat,
                  y = desirability,
                  color = factor(s)
                )) +
  geom_line(size = 1) +
  ggtitle("Central") +
  labs(
    x = expression(hat(y)[i](x)),
    y = "Desirability",
    color = "s"
  ) +
  scale_color_discrete("s") +
  scale_x_continuous(
    breaks = c(L, T1, T2, U),
    labels = c(expression(L[i]), expression(T1[i]), expression(T2[i]), expression(U[i]))
  ) +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 14))



# Define the desirability function
desirability_ends <- function(y_hat, L, T1, T2, U, s) {
  if (y_hat < L) {
    return(1)
  } else if (y_hat >= L & y_hat < T1) {
    return((( y_hat - T1) / (L - T1)) ^ s)
  } else if (T1 <= y_hat & y_hat <= T2) {
    return(0)
  } else if (y_hat > T2 & y_hat < U) {
    return(((y_hat - T2) / ( U - T2)) ^ s)
  } else {
    return(1)
  }
}

# Create a dataset with the desirability values for different values of y_hat and s:
y_hat_values <- seq(0, 10, length.out = 1000)
s_values <- c(0.5, 1, 2)

# Since t is not used, we don't need to expand the grid with t or create an interaction term
df <- expand.grid(y_hat = y_hat_values, s = s_values)

# Define the constants L, T1, T2, U
L <- 2
T1 <- 4
T2 <- 6
U <- 8

# Apply the desirability function to the dataset
df$desirability <- mapply(desirability_ends, df$y_hat, L, T1, T2, U, df$s)

# Plot the desirability function using ggplot2
plot2 <- ggplot(df,
                aes(
                  x = y_hat,
                  y = desirability,
                  color = factor(s)
                )) +
  geom_line(size = 1) +
  ggtitle("Ends") +
  labs(
    x = expression(hat(y)[i](x)),
    y = "Desirability",
    color = "s"
  ) +
  scale_color_discrete("s") +
  scale_x_continuous(
    breaks = c(L, T1, T2, U),
    labels = c(expression(L[i]), expression(T1[i]), expression(T2[i]), expression(U[i]))
  ) +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 14))



# Update the desirability function for maximization
desirability_max <- function(y_hat, L, U, s) {
  if (y_hat < L) {
    return(0)
  } else if (y_hat >= L & y_hat <= U) {
    return(((y_hat - L) / (U - L)) ^ s)
  } else {
    return(1)
  }
}

# Modify the dataset to include the desirability values for the new function
y_hat_values <- seq(0, 10, length.out = 1000)
s_values <- c(0.2, 1, 3)

df_max <- expand.grid(y_hat = y_hat_values, s = s_values)

L <- 2
U <- 8

df_max$desirability <-
  mapply(desirability_max, df_max$y_hat, L, U, df_max$s)

#Modify the ggplot
plot3 <-
  ggplot(df_max, aes(x = y_hat, y = desirability, color = factor(s))) +
  geom_line(size = 1)  +
  ggtitle("High")+
  labs(x = expression(hat(y)[i](x)),
       y = "Desirability",
       color = "s") +
  scale_color_discrete("s") +
  scale_x_continuous(breaks = c(L, U),
                     labels = c(expression(L[i]), expression(U[i]))) +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 14))


# Update the desirability function for minimization
desirability_min <- function(y_hat, L, U, t) {
  if (y_hat < L) {
    return(1)
  } else if (y_hat >= L & y_hat <= U) {
    return(((U - y_hat) / (U - L)) ^ t)
  } else {
    return(0)
  }
}

# Modify the dataset to include the desirability values for the new function
y_hat_values <- seq(0, 10, length.out = 1000)
t_values <- c(1, 0.2, 3)

df_min <- expand.grid(y_hat = y_hat_values, t = t_values)

L <- 2
U <- 8

df_min$desirability <-
  mapply(desirability_min, df_min$y_hat, L, U, df_min$t)

# Modify the ggplot
plot4 <-
  ggplot(df_min, aes(x = y_hat, y = desirability, color = factor(t))) +
  geom_line(size = 1) +
  ggtitle("Low")+
  labs(x = expression(hat(y)[i](x)),
       y = "Desirability",
       color = "t") +
  scale_color_discrete("t") +
  scale_x_continuous(breaks = c(L, U),
                     labels = c(expression(L[i]), expression(U[i]))) +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 14))

# Combine the three plots into a single column with three rows
grid.arrange(plot1, plot2, plot3,plot4, nrow = 2, ncol = 2)
```{r, include=FALSE}
# Importing the libraries that I am going to use for the analysis.
library(dplyr)
library(ggplot2)
library(knitr)

# Importing the Library and the data set I am going to use.
library(palmerpenguins)
data("penguins")
penguins <- na.omit(penguins)
```

```{r}
# Preparing my unique data set according to my student number.
my.student.number <- 200291237
set.seed(my.student.number)
my.penguins <- penguins[sample(nrow(penguins), 200), ]
```

```{r}
# Define Tableu10 color pallete.
tableau10 <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AB"
)
```

```{r}
# Summary of the dataset

summary(my.penguins)
```

```{r, fig.width=10,fig.height=5}
ggplot(my.penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = tableau10) +
  theme_minimal() +
  ggtitle("Population Distribution") +
  facet_wrap(~species, ncol = 1) +
  coord_flip()
  
```

```{r, fig.width=10,fig.height=5}
ggplot(my.penguins, aes(x = sex, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = tableau10) +
  theme_minimal() +
  ggtitle("Sex Distribution") +
  facet_wrap(~species, ncol = 1) +
  coord_flip()
```

```{r, fig.width=10,fig.height=5}
ggplot(my.penguins, aes(x = bill_depth_mm, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.8, binwidth = 0.5, color = "black") + 
  scale_fill_manual(values = tableau10) + 
  theme_minimal() +
  ggtitle("Bill Depth Distribution") +
  facet_grid(species ~ sex)
```

```{r, fig.width=10,fig.height=5}
ggplot(my.penguins, aes(x=bill_length_mm, y=flipper_length_mm)) + 
    geom_point(aes(color=species), alpha = 0.8) + facet_wrap(~island) + 
    scale_color_manual(values = tableau10) + 
    theme_minimal() +
    ggtitle("Flipper Length vs Bill Length")
```

```{r, fig.width=10,fig.height=5}
ggplot(my.penguins, aes(x=bill_length_mm, y=flipper_length_mm)) + 
    geom_point(aes(color=species), alpha = 0.8) + facet_wrap(~sex) + 
    scale_color_manual(values = tableau10) + 
    theme_minimal() +
    ggtitle("Flipper Length vs Bill Length")
```

```{r, fig.width=10,fig.height=5}
ggplot(my.penguins, aes(y=body_mass_g, x=species)) + 
    geom_boxplot(aes(color=species), alpha = 0.8) + facet_wrap(~sex) + 
    scale_color_manual(values = tableau10) + 
    theme_minimal() +
    ggtitle("Body Mass Distribution") + 
    stat_summary(fun=mean, geom="point", shape=3, size=2) 
```

```{r}
bartlett_result <- bartlett.test(body_mass_g ~ sex, data = my.penguins)

# Printing the results
print(bartlett_result)
```

```{r}
# Extracting the data that I am going to use for t-test
male_penguins <- my.penguins[my.penguins$sex == "male", "body_mass_g"]
female_penguins <- my.penguins[my.penguins$sex == "female", "body_mass_g"]
```

```{r}
# Performing a two-sample t-test
t_test_result <- t.test(male_penguins, female_penguins, var.equal = TRUE)

# Printing the results
print(t_test_result)
```

```{r}

adelie_data <- subset(my.penguins, species == "Adelie")

# Perform ANOVA to assess the impact of "island" on "body_mass_g" for Adelie penguins
anova_result_body_mass <- aov(body_mass_g ~ island, data = adelie_data)

# View the ANOVA table
summary(anova_result_body_mass)
```

```{r}
# Perform ANOVA to assess the impact of "island" on "flipper_length_mm"
anova_result_flipper_length <- aov(flipper_length_mm ~ island, data = adelie_data)

# View the ANOVA table
summary(anova_result_flipper_length)
```

```{r}
# Perform ANOVA to assess the impact of "island" on "flipper_length_mm"
anova_result_bill_length <- aov(bill_length_mm ~ island, data = adelie_data)

# View the ANOVA table
summary(anova_result_bill_length)
```

```{r}
# Select the "bill_length_mm" variable
bill_length_data <- my.penguins$bill_length_mm

# Define the log-likelihood function for the normal distribution
log_likelihood_normal <- function(params) {
  mu <- params[1]
  sigma <- params[2]
  
  log_likelihood <- sum(dnorm(bill_length_data, mean = mu, sd = sigma, log = TRUE))
  
  return(-log_likelihood)  # Negative log-likelihood for maximization
}

# Perform Maximum Likelihood Estimation (MLE)
initial_params <- c(mean(bill_length_data), sd(bill_length_data))
mle_result <- optim(initial_params, log_likelihood_normal, method = "L-BFGS-B")

# Extract estimated parameters
estimated_mean <- mle_result$par[1]
estimated_sd <- mle_result$par[2]

# Print the estimated parameters
cat("Estimated Mean:", estimated_mean, "\n")
cat("Estimated Standard Deviation:", estimated_sd, "\n")
```

```{r}
# Define the desired confidence level
confidence_level <- 0.95

# Calculate the standard error of the estimated mean
standard_error <- estimated_sd / sqrt(length(bill_length_data))

# Calculate the margin of error
margin_of_error <- qnorm((1 + confidence_level) / 2) * standard_error

# Calculate the lower and upper bounds of the confidence interval
lower_bound <- estimated_mean - margin_of_error
upper_bound <- estimated_mean + margin_of_error

# Display the confidence interval
cat("Confidence Interval for the estimated mean : [", lower_bound, ", ", upper_bound, "]\n")

```

```{r, fig.width=10,fig.height=4.5}
# Create a histogram with MLE and normal distribution curve 
ggplot(data = my.penguins, aes(x = bill_length_mm)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, fill = tableau10[1], color = "black", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = estimated_mean, sd = estimated_sd), linewidth = 1,  color = tableau10[3]) +
  labs(title = "Normal Distribution Histogram and MLE") +
  xlab("Bill Length (mm)") +
  ylab("Frequency") +
  geom_vline(xintercept = estimated_mean, linetype = "dashed", color = tableau10[3], linewidth = 1) +
  annotate("text", x = estimated_mean, y = 0.01,
           label = paste("MLE Mean = ", round(estimated_mean, 2)), vjust = 0, hjust = -0.01, color = "black")
```


```{r, fig.width=10,fig.height=5} 
# Create a QQ plot for the "bill length".
qqnorm(my.penguins$bill_length_mm, main = "Q-Q Plot for Bill Length")
qqline(my.penguins$bill_length_mm, col = "red")
```
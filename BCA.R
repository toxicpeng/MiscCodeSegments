### David Hall --- 20210829 --- davidross.hall@mail.utoronto.ca
## BCA protein concentration


# 1. packages  ----

library(tidyverse)
library(plater)
library(broom)

# 2. importing plate data ----

# Note the layout in the csv file

data <- plater::read_plate("data/20210821_BCA.csv") %>%
  drop_na(loading) # dropping well I didn't use



# 3. getting slope and intercept ----

# Because BSA stds and samples are in different buffers, will need to account for both. 

# std buffer background
BSA0 <- data %>%
  filter(conc == 0) %>%
  summarize(mean = mean(A562)) %>%
  pull(mean)

# correcting backgroundin stds & fitting
BSAstds <- data %>%
  filter(loading == "BSA" | conc > 0) %>%
  mutate(corA562 = A562 - BSA0) %>%
  group_by(loading) %>%
  nest() %>%
  mutate(fit = map(data, ~lm(corA562 ~ conc, data = .x)),
         tidied = map(fit, tidy), 
         glanced = map(fit, glance),
         augmented = map(fit, augment))

BSAaugment <- BSAstds %>%
  unnest(augmented)

  
a <- ggplot(BSAaugment, aes( x = conc, y = corA562)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        parse = TRUE, size = 3) +
  ylim(c(0, 1.1))
  
a  
## 3.2 getting slope and intercept ----
  

BSAtidied <- BSAstds %>%
  unnest(tidied)

intercept <- as.numeric(BSAtidied[1,5])
slope <- as.numeric(BSAtidied[2,5])


# 4. calculating concentrations ----
# need to correct bacground of sample buffer. 

sampBuf <- data %>%
  filter(type == "buffer") %>%
  summarize(mean = mean(A562)) %>%
  pull(mean)

# correcting backgroundin stds & fitting
samples <- data %>%
  filter(str_detect(loading, "DH5")) %>%
  mutate(corA562 = A562 - sampBuf) %>%
  mutate(predConc = (corA562 - intercept)/slope) 

results <-  samples %>%
  mutate(corConc = predConc * dilution) %>%
  group_by(type, loading) %>%
  summarise(meanProt = mean(corConc),
            sd = sd(corConc))

b <- ggplot(samples,
       aes(x = type, y = corA562, colour = loading))  +
  geom_point() +
  ylim(c(0, 1.1))

b
a

cowplot::plot_grid(a, b,  align = "v")
  
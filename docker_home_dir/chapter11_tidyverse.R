data(chimpanzees, package = "rethinking")
d <- chimpanzees
rm(chimpanzees)
# create treatment column to represent combinations of prosoc_left and condition
d$treatment <- 1 + d$prosoc_left + 2*d$condition
# value counts 
table(d$actor)


library(tidyverse)
library(flextable)

d %>% 
  distinct(prosoc_left, condition) %>% 
  mutate(description = c("Two food items on right and no partner",
                         "Two food items on left and no partner",
                         "Two food items on right and partner present",
                         "Two food items on left and partner present")) %>% 
  flextable() %>% 
  width(width = c(1, 1, 4))

d <-
  d %>% 
  mutate(treatment = factor(1 + prosoc_left + 2 * condition)) %>% 
  # this will come in handy, later
  mutate(labels = factor(treatment,
                         levels = 1:4,
                         labels = c("r/n", "l/n", "r/p", "l/p")))

d %>% 
  count(condition, treatment, prosoc_left)

# We start with the simple intercept-only logistic regression model, which follows the statistical formula
library(brms)
b11.1 <-
  brm(data = d, 
      family = binomial,
      pulled_left | trials(1) ~ 1,
      prior(normal(0, 10), class = Intercept),
      seed = 11,
      sample_prior = T,
      file = "fits/b11.01")

library(wesanderson)
wes_palette("Moonrise2")
wes_palette("Moonrise2")[1:4]

library(ggthemes)

theme_set(
  theme_default() + 
    theme_tufte() +
    theme(plot.background = element_rect(fill = wes_palette("Moonrise2")[3],
                                         color = wes_palette("Moonrise2")[3]))
)

prior_draws(b11.1) %>% 
  mutate(p = inv_logit_scaled(Intercept)) %>% 
  
  ggplot(aes(x = p)) +
  geom_density(fill = wes_palette("Moonrise2")[4], 
               size = 0, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("prior prob pull left")

b11.1b <-
  brm(data = d, 
      family = binomial,
      pulled_left | trials(1) ~ 1,
      prior(normal(0, 1.5), class = Intercept),
      seed = 11,
      sample_prior = T,
      file = "fits/b11.01b")

prior_draws(b11.1b) %>% 
  mutate(p = inv_logit_scaled(Intercept)) %>% 
  
  ggplot(aes(x = p)) +
  geom_density(fill = wes_palette("Moonrise2")[4], 
               size = 0, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("prior prob pull left")

# wrangle


bind_rows(prior_draws(b11.1),
          prior_draws(b11.1b)) %>% 
  mutate(p = inv_logit_scaled(Intercept),
         w = factor(rep(c(10, 1.5), each = n() / 2),
                    levels = c(10, 1.5))) %>% 
  # plot
  ggplot(aes(x = p, fill = w)) +
  geom_density(size = 0, alpha = 3/4, adjust = 0.1) +
  scale_fill_manual(expression(italic(w)), values = wes_palette("Moonrise2")[c(4, 1)]) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(alpha%~%Normal(0*", "*italic(w))),
       x = "prior prob pull left")


# w = 10
b11.2 <- 
  brm(data = d, 
      family = binomial,
      bf(pulled_left | trials(1) ~ a + b,
         a ~ 1, 
         b ~ 0 + treatment,
         nl = TRUE),
      prior = c(prior(normal(0, 1.5), nlpar = a),
                prior(normal(0, 10), nlpar = b, coef = treatment1),
                prior(normal(0, 10), nlpar = b, coef = treatment2),
                prior(normal(0, 10), nlpar = b, coef = treatment3),
                prior(normal(0, 10), nlpar = b, coef = treatment4)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 11,
      sample_prior = T,
      file = "fits/b11.02")

# w = 0.5
b11.3 <- 
  brm(data = d, 
      family = binomial,
      bf(pulled_left | trials(1) ~ a + b,
         a ~ 1, 
         b ~ 0 + treatment,
         nl = TRUE),
      prior = c(prior(normal(0, 1.5), nlpar = a),
                prior(normal(0, 0.5), nlpar = b, coef = treatment1),
                prior(normal(0, 0.5), nlpar = b, coef = treatment2),
                prior(normal(0, 0.5), nlpar = b, coef = treatment3),
                prior(normal(0, 0.5), nlpar = b, coef = treatment4)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 11,
      sample_prior = T,
      file = "fits/b11.03")

# wrangle
prior <-
  bind_rows(prior_draws(b11.2),
            prior_draws(b11.3)) %>% 
  mutate(w  = factor(rep(c(10, 0.5), each = n() / 2),
                     levels = c(10, 0.5)),
         p1 = inv_logit_scaled(b_a + b_b_treatment1),
         p2 = inv_logit_scaled(b_a + b_b_treatment2)) %>% 
  mutate(diff = abs(p1 - p2)) 

# plot
prior %>% 
  ggplot(aes(x = diff, fill = w)) +
  geom_density(size = 0, alpha = 3/4, adjust = 0.1) +
  scale_fill_manual(expression(italic(w)), values = wes_palette("Moonrise2")[c(4, 2)]) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(alpha%~%Normal(0*", "*italic(w))),
       x = "prior diff between treatments")

prior %>% 
  group_by(w) %>% 
  summarise(mean = mean(diff))

print(b11.3)

d %>% 
  summarise(grand_mean = mean(pulled_left))

library(tidybayes)

posterior_samples(b11.3) %>% 
  transmute(alpha = inv_logit_scaled(b_a_Intercept)) %>% 
  mean_qi()

d %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(pulled_left))

posterior_samples(b11.3) %>% 
  pivot_longer(b_b_treatment1:b_b_treatment4) %>% 
  mutate(treatment = str_remove(name, "b_b_treatment"),
         mean      = inv_logit_scaled(b_a_Intercept + value)) %>%
  group_by(treatment) %>% 
  mean_qi(mean)

# Okay, let’s get back on track with the text. Now we’re ready to fit the full model, which follows the form
d <-
  d %>% 
  mutate(actor = factor(actor))

b11.4 <- 
  brm(data = d, 
      family = binomial,
      bf(pulled_left | trials(1) ~ a + b,
         a ~ 0 + actor, 
         b ~ 0 + treatment,
         nl = TRUE),
      prior = c(prior(normal(0, 1.5), nlpar = a),
                prior(normal(0, 0.5), nlpar = b)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 11,
      file = "fits/b11.04")

print(b11.4)

library(tidybayes)

post <- posterior_samples(b11.4)
# this plot only plot the intercept of each actor, the baseline without treatment,
# baseline tendency is the preference of using which hand 左翩子 或者右撇子
post %>% 
  pivot_longer(contains("actor")) %>%
  mutate(probability = inv_logit_scaled(value),
         actor       = factor(str_remove(name, "b_a_actor"),
                              levels = 7:1)) %>% 
  
  ggplot(aes(x = probability, y = actor)) +
  geom_vline(xintercept = .5, color = wes_palette("Moonrise2")[1], linetype = 3) +
  stat_pointinterval(.width = .95, size = 1/2,
                     color = wes_palette("Moonrise2")[4]) +
  scale_x_continuous(expression(alpha[actor]), limits = 0:1) +
  ylab(NULL) +
  theme(axis.ticks.y = element_blank())

# now consider the treatment effect 
tx <- c("R/N", "L/N", "R/P", "L/P")   

post %>% 
  select(contains("treatment")) %>% 
  set_names("R/N","L/N","R/P","L/P") %>% 
  pivot_longer(everything()) %>%
  mutate(probability = inv_logit_scaled(value),
         treatment   = factor(name, levels = tx)) %>% 
  mutate(treatment = fct_rev(treatment)) %>% 
  
  ggplot(aes(x = value, y = treatment)) +
  geom_vline(xintercept = 0, color = wes_palette("Moonrise2")[2], linetype = 3) +
  stat_pointinterval(.width = .95, size = 1/2,
                     color = wes_palette("Moonrise2")[4]) +
  labs(x = expression(beta[treatment]),
       y = NULL) +
  theme(axis.ticks.y = element_blank())


post %>% 
  mutate(db13 = b_b_treatment1 - b_b_treatment3,
         db24 = b_b_treatment2 - b_b_treatment4) %>% 
  pivot_longer(db13:db24) %>%
  mutate(diffs = factor(name, levels = c("db24", "db13"))) %>% 
  
  ggplot(aes(x = value, y = diffs)) +
  geom_vline(xintercept = 0, color = wes_palette("Moonrise2")[2], linetype = 3) +
  stat_pointinterval(.width = .95, size = 1/2,
                     color = wes_palette("Moonrise2")[4]) +
  labs(x = "difference",
       y = NULL) +
  theme(axis.ticks.y = element_blank())

# posterior predictive check.
d %>%
  group_by(actor, treatment) %>%
  summarise(proportion = mean(pulled_left)) %>% 
  filter(actor == 1)

p1 <-
  d %>%
  group_by(actor, treatment) %>%
  summarise(proportion = mean(pulled_left)) %>% 
  left_join(d %>% distinct(actor, treatment, labels, condition, prosoc_left),
            by = c("actor", "treatment")) %>% 
  mutate(condition = factor(condition)) %>% 
  
  ggplot(aes(x = labels, y = proportion)) +
  geom_hline(yintercept = .5, color = wes_palette("Moonrise2")[3]) +
  geom_line(aes(group = prosoc_left),
            size = 1/4, color = wes_palette("Moonrise2")[4]) +
  geom_point(aes(color = condition),
             size = 2.5, show.legend = F) + 
  labs(subtitle = "observed proportions")

nd <- 
  d %>% 
  distinct(actor, treatment, labels, condition, prosoc_left)

p2 <-
  fitted(b11.4,
         newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd) %>% 
  mutate(condition = factor(condition)) %>% 
  
  ggplot(aes(x = labels, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_hline(yintercept = .5, color = wes_palette("Moonrise2")[3]) +
  geom_line(aes(group = prosoc_left),
            size = 1/4, color = wes_palette("Moonrise2")[4]) +
  geom_pointrange(aes(color = condition),
                  fatten = 2.5, show.legend = F) + 
  labs(subtitle = "posterior predictions")

# combine the two ggplots
library(patchwork)

(p1 / p2) &
  scale_color_manual(values = wes_palette("Moonrise2")[c(2:1)]) &
  scale_y_continuous("proportion left lever", 
                     breaks = c(0, .5, 1), limits = c(0, 1)) &
  xlab(NULL) &
  theme(axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = alpha("white", 1/10), size = 0)) &
  facet_wrap(~ actor, nrow = 1, labeller = label_both)

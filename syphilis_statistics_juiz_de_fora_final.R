# requiring librarys ------------------------------------------------------
library(tidyverse)
library(patchwork)
library(gridExtra)
theme_set(theme_classic())
layout = "
AAAAA
AAAAA
AAAAA
##B##"
options(scipen = 999)

# Acquired Syphilis -------------------------------------------------------

# Building up syphilis's dataset based on http://indicadoressifilis.aids.gov.br

as <- data.frame(read.csv("l_sajf.csv"))
asf <- data.frame(year = as$year[-1], rate = as$rate[-1])

# fitting data in a log linear model 

yearspos_as <- 1:8 #years counted 2012~2019 because 2011 value is 0 
lmexp_as <- lm(log(asf$rate)~yearspos_as) #starting in 2011
aas <- exp(as.numeric(lmexp_as$coefficients[2])) #value of 'a' coefficient
bas <- exp(as.numeric(lmexp_as$coefficients[1])) #valeu of 'b' coefficient

r2as <- summary(lmexp_as)$r.squared # value of R2 
# p-value of the AAPC
p_value_as <- summary(lmexp_as)$coefficients[2,4]

## plotting the exponential regression

RE_1 <- ggplot(asf, aes(x = yearspos_as, y = rate)) +
  geom_point() +
  geom_smooth (method = "lm", 
               formula = y ~ I((bas*aas^x)),
               color = "black") + #fitting curve in a regular exponential function
  ylab("Detection Rate") +
  xlab("Year") +
  scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7,8),
    labels = c("2012","2013","2014","2015","2016","2017","2018","2019")) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = 0, 
                              margin = margin(t = 0, r = 0, b = 20, l = 0))) +
  ggtitle('A')
#saving plot
ggsave("acquired_syphilis.png", dpi = 300 )

# Yearly estimate change rate of acquired syphilis 

p2.5as <- exp(confint(lmexp_as)[2,1]) - 1  # 2.5% percentile of CI 95%
p50as <- aas -  1                         # estimate
p97.5as <- exp(confint(lmexp_as)[2,2]) - 1 # 97.5% percentile of CI 95%

IC_as <- tibble(APC = round(p50as,2),
                CI = paste0(round(p2.5as,2),"-",round(p97.5as,2)),
                "p-value" = ("<0.001"),
                R2 = round(r2as,2))


# Syphilis in pregnant women ----------------------------------------------

# Building up syphilis's dataset based on http://indicadoressifilis.aids.gov.br

sgf <- data.frame(read.csv("l_sgjf.csv"))

# fitting data in a log linear model 

yearspos_sg <- 1:9 #years counted 2011~2019 
lmexp_sg <- lm(log(sgf$rate)~yearspos_sg)
asg <- exp(as.numeric(lmexp_sg$coefficients[2])) #value of 'a' coefficient
bsg <- exp(as.numeric(lmexp_sg$coefficients[1])) #valeu of 'b' coefficient

r2sg <- summary(lmexp_sg)$r.squared # value of R2 
# p-value of the AAPC
p_value_sg <- summary(lmexp_sg)$coefficients[2,4]


## plotting the exponential regression

RE_2 <- ggplot(sgf, aes(x = yearspos_sg, y = rate)) +
  geom_point() +
  geom_smooth (method = "lm", 
               formula = y ~ I((bsg*asg^x)),
               color = "black") +  #fitting curve in a regular exponential function
  ylab("Detection Rate") +
  xlab("Year") +
  scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7,8,9),
    labels = c("2011","2012","2013","2014",
               "2015","2016","2017","2018","2019")) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = 0, 
                              margin = margin(t = 0, r = 0, b = 20, l = 0))) +
  ggtitle('B')

# Saving plot
ggsave("pregnant_syphilis.tiff",dpi = 600)

# Yearly estimate change rate of syphilis in pregnant women

p2.5sg <- exp(confint(lmexp_sg)[2,1]) - 1  # 2.5% percentile of CI 95%
p50sg <- asg -  1                         # estimate 
p97.5sg <- exp(confint(lmexp_sg)[2,2]) - 1 # 97.5% percentile of CI 95%

IC_sg <- tibble(APC = round(p50sg,2),
                CI = paste0(round(p2.5sg,2),"-",round(p97.5sg,2)),
                "p-value" = ("<0.001"),
                R2 = round(r2sg,2))

# Congenital Syphilis  ----------------------------------------------------

# Building up syphilis's dataset based on http://indicadoressifilis.aids.gov.br

scf <- data.frame(read.csv("l_scjf.csv"))

# fitting data in a log linear model 

yearspos_sc <- 1:9 #years counted 2011~2019 
lmexp_sc <- lm(log(scf$rate)~yearspos_sc)
asc <- exp(as.numeric(lmexp_sc$coefficients[2])) #value of 'a' coefficient
bsc <- exp(as.numeric(lmexp_sc$coefficients[1])) #valeu of 'b' coefficient
lmexp_sc %>% summary
r2sc <- summary(lmexp_sc)$r.squared # value of R2 
# p-value of the AAPC
p_value_sc <- summary(lmexp_sc)$coefficients[2,4]


## plotting the exponential regression

RE_3 <- ggplot(scf, aes(x = yearspos_sc, y = rate)) +
  geom_point() +
  geom_smooth (method = "lm", 
               formula = y ~ I((bsc*asc^x)),
               color = "black") + #fitting curve in a regular exponential function
  ylab("Detection Rate") +
  xlab("Year") +
  scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7,8,9),
    labels = c("2011","2012","2013","2014",
               "2015","2016","2017","2018","2019")) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = 0, 
                              margin = margin(t = 0, r = 0, b = 20, l = 0))) +
  ggtitle('C') 
# Saving plot
ggsave("congenital_syphilis.tiff", dpi = 600)

# Yearly estimate change rate of syphilis in pregnant women

p2.5sc <- exp(confint(lmexp_sc)[2,1]) - 1  # 2.5% percentile of CI 95%
p50sc <- asc -  1                         # estimate
p97.5sc <- exp(confint(lmexp_sc)[2,2]) - 1 # 97.5% percentile of CI 95%

IC_sc <- tibble(APC = round(p50sc,2),
                CI = paste0(round(p2.5sc,2),"-",round(p97.5sc,2)),
                "p-value" = ("<0.001"),
                R2 = round(r2sc,2))

# creating plots with table regarding AAPC, CI and R2
A <- RE_1/gridExtra::tableGrob(IC_as,theme = ttheme_default(base_size = 11), 
                               row = NULL) + plot_layout(design = layout)
B <- RE_2/gridExtra::tableGrob(IC_sg,theme = ttheme_default(base_size = 11), 
                               row = NULL) + plot_layout(design = layout)
C <- RE_3/gridExtra::tableGrob(IC_sc,theme = ttheme_default(base_size = 11), 
                               row = NULL) + plot_layout(design = layout)

APC_plot <- A|B|C

ggsave('APC.tiff',
       plot = APC_plot, 
       dpi= 600,
       width = 10.2,
       height = 5.25, 
       units = "in")

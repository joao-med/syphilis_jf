library(tidyverse)
theme_set(theme_classic())
#processing data
cases_sex <- read_csv("cases_sex.csv") %>% 
  as.tibble() %>%
  gather(category, 
         cases,
         Men, 
         Women, 
         Pregnant_Women,
         -Year) %>% 
  select(Year, cases, category)
cases_sex <- cases_sex %>% mutate(
  category = category %>% 
    str_replace("Pregnant_Women","Pregnant Women"))

cases_sex_percentage <- read.csv(link_cases_sex) %>% 
  gather(percentage_cat,
         percentage,
         percentage_men,
         percentage_women,
         percentage_preg_women) %>% 
  select(Year, percentage_cat, percentage) %>% 
  select(-Year) %>% 
  mutate(
    percentage_cat = percentage_cat %>% 
      str_replace("percentage_men", "Men") %>% 
      str_replace("percentage_women", "Women") %>% 
      str_replace("percentage_preg_women", "Pregnant Women")
  )

cases_sex <- tibble(cases_sex, cases_sex_percentage) 
# Creating figure
plot_cases_sex <- cases_sex %>% 
  ggplot(aes (x = Year, y = percentage, color = percentage_cat))+
  geom_line(lwd= 1.2)+
  geom_point(size = 3, aes(shape = percentage_cat), show.legend = T)+
  geom_text(aes(label = cases), vjust= 2, size= 3.5, check_overlap = T, show.legend = F)+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  ylab ('Relative frequency')+
  scale_color_grey()+
  scale_x_continuous(
    breaks = c(2010:2019),
    labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  scale_y_continuous(labels = scales::percent)
# saving
ggsave('fig1.tiff', plot = plot_cases_sex, dpi= 600, units = "in", height = 5.25, width = 5.25)

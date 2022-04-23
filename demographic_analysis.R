library(tidyverse)
library(patchwork)
layout_gest <- "
AABB
AABB
#CC#
#CC#
"
theme_set(theme_classic())
#Processing SPW data

tabela_gest <- 
  read.csv("tabela_gest.csv",
           encoding = 'UTF-8') %>% 
  as.tibble() %>% 
  mutate(Year = Year %>% as.integer(),
         Categories = key)

tabela_gest_split <- split(tabela_gest, f=tabela_gest$class)
#building up the figures
p1 <- tabela_gest_split$`Classificacao clinica` %>% 
  ggplot(aes (x = Year, y = value, color = Categories))+
  geom_line(lwd= 1.2)+
  scale_colour_grey()+
  geom_point(size = 2, aes(shape = Categories))+
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  guides(color= guide_legend(nrow=2, byrow=TRUE))+
  ylab ('Relative frequency in %')+
  scale_x_continuous(
    breaks = 2011:2020,
    labels = c("2011","2012","2013","2014","2015","2016","2017","2018","2019",""))+
  scale_y_continuous(limits = c(0,90)) 

#repeating the process for the next figures
p2 <- p1 %+% tabela_gest_split$`Cor raca`+ ggtitle('C') 
p3 <- p1 %+% tabela_gest_split$`Faixa etaria`+ggtitle('A') 
p4 <- p1 %+% tabela_gest_split$`gestational age`+ggtitle('B')
p5 <- p1 %+% tabela_gest_split$Escolaridade

plot_sifilis_gest <- p3+p4+p2+plot_layout(design =  layout_gest)

#saving

ggsave('fig2.tiff',dpi= 600, 
       plot = plot_sifilis_gest,
       units = "in", 
       width = 10,
       height = 7.5)


#processing congenital syphilis data
tabela_cong <- 
  read.csv("tabela_cong.csv", 
           encoding = 'UTF-8', sep = ",") %>% 
  as.tibble() %>% 
  mutate(Year = Year %>% as.integer(),
          Categories = key)


tabela_cong_split <- split(tabela_cong, f=tabela_cong$class)
#building up the figures
p1 <- tabela_cong_split$`Diagnostico final` %>% 
  ggplot(aes (x = Year , y = value, color = Categories))+
  geom_line(lwd= 1.2)+
  geom_point(size = 2, aes(shape = Categories))+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  guides(color= guide_legend(nrow=2, byrow=TRUE))+
  ylab ('Relative frequency in %')+
  scale_colour_grey()+
  scale_x_continuous(
    breaks = c(2010:2019),
    labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  scale_y_continuous(limits = c(0,100))+ ggtitle('A')
#repeating the process for the next figures

p2 <- p1 %+% tabela_cong_split$`Escolaridade da mae`
p3 <- p1 %+% tabela_cong_split$`Faixa etaria da mae`+ggtitle('B')
p4 <- p1 %+% tabela_cong_split$`Idade da crianca`+ggtitle('C')
p5 <- p1 %+% tabela_cong_split$`Momento de diagnostico`+ggtitle('D')
p6 <- p1 %+% tabela_cong_split$`Raca cor`+ggtitle('E')
p7 <- p1 %+% tabela_cong_split$`Realizacao do pre natal`+ggtitle('F')

plot_sifilis_congenita <- p1+p3+p4+p5+p6+p7+plot_layout(ncol = 2)

#saving
ggsave('fig3.tiff',dpi= 600, 
       plot = plot_sifilis_congenita,
       units = "in", 
       width = 10,
       height = 7.5)





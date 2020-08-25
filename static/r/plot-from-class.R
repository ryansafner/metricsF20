# load ggplot2 package
library(ggplot2)

# make plot
ggplot(data = mpg)+ # set data source to mpg (included in ggplot2)
  aes(x = displ, # x is displacement
      y = hwy)+ # y is hwy mpg
  geom_point(aes(color = class))+ # color points by car class
  geom_smooth()+ # add regression line
  facet_wrap(~year)+ # separate plots by year
  labs(x = "Engine Displacement (Liters)",
       y = "Highway MPG",
       title = "Car Mileage and Displacement",
       subtitle = "More Displacement Lowers Highway MPG",
       caption = "Source: EPA",
       color = "Vehicle Class")+
  scale_color_viridis_d()+ # change color scale
  theme_minimal()+ # change theme
  theme(text = element_text(family = "Fira Sans")) # change font
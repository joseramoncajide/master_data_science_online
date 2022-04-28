##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: ggplot2
##########################################################################

library(ggplot2)

# El lenguaje ggplot ------------------------------------------------------

# https://www.rstudio.com/wp-content/uploads/2016/12/ggplot2-cheatsheet-2.1-Spanish.pdf

ggplot()

(vector_y <- sample(10)) # Variable dependiente
(vector_x <- sample(10)) # Variable independiente

ggplot(mapping = aes(y = vector_y, x = vector_x))

ggplot(mapping = aes(y = vector_y, x = vector_x))  + geom_point()

# https://www.youtube.com/watch?v=jbkSRLYSojo
library(gapminder)
gapminder
?gapminder
summary(gapminder)

glimpse(gapminder)

dim(gapminder)

View(gapminder)



ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point()

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10()

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) 

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) + geom_smooth()

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) + geom_smooth(lwd=1, se=FALSE, method="lm", col="black")

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent))  + geom_point() + scale_x_log10()  + geom_smooth(se=F, lwd=1)


# Todo junto:
p <- ggplot(gapminder, aes(gdpPercap, lifeExp))
p <- p + aes(color = continent) + geom_point() 
p <- p + scale_x_log10() + aes(color = continent) + geom_point() + geom_smooth(se=F, lwd=1)
p

gappminder_plot <- ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, colour = continent,
      size = pop, frame = year) +
  geom_point(alpha = 0.4) +
  scale_x_log10()
gappminder_plot

#' Histograma: distribución de una variable cuantitativa
ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram()

ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram(binwidth = 1)

#' Añadimos una nueva variable: continent
ggplot(gapminder, aes(x = lifeExp, fill = continent)) +
  geom_histogram()


#' Gráfico de frecuencias con líneas
ggplot(gapminder, aes(x = lifeExp, color = continent)) +
  geom_freqpoly()

#' Gráfico de densidad
ggplot(gapminder, aes(x = lifeExp)) + geom_density()

#' Gráfico de densidad con más de una variable
ggplot(gapminder, aes(x = lifeExp, color = continent)) + geom_density()

#' alpha 
ggplot(gapminder, aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.2)

#' facets 
ggplot(gapminder, aes(x = lifeExp)) + geom_density() + facet_wrap(~ continent)

ggplot(gapminder,
       aes(x = lifeExp, fill = continent)) + geom_histogram() +
  facet_grid(continent ~ .)

#' boxplot de una variable cuantitativa y una discreta
#' ojo: year no es un factor
ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_boxplot()

#' podemos solucinarlo
ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_boxplot(aes(group = year))


#' geom_violin()
ggplot(gapminder, aes(x = year, y = lifeExp)) +
  geom_violin(aes(group = year)) +
  geom_jitter(alpha = 1/4) +
  geom_smooth(se = FALSE)

#' stripplots: para una variable cualitativa y una cuantitativa
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_point()

#' geom_jitter
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_jitter()

ggplot(gapminder, aes(x = continent, y = lifeExp)) + 
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

#' boxplot
ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_boxplot()

#' raw data y boxplots
ggplot(gapminder, aes(x = continent, y = lifeExp)) +
  geom_boxplot(outlier.colour = "hotpink") +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

#' superposición de información estadística
ggplot(gapminder, aes(x = continent, y = lifeExp)) + 
  geom_jitter(position = position_jitter(width = 0.1), alpha = 1/4) +
  stat_summary(fun.y = median, colour = "red", geom = "point", size = 5)

#' podemos mejorarlo reordenando el eje
ggplot(gapminder, aes(reorder(x = continent, lifeExp), y = lifeExp)) + 
  geom_jitter(position = position_jitter(width = 0.1), alpha = 1/4) +
  stat_summary(fun.y = median, colour = "red", geom = "point", size = 5)


# labels and legends ------------------------------------------------------

(gappminder_plot <- gappminder_plot + ylab("Esperanza de vida") + xlab("PIB per cápita"))

(gappminder_plot <- gappminder_plot + labs(title="Esperanza de vida y PIB per cápita", 
                                           subtitle= "Fuente: Gapminder", 
                                           caption = "Visualización: R + ggplot2"))
gappminder_plot + theme(legend.position="bottom")


# themes ------------------------------------------------------------------


gappminder_plot + theme_grey()

gappminder_plot + theme_bw()

gappminder_plot + theme_minimal()

#Crear nuestros propios temas
gappminder_plot + theme(panel.border=element_rect(color = 'white', fill = NA), 
                        panel.background = element_rect(color = 'white', fill = NA) )

gappminder_plot + theme_grey()  + theme_minimal() + theme(
  axis.title.x = element_text(size = 13), 
  text = element_text(family="Arial", colour="grey50", size=12),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),  
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
) 

# Con ggthemes: library("ggthemes")

gappminder_plot + theme_excel() + scale_fill_excel()

gappminder_plot + theme_economist() + scale_fill_economist()

gappminder_plot + theme_wsj() + scale_fill_wsj(palette = "black_green")

(gappminder_plot <- gappminder_plot + theme_tufte() + scale_fill_tableau())


# guardar un gráfico ------------------------------------------------------

ggsave("gapminder.pdf", gappminder_plot)
ggsave("gapminder.png", width = 6, height = 4)


# Bonus: library(gganimate) -----------------------------------------------

library(gganimate)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

anim_save(animation = last_animation(),filename = 'tmp/gapminder.gif')

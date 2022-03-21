
universo tiduvers
#https://derek-corcoran-barrios.github.io/Libro/_book/visualizacion.html#paquetes-necesarios-para-este-cap%C3%ADtulo-3
#tablas de colores
#https://www.r-graph-gallery.com/218-basic-barplots-with-ggplot2.html


View(babies)
#curso de dployr
install.packages("gapminder")
#instalando paquete
install.packages("hflights")
install.packages("knitr")

library(gapminder)
library(tidyverse)
library(ggplot2)
library(datasets)
library(dplyr)
library(scales)
library(hflights)

str(babies)
View(babies)
#manipular datos con dplyr
#Chapter 1 - select() function {dplyr}


#mutate() : agrega una nueva variable
#select()  escoge las columnas o variables que necesitamos.
#filter()   filtra las observaciones en base a sus valores.
#arrange()  cambia el orden de las observaciones

#https://www.youtube.com/watch?v=tpAzZOdA4rw
#video tutorial
#https://www.youtube.com/watch?v=yxo5HTCycBY
#https://www.youtube.com/watch?v=AMUGzn_j2sA

#curso de ggplot2 para graficas


#apartamentos
url <- 'https://tinyurl.com/hwhb769'
datos <- read.table(file=url, header=T)
View(datos)
#edad,peso
url <- 'https://tinyurl.com/k55nnlu'
atos <- read.table(file=url, header=T)
View(atos)
str(atos$sexo)
table(atos$sexo)

atos$sexo<-factor(atos$sexo,
                  levels=c("Hombre","Mujer"))
atos
str(atos)
prop.table(atos$sexo)


#cargar datos

data("Titanic")#
data(hig)
View(Titanic)
data("starwars")#.........
View(starwars)
head(starwars)
View(starwars)
data("gapminder")#.........
View(gapminder)
data("babies").......
data("dt1")
data("dato")
data("iris")#.......
View(iris)
hist(iris$Sepal.Length)
summary(babies)
data("gapminder")
View(gapminder)
View(datos)






# continuacion dployr

starwars %>% 
  filter(skin_color=="fair") %>% 
  summarise(conteo= n())

iris_2 <- iris %>% mutate(sepalo= Sepal.Length*Sepal.Width)
print(iris_2)

starwars %>% 
  filter(gender=="masculine",
         species=="Human")

starwars %>% 
  group_by(species) %>% 
  summarise(conteo=n(),
            media=mean(species,na.rm=TRUE))
  

starwars %>% 
  group_by(eye_color) %>% 
  summarise(conteo=n())


starwars %>% 
  group_by(species) %>%
  summarise(promedio = mean(mass,na.rm=TRUE),
            desv_est = sd(mass,na.rm=TRUE),
            cantidad=n(),
            ) %>% 
  filter(cantidad>1) %>% 
  arrange(desc(promedio))

starwars %>% 
  filter(species=='Human') %>% 
  summarise(conteo=n())

starwars %>% 
  arrange((height))

starwars %>% 
  group_by(species) %>% 
  summarise(promedio = mean(mass)) %>% 
  arrange(desc(promedio))
  



max()iris_2year<=1972,lifeExp>30)
max()gapminder %>% 
  filter(continent=='Europe',
         year>1972,lifeExp>30)

iris %>% 
  filter(Species=='setosa',
         Sepal.Length>4.7,
         Sepal.Width>3.5) %>% 
  summarise(conteo=n())
      
          
datos %>% 
  filter(ubicacion=='norte',
         estrato==4,balcon=='si')

datos %>% 
  filter(ubicacion=='norte',
         estrato==4,balcon=='si') %>% 
  summarise(conteo=n())

atos %>% 
  filter(sexo=='Mujer',
         altura>170,
         peso>42)

#para saber el numero de mujeres empleamos
#summarise


atos %>% 
  filter(sexo=='Mujer',
         altura>170,
         peso>42) %>% 
  summarise(conteo=n())

#AHORA Vamos a crear dos variables con mutate apartir de iris
#hemos creado de los datos de irirs una nueva base de datos llamada
#iris2 para eso empleamos mutate

iris2 <- iris %>% 
  mutate(totalpetal=Petal.Length*Petal.Width,
         totalsepa=Sepal.Length*Sepal.Width)
  

View(iris2)

#..............ahora vamosa empleay gropu by

gapminder %>% 
  group_by(year) %>% 
  summarise(promedio=mean(lifeExp))

gapminder %>% 
  group_by(country) %>% 
  summarise(prom=mean(lifeExp))

gapminder %>% 
  group_by(continent) %>% 
  summarise(promm=mean(lifeExp))

iris %>% 
  group_by(Species) %>% 
  summarise(prom=mean(Petal.Length))

iris %>% 
  group_by(Species) %>% 
  summarise(prom=mean(Petal.Width))

starwars %>% 
  group_by(species) %>% 
  summarise(conteo=n(), 
            media=mean(height,na.rm=TRUE),
            maximo=max(height,na.rm=TRUE)) %>% 
  filter(conteo>1) %>% 
  arrange(desc(conteo))

gapminder %>% 
  group_by(continent) %>% 
  summarise(media=mean(lifeExp,na.rm=TRUE)) %>% 
  arrange(desc(media))

atos %>% 
  group_by(sexo) %>% 
  summarise(media_peso=mean(peso),
            media_altura=mean(altura),
            media_muneca=mean(muneca),
            media_bicep=mean(biceps)) %>% 
  arrange(desc(media_peso))


###### ahora vamos a emplear select es una funcion muy importante ya que me 
#permite elegir y crear variables asi como descartar ademas select tiene otras funciones
#importantes

iris3 <- iris %>% 
  select(-Species)
iris3

iris4 <- iris %>% 
  select(starts_with("petal"))
iris4


iris5<- iris %>% 
  select(-c(Petal.Length,Petal.Width))
iris5
iris6<- iris %>% 
  select(Species)
iris6


atos %>% 
  group_by(edad) %>% 
  summarise(min(edad),max(edad))

?filter

babies %>% #funcion pipe
  filter(clase.edad=='viejos')

iris %>%
  filter(Species=='setona')
datos %>%
  filter(ubicacion=='poblado')
datos %>%
  filter(balcon=='no')

atos %>%
  filter(sexo =='mujer')

head(atos)

#los mayores de 40 años y de orden ascendente y creacionnde nueva variable

atos %>%
  select(edad,sexo) %>%
  filter((edad=35)) %>%
  arrange((edad)) %>%
  
  


babies %>% 
  select(age,weight) %>% 
  filter(age>=40) %>% 
  arrange(age) %>% 
  mutate( total = age*weight)

babies %>% 
  mutate(total=age*weight)
  

#crear otro dataframe u otra variable..........
mibabies=select(babies,weight,age)

View(mibabies)

#los que pesan mas de 100
babies %>%
  filter(weight>=100)



           
           
 babies %>%  
   select(clase.edad) %>% 
   filter(clase.edad=='viejos')

 #selleccionar columnas
 babies[,c("height","weight","age")]
 babies %>% 
   select("height","weight")
 
 iris[,c("species","petal.width")]
 babies %>% 
   select("species","petalwidth")
 
gapminder %>%  
  filter(lifeExp<=40,
         year==2002)

dt1 %>% 
  filter(sexo=="mujer")

#sacar medias cuando se tienen NAS se emplear na.rm =TRUE

mean(babies$height,na.rm = TRUE)
mean(babies$weight,na.rm=TRUE)
mean(babies$clase.edad)
mean(gapminder$lifeExp)

mean(as.factor(babies$clase.edad))

table(babies$clase.edad)
babies %>% 
  group_by(age) %>% 
  summarise(media=mean(age),min(age),max(age))

gapminder %>% 
  group_by(year) %>% 
  summarise(media=mean(lifeExp),min(lifeExp),max(lifeExp))


#...........funcion glimpse saber que tipo de variables tengo
glimpse(babies)
class(babies)
##### con esta funcion sabemos que tipo de variable tenemos.....

class(babies$height)
str(babies)
summary(babies$smoke)
 #######%>% %>% %>% %>% %>%  #control...flechita...y letra m
%>% 
####........................................curso de ggplot2

datasets::iris
colnames(iris)
data("starwars")

m<-ggplot(iris,aes( x = Sepal.Width, y = Sepal.Length))+geom_point()+
  geom_smooth(method = "lm")
m + xlim(c(1,5))+ ylim(c(4,8))
View(iris)

# ahora agregaremos etiquetas
m + labs(title = "width vs length" , subtitle = "gio",y= "lenth" ,x= "width")

## vamos a probar la funcion geom_bar
table(iris$Species)
prop.table((iris$Species))

ggplot(iris,aes(x=Species))+geom_bar()

table(gapminder$continent)
ggplot(gapminder,aes(x=continent))+geom_bar()

## ...................................graficas con frecuencia absoluta 

table(gapminder$continent)

ggplot(gapminder, aes(x = continent)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "continente", 
       y = "Frequencias absolutas", 
       title = "numero de continentes")

##........................................... graficas absolutas con colores

ggplot(gapminder, aes(x=as.factor(continent), fill=as.factor(continent) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") +
labs(x = "continente", 
     y = "Frequencias absolutas", 
     title = "numero de continentes")

ggplot(gapminder, aes(x=continent, fill=continent )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

#.................................  gragicas de orden ascendente 

library(dplyr)
plotdata <- gapminder %>%
  count(continent)
ggplot(plotdata, 
       aes(x = reorder(continent, n), 
           y = n)) + 
  geom_bar(stat = "identity",fill="blue",color="black") +
   labs(x = "continente", 
       y = "Frequencia", 
       title  = "continentes")



data(Marriage, package="mosaicData")
install.packages("mosaicData")
library(mosaicData)
data(Marriage, package = "mosaicData")

####### graficas con valores en la parte superior


library(dplyr)
plotdata <- gapminder %>%
  count(continent)
ggplot(plotdata, 
       aes(x = continent, 
           y = n)) + 
  geom_bar(fill="blue",stat = "identity") +
  geom_text(aes(label = n), 
            vjust=-0.5) +
  labs(x = "continente", 
       y = "Frequencia", 
       title  = "Participants by race")

ggplot(gapminder, aes(x=continent, fill=continent )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")



###### graficas de frecuencias con valores numerocos en la parte superior y un color

library(dplyr)
plotdata <- gapminder %>% #  en iris nuestra base de datos
  count(continent) # la variable a tener en cienta
ggplot(plotdata, 
       aes(x = continent, # en x lo que queremos graficas
           y = n)) + 
  geom_bar(fill = "cornflowerblue",color= "black",
           stat = "identity") +
  geom_text(aes(label = n), 
            vjust=-0.5) +
  labs(x = "continentes", 
       y = "Frecuencia", 
       title  = "continentes")




## .............................................grafica de porcentajes

ggplot(gapminder, 
       aes(x = continent, 
           y = ..count.. / sum(..count..))) + 
  geom_bar() +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race") +
  scale_y_continuous(labels = scales::percent)


prop.table(gapminder$continent)

#.....................................................pendiente poner porcentajes


library(ggplot2)
data(Marriage, package = "mosaicData")

library(dplyr)
library(scales)
plotdata <- Marriage %>%
  count(race) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

ggplot(plotdata, 
       aes(x = reorder(race, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race")

#.......................................... barras horizontales

library(ggplot2)
data(Marriage, package = "mosaicData")


ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  coord_flip()

       
ggplot(gapminder, aes(x = continent)) + 
  geom_bar(fill="indianred3",color= "black") +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  coord_flip()

# ...................los denominadores rotados

ggplot(gapminder, aes(x = continent)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


#.................................histogramas----solo variables cuantitativas.....

library(ggplot2)

# plot the age distribution using a histogram

# bin sirve para el numero de barras horizontales
ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="expectativa de vida por años", 
       subtitle = "number of bins = 20",
       x = "edad",y= "frecuencia")

#bin sirvewidth sirve para grosor de barras

ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 10) + 
  labs(title="Participants by age", 
       subtitle = "binwidth = 10 years",
       x = "Age",y="frecuencias")
#......................................histograma procentaje

library(scales)

ggplot(gapminder, 
       aes(x = lifeExp, 
           y= ..count.. / sum(..count..))) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 10) + 
  labs(title="Participants by age", 
       y = "Porcentage",
       x = "edad") +
  scale_y_continuous(labels = percent)

#.......................................grafica de densidades

  ggplot(gapminder, aes(x = lifeExp)) +
    geom_density(fill = "indianred3") + 
    labs(title = "Participants by age") 
  
##3....................................................barchat.....graficas bivariadas
#cuantos valores nulos tenemos
  sapply(starwars,function(x)sum(is.na(x)))
  
#otra tabla de datos
  starwars2<-na.omit(starwars)
str(starwars2)
summary(starwars2)
View(starwars2)
mean(starwars$height,na.rm = TRUE)

media_height<-round(mean(starwars$height,na.rm= TRUE),2)
media_height
starwars$height[is.na(starwars$height)]=media_height
View(starwars)


  ggplot(mpg, 
         aes(x = class, 
             fill = drv)) + 
    geom_bar(position = "stack")
# ......................................... 
  ggplot(starwars2, 
         aes(x = gender, 
             fill = skin_color)) + 
    geom_bar(position = "stack")
  
  str(starwars)
  summary(starwars)
  table(starwars$homeworld)
  
  
  library(dplyr)
  plotdata <- starwars %>%
    group_by(gender, skin_color) %>%
    summarize(n = n()) %>% 
    mutate(pct = n/sum(n),
           lbl = scales::percent(pct))
  plotdata
 #................................. 
  
  #333   ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,  sacar porcentajes

  
  library(dplyr)
  plotdata <- mpg %>%
    group_by(class, drv) %>%
    summarize(n = n()) %>% 
    mutate(pct = n/sum(n),
           lbl = scales::percent(pct))
  plotdata
  
  
  
  ggplot(plotdata, 
         aes(x = factor(class,
                        levels = c("2seater", "subcompact", 
                                   "compact", "midsize", 
                                   "minivan", "suv", "pickup")),
             y = pct,
             fill = factor(drv, 
                           levels = c("f", "r", "4"),
                           labels = c("front-wheel", 
                                      "rear-wheel", 
                                      "4-wheel")))) + 
    geom_bar(stat = "identity",
             position = "fill") +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = percent) +
    geom_text(aes(label = lbl), 
              size = 3, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2") +
    labs(y = "Percent", 
         fill = "Drive Train",
         x = "Class",
         title = "Automobile Drive by Class") +
    theme_minimal()  
  
  #..................................  line plot..........series de tiempo
  
  data(gapminder, package="gapminder")
  
  # Select US cases
  library(dplyr)
  plotdata <- filter(gapminder, 
                     country == "Bolivia")
plotdata
  
  # simple line plot
  ggplot(plotdata, 
         aes(x = year, 
             y = pop)) +
    geom_line()

  
######
  
  ggplot(plotdata, 
         aes(x = year, 
             y = lifeExp)) +
    geom_line(size = 1.5, 
              color = "green") +
    geom_point(size = 3, 
               color = "steelblue") +
    labs(y = "Life Expectancy (years)", 
         x = "Year",
         title = "Life expectancy changes over time",
         subtitle = "United States (1952-2007)",
         caption = "Source: http://www.gapminder.org/data/")  
  
 ###########................................. categorica vs cuantitativa
 # esta grafica nos permite sacar tambien codigos de barras relacionadas
# graficos de barras cuando tengamos variables categoricas y cuantitativas
  
  library(dplyr)
  plotdata <- Salaries %>%
    group_by(rank) %>%
    summarize(mean_salary = mean(salary))
  
  # plot mean salaries
  ggplot(plotdata, 
         aes(x = rank, 
             y = mean_salary)) +
    geom_bar(stat = "identity")
  
  
  library(scales)
  ggplot(plotdata, 
         aes(x = factor(rank,
                        labels = c("Assistant\nProfessor",
                                   "Associate\nProfessor",
                                   "Full\nProfessor")), 
             y = mean_salary)) +
    geom_bar(stat = "identity", 
             fill = "cornflowerblue") +
    geom_text(aes(label = dollar(mean_salary)), 
              vjust = -0.25) +
    scale_y_continuous(breaks = seq(0, 130000, 20000), 
                       label = dollar) +
    labs(title = "Mean Salary by Rank", 
         subtitle = "9-month academic salary for 2008-2009",
         x = "",
         y = "")  
  
  
  ### otro ejemplo
  
  library(dplyr)
  kio <- gapminder %>%
    group_by(continent) %>%
    summarize(promedio_expectativa = mean(lifeExp))
  kio
  
  # plot mean salaries
  ggplot(kio, 
         aes(x = continent, 
             y = promedio_expectativa)) +
    geom_bar(stat = "identity")
  
# ............................................. grafico de densidades
 
  ggplot(iris, 
         aes(x = Species, 
             fill = Sepal.Width)) +
    geom_density(alpha = 0.5) +
    labs(title = "Salary distribution by rank")  

  
  ggplot(Salaries, 
         aes(x = salary, 
             fill = rank)) +
    geom_density(alpha = 0.4) +
    labs(title = "Salary distribution by rank")
  
  
#otro ejemplo...........................................grafico de box plot
  
  ggplot(iris, 
         aes(x = Species, 
             y = Sepal.Width)) +
    geom_boxplot() +
    labs(title = "Salary distribution by rank")

 ####333 otra grafica
  
  library(dplyr)
pe <-  iris %>%
    group_by(Species) %>%
    summarize(conteo = n(),
              mean_sepal=mean(Sepal.Width,na.rm=TRUE))
pe

ggplot(pe, 
       aes(x = Species, 
           y = mean_sepal, 
           group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_sepal, 
                    ymax = mean_sepal ), 
                width = .1)    
starwars$species           
summary(starwars$species)
str(starwars$species)   
  

  
  


    
  
  
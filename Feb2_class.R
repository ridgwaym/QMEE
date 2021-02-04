#code from class Feb 2

getwd()

library(tidyverse)

dat <- read_csv("CA_homicide.csv", col_types = cols())
dat
popdat <- read_csv("CA_popdat.csv", col_types = cols())
popdat
head(dat)

view(dat) #opens data table in new window




#What if we want combine other information?
#set up a region variable to distinuish btw all of canada and all regions included
#rep means repeat, e.g rep(8,4) will give you 8,8,8,8
# "$" symbol lists subset of data you tell it to
rdat <- tibble(Place=dat$Place,
               Region=c("all",rep("Atlantic",4),
                        rep("East",2),
                        rep("West",4),
                        rep("North",3)))
head(rdat)





#Let’s start by converting the data to long form:
#want to put the names in the columns in a column called "year" and the values 
#from those columns in a column called "homicide"
# pipe passes the data to the function
# - place means you want to squash out the columns except place (it will be replicated)
#use name_transform to convert the years back to numeric values
sdat <- (dat
         %>% pivot_longer(names_to="year",values_to="homicides",-Place,
                          names_transform=list(year=as.numeric))
)
view(sdat)
head(sdat) #shows first few rows






#need to add the region and population data to the homicide data
#now combine all 3 data sets (full_join will automatically match all columns 
#with identical names across the data sets, but its better practice to specify
#the matching columns explicitly)
#the values need to match perfectly for full join to work...

sdat2 <- sdat %>%
  full_join(rdat,by="Place") %>%
  full_join(popdat,by="Place") #tell it which columns should match
view(sdat2)





#If we just used the original data set (without the added stuff), it’s fairly 
#easy to get summary statistics by dropping the first row (so that we have a 
#data frame that is all numeric) and computing means of rows and columns:

dmat <- as.matrix(dat[,-1])
rownames(dmat) <- dat$Place
rowMeans(dmat)  ## means by place

colMeans(dmat)  ## means by year

#(Don’t forget the na.rm argument, unnecessary in this case, that can be 
#provided to most R summary functions to get them to ignore NA values.)
#If we want summary statistics from the full data set we can do...

sdat2 %>%
  group_by(Place) %>%
  summarise(homicides=mean(homicides))

## fancier
sdat2 %>%
  group_by(year) %>%
  summarise(across(homicides,list(mean=mean,sd=sd)))




##Re-ordering data:
#One more useful technique is reordering factors (representing categorical 
#variables) in a sensible way. Right now the ‘places’ (provinces, territories, 
#etc.) are ordered alphabetically, R’s default.

sdat3 <- sdat2 %>%
  mutate(Place=forcats::fct_reorder(Place,Pop_2011))
view(sdat3)
## equivalent
sdat3 <- sdat2 %>%
  mutate(across(Place,~forcats::fct_reorder(.,Pop_2011)))
##to reorder place based on 2011 population size
sdat3 <- (sdat2
          %>%mutate(Place=fct_reorder(Place, Pop_2011)))
sdat3

#This will be useful in the future, but is different from the order the data 
#frame is stored in, which we can modify via arrange() (use desc(Pop_2011) to 
#arrange in descending order of population):

(sdat3
  %>% arrange(desc(Pop_2011))
  %>% head()
)



##summarize by multiple variables:
#I can also summarise by combinations of variables:

sdat3 %>% group_by(year,Region) %>%
  summarise(across(homicides,mean),.groups="drop")



##SEM:
#What if I want the mean and standard error? R doesn’t have a built-in 
#“standard error of the mean” function so I define one when I need it:

sem <- function(x) { sd(x)/sqrt(length(x)) }
region_avgs <- sdat3 %>% group_by(year,Region) %>%
  summarise(across(homicides,list(mean=~mean(.,na.rm=TRUE),
                                  sem=sem)),
            .groups="drop")
head(region_avgs)

#Question: why do I have NA values?

#Drilling down to check some values:

sdat3 %>% filter(year==2007 & Region=="all")

#Sometimes it’s useful to be able to go from long to wide format. pivot_wider() 
#is the opposite of pivot_longer(): we specify a column in the current data set 
#to spread out into new columns (key) and a column to use as the vales for the 
#table (value)

(region_avgs
  %>% select(-homicides_sem)
  %>% pivot_wider(names_from=Region,values_from=homicides_mean)
)

#Save the results:
saveRDS(sdat3,file="CA_homicide.rds")

#For analysis in R, it is generally best to keep your data in long format and 
#pivot_wider() it as necessary (e.g. when creating a human-readable table for 
#output).







####Part 2: Pictures

mdat <- readRDS("CA_homicide.rds")
mdat

#One of the advantages of long format is that it allows us to use some of R’s 
#more powerful graphics tools such as the ggplot2 and lattice packages (and 
#it’s what most statistics packages expect):

library(ggplot2)
theme_set(theme_bw())  ## black-and-white theme
## set up basic plot:
p1 <- ggplot(mdat,aes(year,homicides,colour=Place)) #get empty plot

#Unlike base plots (which can only be saved/replayed through the RStudio 
#interface), ggplot produces an R object which can then be printed (=displayed 
#in a graphics window), or saved (or exported to a graphics file via ggsave()):

print(p1+geom_line())

#We could add both lines and points:
print(p1+geom_line() +geom_point())

#Might be better on a log scale, with a sensible y-axis label:
p1L <- (p1
        + geom_line()
        + scale_y_log10()
        + labs(y="Homicides per 100,000 population")
)
print(p1L)

### Warning: Transformation introduced infinite values in continuous y-axis


#(Zero values get squashed against the lower axis)
#Maybe we don’t care about time at all:

b1 <- (ggplot(mdat,aes(x=Place,y=homicides,
                       colour=Region))
       + geom_boxplot(outlier.colour=NULL)  ## set outlier points to same colour as boxes
       + scale_y_log10()
       + labs(y="Homicides per 100,000 population")
)
print(b1)

## Warning: Transformation introduced infinite values in continuous y-axis
## Warning: Removed 4 rows containing non-finite values (stat_boxplot).

#The x-axis tick labels overlap enough to be unreadable (unless we resize the plot to be ridiculously long and narrow).

#We could rotate them 90 degrees to be vertical:

b1_vertlabels <- b1+theme(axis.text.x=element_text(angle=90))
print(b1_vertlabels)

## Warning: Transformation introduced infinite values in continuous y-axis
## Warning: Removed 4 rows containing non-finite values (stat_boxplot).

#In general if you want to tweak a ggplot plot, Google it or search the ggplot 
#theme documentation or the ggplot cheat sheet for more information …

#Rotating the whole plot is less familiar, but arguably better. Here I’m 
#also (1) changing the colour palette and (2) changing the order of the Place 
#variable, using %+% to substitute a different set of data into an existing plot:

mdat_sort <- mdat %>% mutate(across(Place,~forcats::fct_reorder(.,homicides)))
print(b1
      %+% mdat_sort  ## substitute sorted data
      + coord_flip()      ## rotate entire plot
      + xlab("")          ## x-label redundant
      + scale_colour_brewer(palette="Dark2") ## change palette
)

#Maybe we want to make our line graph less busy:
print(p1L+facet_wrap(~Region))

#We could also code population size by line width:
p2 <- (ggplot(mdat,
              aes(year,homicides,colour=Region,size=Pop_2011,
                  group=Place))
       + geom_line(alpha=0.5)
       + scale_y_log10()
       + scale_size_continuous(trans="log10")
       + labs(y="Homicides per 100,000 population")
)
print(p2)

#Using the directlabels package:
library(directlabels)
print(p1L
      + expand_limits(x=2014)  ## add a little more space
      + geom_dl(aes(label=Place),method="last.bumpup") 
      + theme(legend.position="none")  ## don't need the legend any more
)

#We’d have to work a little harder to avoid clipping the “Yukon” label …







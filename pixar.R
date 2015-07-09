#use libs
library(ggplot2);library(plyr);library(reshape2); library(directlabels)
library(grid);library(scales);library(RColorBrewer); library(wordcloud); library(gridExtra)

#load data
pix=read.csv("pixar.csv")

#Create a function for the look of my charts
#Used minimaxir's code as base R code to work off of
my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="right") +
  theme(legend.background = element_rect(fill=color.panel)) +
  theme(legend.text = element_text(size=10,color=color.axis.title)) + 

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=20, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=12,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

is.even <- function(x) {x %% 2 == 0}
is.odd <- function(x) {x %% 2 != 0}

#Scatter 1
ggplot(pix, aes(x=year, y=tomatoes))+
my_theme()+
geom_point(aes(size=weekend, color=factor(Oscars))) +
scale_colour_manual(values = c("#fd8d3c","#252525", "#41b6c4"), guide_legend(title="Best Animated Film?"))+
scale_size_continuous(guide_legend(title="Opening Weekend Gross \n ($ Millions)"), range = c(2, 10))+
geom_smooth(method="auto", se=FALSE)+
geom_text(data=subset(pix, is.even(year)), aes(label=movie), vjust=3, hjust=0.5, size=3)+
geom_text(data=subset(pix, is.odd(year)), aes(label=movie), vjust=-2, hjust=.5, size=3)+
labs(title= "", x="Year", y="Rotten Tomatoes % Fresh")+
ggtitle(expression(atop(bold("Pixar is Back, Baby"), atop(italic("20 years of pixar at a glance (part I)"),""))))+
theme(plot.title = element_text(size = 16, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) +
theme(legend.title = element_text(size=10, face="bold"))

#Scatter 2
ggplot(pix, aes(x=year, y=imdb))+
my_theme()+
geom_point(aes(size=weekend, color=factor(Oscars))) +
scale_colour_manual(values = c("#fd8d3c","#252525", "#41b6c4"), guide_legend(title="Best Animated Film?"))+
scale_size_continuous(guide_legend(title="Opening Weekend Gross \n ($ Millions)"), range = c(2, 10))+
geom_smooth(method="auto", se=FALSE)+
geom_text(aes(label=movie), vjust=-1.8, hjust=0.5, size=3)+
labs(title= "", x="Year", y="IMDb Rating (Out of 10)")+
ggtitle(expression(atop(bold("Pixar is Back, Baby"), atop(italic("20 years of pixar at a glance (part II)"),""))))+
theme(plot.title = element_text(size = 16, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) +
theme(legend.title = element_text(size=10, face="bold"))+ 
ylim(6,10)



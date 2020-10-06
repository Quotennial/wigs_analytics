theme_wigs_night = function(base_size=8, base_family="Verdana") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold", size = rel(3.5), hjust = 0.5, color = "white"),
            plot.subtitle = element_text(size = rel(2.5), color = "white"),
            text = element_text(color = "white"),
            panel.background = element_rect(fill = "#343436", colour = "#343436"),
            plot.background = element_rect(fill = "#343436", colour = "#343436"),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(2), color = "white"),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(size = rel(2.5), color = "white"), 
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.background = element_rect(fill = "#343436", linetype = "solid"),
            legend.text = element_text(colour = "white", size = rel(2)),
            legend.title = element_text(colour = "white", size = rel(2.5)),
            legend.key = element_rect(fill = "#343436", color = NA),
            legend.key.size= unit(1.5, "cm"),
            legend.spacing  = unit(0.2, "cm"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold", size = rel(2.5), color = "#343436")
    )) }

theme_wigs_day = function(base_size=8, base_family="Verdana") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold", size = rel(3.5), hjust = 0.5, color = "#343436"),
            plot.subtitle = element_text(size = rel(2.5), color = "#343436"),
            text = element_text(color = "#343436"),
            panel.background = element_rect(fill = "white", colour = "white"),
            plot.background = element_rect(fill = "white", colour = "white"),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(2), color = "#343436"),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(size = rel(2.5), color = "#343436"), 
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.background = element_rect(fill = "white", linetype = "solid"),
            legend.text = element_text(colour = "#343436", size = rel(2)),
            legend.title = element_text(colour = "#343436", size = rel(2.5)),
            legend.key = element_rect(fill = "white", color = NA),
            legend.key.size= unit(1.5, "cm"),
            legend.spacing  = unit(0.2, "cm"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#343436",fill="#343436"),
            strip.text = element_text(face="bold", size = rel(2.5), color = "white")
    )) }


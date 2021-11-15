##----theme-validation--------------------------------------------------------------------
theme_validation <- function() {
  theme_bw() +
    theme(
      strip.text = element_text(size = 8, 
                                margin = margin(b = 0, t = 1)),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.title.y = element_blank(),
      #axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )
}


##----theme_characterisation--------------------------------------------------------------

theme_characterisation <- function() {
  
  theme_bw() + # seeting theme
    theme(strip.text = element_text(size = 10,
                                    margin = margin(b = 0, t = 0))) +
    # narrow facet space
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + # no axis ticks 
    theme(panel.spacing =unit(0, "lines")) +  # to ensure no gap between facets
    theme(axis.text.x = element_text(angle=90, hjust=1, size = 10)) + 
    # rotate the x-axis text
    theme(legend.position = "bottom")+
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
    theme(axis.text.x = element_text(size=5)) +
    theme(axis.text = element_text(size = 12))  
}


##----mytheme-application----------------------------------------------------------------
theme_application <- function() {
  
  theme_light() + # setting theme
    theme(strip.text = element_text(size = 10,
                                    margin = margin(b = 0, t = 0)), 
          strip.text.x = element_text(size = 8)) + # narrow facet space
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + # no axis ticks 
    theme(panel.spacing =unit(0, "lines")) +  # to ensure no gap between facets
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) + # no x-axis labels to further reduce the gap between facets
    #theme(axis.text.x = element_text(angle=90, hjust=1, size = 9)) + # rotate the x-axis text
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
    #theme(axis.text.x = element_text(size=5)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank()) +
    theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
          )
}



##----mytheme-application2----------------------------------------------------------------
theme_application2 <- function() {
  theme_characterisation() +   
    theme(strip.background = element_blank(),
                                       strip.text.x = element_blank()) +
    theme(axis.text.x = element_text(size=6)) +
    theme(axis.text = element_text(size = 6))  

}

# ##----mytheme-application3
# theme_application3 <- function() {
#   theme_characterisation() +
#     theme(
#       panel.grid.major.y = element_blank(),
#       panel.grid.minor.y = element_blank()
#     )+ 
#     theme(axis.ticks.x = element_blank(),
#           axis.text.x = element_blank()) +
#     theme(
#           strip.text.y = element_blank())
#     #theme(strip.text = element_text(size = 10,
#      #                               margin = margin(b = 0, t = 0)))
#   
# }


##----mytheme-application3
theme_application3 <- function() {
  theme_characterisation() +   
    theme(axis.text.x = element_text(size=6)) +
    theme(axis.text = element_text(size = 6))  
  
}


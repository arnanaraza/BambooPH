multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



RegLine <- function(df){
  names(df) <- c(names(covs), 'reference', 'predicted')
  ggplot(df, aes(x=reference, y=predicted))+geom_point()+
    labs(x = bquote('Reference bamboo cover ) (%'~ha^-1), y= bquote('Predicted bamboo cover ) (%' ~ha^-1)) + 
    xlim(0, 100) + ylim(0, 100)+
    geom_smooth(method=lm, se=FALSE,fullrange=TRUE) + 
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          text = element_text(size=13),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
}
#bquote('x axis'~(Å^2))


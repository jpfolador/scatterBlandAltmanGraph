#--------------------------------------------
# Scatter and Bland-Altman chart in R   
#
# Author: Joao Paulo Folador
# email: jpfolador at gmail.com
# version: 1.0
#--------------------------------------------

# load packages
if (!require("tidyr")) { install.packages('tidyr') }
if (!require("irr")) { install.packages('irr') }
if (!require('blandr')) { install.packages('blandr') }
if (!require("ggplot2")) { install.packages("ggplot2") }
if (!require("ggthemes")) { install.packages("ggthemes") }
if (!require("cowplot")) { install.packages("cowplot") }
if (!require("extrafont")) { install.packages("extrafont") }
if (!require("RColorBrewer")) { install.packages("RColorBrewer") }

# Load Windows fonts
loadfonts(device = "win")

# Load a theme for ggplot
theme_set(theme_bw())

# Function to build the scatter and Bland-Altman plots
# A: data from judge/evaluator A
# B: data from judge/evaluator B
# nameA:  the label of X-axis judge/evaluator A
# nameB: the label of Y-axis judge/evaluator B
# extraTitle: extra information in the chart title
scatterBAChart <- function(A, B, nameA, nameB, extraTitle='') 
{
  axisTextColor <- "#555555"
  lineLimitColor <- "#AC3E00"
  pointColor <- "#8C3E00"
  mainLineColor <- "#FF9366"
  areaColor <- "#6F5845"
  fontFamily <- "serif"
 
  nameA = as.character(nameA)
  nameB = as.character(nameB)
  
  mean <- (A + B) / 2
  
  dif.judgeAB <- A - B
  dif.mean <- mean(dif.judgeAB, na.rm = TRUE)
  dif.sd <- sqrt(var(dif.judgeAB, na.rm = TRUE))
  
  lowerLimit <- dif.mean - (2 * dif.sd)
  upperLimit <- dif.mean + (2 * dif.sd)
  
  # build the limits
  limit.y <- max(abs(min(dif.judgeAB, lowerLimit)), abs(max(dif.judgeAB, upperLimit)))
  
  # build t-test to the evaluators A and B
  tTest <- t.test(A, B, paired = TRUE)
  
  # Correlation coefficient between evaluator A and B
  corTest <- cor.test(A, B)
  
  # build the label of correlation coefficient and confidence interval
  r_ci <- paste("r: ", round(corTest$estimate[[1]],2), 
                ", ci95%: [", round(corTest$conf.int[[1]], 2), "; ", 
                round(corTest$conf.int[[2]],2), "]", sep="")
  
  data <- data.frame("mean"=mean, "dif"=dif.judgeAB, "A"= A, "B"=B)
  
  disp <- ggplot(data, aes(x=A, y=B)) +
            geom_point(shape=18, stat = "identity", color = pointColor, size=3) +    
            geom_smooth(method=lm, color = mainLineColor, fill = areaColor) +
            geom_hline(yintercept = 0, color = "grey") + 
            geom_vline(xintercept = 0, color = "grey") + 
            geom_abline(slope = 1, color = "#444444", linetype = "dashed") +
            labs(x = nameA, y = nameB, subtitle = paste("A", extraTitle)) +
            theme(text = element_text(size=22,  family=fontFamily, color=axisTextColor),
                  axis.text.x = element_text(size=24, color=axisTextColor),
                  axis.text.y = element_text(size=24, color=axisTextColor), 
                  plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
                  plot.title = element_text(size = 20, color=axisTextColor)) + 
          
            annotate("text", x = 0.2, y = 1.4, size=7,  family=fontFamily, color=axisTextColor,
                     label = as.character(as.expression(r_ci)), hjust=0,
                     parse = FALSE)
  
  blalt <- ggplot(data, aes(x=mean, y=dif)) +
             annotate("rect", xmin=-Inf, xmax=Inf, ymin=lowerLimit, ymax=upperLimit, 
                      alpha=0.3, fill=areaColor) +
             geom_point(shape = 18, color = pointColor, size=3) +    
             geom_hline(yintercept = 0, color = "grey") + 
             geom_vline(xintercept = 0, color = "grey") + 
            
             labs(x = "Average", y = "Differences", subtitle = paste("B", extraTitle)) +
             theme(text = element_text(size=22,  family=fontFamily, color=axisTextColor),
                   axis.text.x = element_text(size=24, color=axisTextColor),
                   axis.text.y = element_text(size=24, color=axisTextColor),
                   plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
                   plot.title = element_text(size = 20, color=axisTextColor)) + 
            
             ylim( c( ifelse( min(dif.judgeAB, lowerLimit) < 0, -limit.y, limit.y) - 0.1 * limit.y, limit.y + 0.1 * limit.y) ) +
            
             # main line, the average
             geom_hline(yintercept = dif.mean, color = mainLineColor, linetype ="solid", size = .8) +
             geom_text(aes(x=0.2, y=dif.mean), vjust=1.3, hjust=0,
                       label = paste("bias: ", round(dif.mean, 2), ifelse(tTest$p.value < 0.05,"*", "")),
                       family=fontFamily, color=axisTextColor,
                       size=7, data = data.frame()) +
            
             # lower limit
             geom_hline(yintercept = lowerLimit, color = lineLimitColor, linetype ="longdash", size = .8) +
             geom_text(aes(x=0.2, y=lowerLimit), vjust=1.2, hjust=0, 
                       label = paste("lower: ", round(lowerLimit, 2)), 
                       family=fontFamily, color=axisTextColor,
                       size=7, data = data.frame()) +
            
             # upper limit
             geom_hline(yintercept = upperLimit, color = lineLimitColor, linetype ="longdash", size = .8) +
             geom_text(aes(x=0.2, y=upperLimit + 0.5), vjust=0, hjust=0,
                       label = paste("upper: ", round(upperLimit, 2)), 
                       family=fontFamily, color=axisTextColor,
                       size=7, data = data.frame())

   plot_grid(disp, blalt, nrow=1, ncol=2)
}  


#--------------------------------------------
# Example of using
#--------------------------------------------

# load the data sample from some sessions of data collect
df <- read.csv(file = 'data-collected.csv', header = TRUE, sep = ";")

#--------------------------------------------
# get the data from session 1
df.session1 = df[which(df$session == 1), ]
# add the overall sum of questions per line
df.session1["total"] <- apply(df.session1[,4:ncol(df.session1)], 1, sum)

# build a df with an adjusted  columns
df.adjusted1 <- data.frame( "eva1" = df.session1[which(df.session1$evaluator == "e1"), 16],
                            "eva2" = df.session1[which(df.session1$evaluator == "e2"), 16] )

# add mean and standard deviation
df.adjusted1$mean <- apply(df.adjusted1, 1, mean)
df.adjusted1$sd <- apply(df.adjusted1, 1, sd)


#---------------------------------------------
# get the data from session 2
df.session2 = df[which(df$session == 2), ]
# add the overall sum of questions per line
df.session2["total"] <- apply(df.session2[,4:ncol(df.session2)], 1, sum)

# build a df with an adjusted  columns
df.adjusted2 <- data.frame( "eva1" = df.session2[which(df.session2$evaluator == "e1"), 16],
                            "eva2" = df.session2[which(df.session2$evaluator == "e2"), 16] )

# add mean and standard deviation
df.adjusted2$mean <- apply(df.adjusted2, 1, mean)
df.adjusted2$sd <- apply(df.adjusted2, 1, sd)

#-------------------------------------------------
# Build the charts  
#-------------------------------------------------
x <- scatterBAChart(df.adjusted1$eva1, df.adjusted1$eva2, "Evaluator 1", "Evaluator 2", "- Session 1")
y <- scatterBAChart(df.adjusted2$eva1, df.adjusted2$eva2, "Evaluator 1", "Evaluator 2", "- Session 2") 
plot_grid(x, y, nrow = 2, ncol = 1) 


#---------------------------------------------
# Kendall's coefficient of concordance  
#---------------------------------------------
# separate the evaluators columns [1:2]
data <- data.frame(sapply(df.adjusted1[,1:2], function(x) as.numeric(as.character(x))))
sapply(data, class)

# Coefficient of agreement
kendall(data, correct = TRUE)

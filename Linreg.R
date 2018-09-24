library(ggplot2)
library(extrafont)

getInput = function(message)
{
  userInput = readline(prompt=message)
  return(userInput)
}

filename = getInput("Filename: ")
path = paste("file:///C:/Users/John/Desktop/", filename, ".csv", sep="")
input = read.csv(file=path)

xValues = input[1][,]
yValues = input[3][,]
yUncertainty = input[4][,]
yMax = yValues + yUncertainty
yMin = yValues - yUncertainty
#errorBarWidth = round(max(xValues)*0.011)

chartTitle = getInput("Chart Title: ")
autoPlot = getInput("Plot Automatically? (Y/N) ")
if (tolower(autoPlot) == "y" || tolower(autoPlot) == "yes") {
  xTitle = colnames(input[1])
  yTitle = colnames(input[3])
  xScaleMax = max(xValues) * 1.05
  yScaleMax = max(yValues) * 1.05
} else {
  xTitle = getInput("Horizontal Axis Title: ")
  yTitle = getInput("Vertical Axis Title: ")
  xScaleMax = as.numeric(getInput("Horizontal Scale: "))
  yScaleMax = as.numeric(getInput("Vertical Scale: "))
}

# Position of slope and intercept information
xAnnotationPosition = (xScaleMax * 0.05)
yAnnotationPosition = (yScaleMax * 0.98)

# Stats
stats = summary(lm(yValues ~ xValues))
slope = coef(stats)[2]
slopeUncertainty = (coef(stats)[4] * 2)
intercept = coef(stats)[1]
interceptUncertainty = (coef(stats)[3] * 2)
r2 = stats$r.squared
coef(stats)

# Min-Max Lines
pointX1 = xValues[1]
pointY1Min = yValues[1] - yUncertainty[1]
pointY1Max = yValues[1] + yUncertainty[1]

pointX2 = xValues[length(xValues)]
pointY2Min = yValues[length(yValues)] - yUncertainty[length(yUncertainty)]
pointY2Max = yValues[length(yValues)] + yUncertainty[length(yUncertainty)]

minLineSlope = (pointY2Max - pointY1Min) / (pointX2 - pointX1)
minLineB = pointY2Max - (minLineSlope * pointX2)
minLine = function(x){minLineSlope*x + minLineB}

maxLineSlope = (pointY2Min - pointY1Max) / (pointX2 - pointX1)
maxLineB = pointY2Min - (maxLineSlope * pointX2)
maxLine = function(x){maxLineSlope*x + maxLineB}


slopeUnits = getInput("Slope Units: ")
interceptUnits = getInput("Intercept Units: ")

# Slope and Intercept
equation = paste("Slope:\n", format(slope, nsmall=1, digits=1),
                 "±", format(slopeUncertainty, nsmall=1, digits=1), " ", slopeUnits, "\n",
                 "Intecept:\n", format(intercept, nsmall=1, digits=1),
                 "±", format(interceptUncertainty, nsmall=1, digits=1), " ", interceptUnits, "\n", sep="")

# r^2
#equation = paste(equation, "r^2:\n", format(r2, nsmall=2, digits=2), "\n", sep="")

# Equation Override
override = FALSE
if (override) {
  print("Equation override is enabled")
  equation = paste("Slope:\n1.46±0.06 \n", "Intercept:\n0.01±0.02\n", sep="")
}


# Function Plot
ggplot(input, aes(x=xValues, y=yValues)) +
  geom_point() +
  stat_function(fun=minLine, colour="grey", linetype=2) +
  stat_function(fun=maxLine, colour="grey", linetype=2) +
  geom_errorbar(aes(ymin=yMin, ymax=yMax), width=1) +
  geom_abline(intercept=intercept, slope=slope) +
  scale_x_continuous(limits=c(0, xScaleMax), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, yScaleMax), expand=c(0, 0)) +
  annotate("text", label=equation, family="Roboto",
           x=xAnnotationPosition, y=yAnnotationPosition,
           hjust=0, vjust=1) +
  xlab(xTitle) +
  ylab(yTitle) +
  labs(title=chartTitle) +
  theme_bw() +
  theme(text=element_text(family="Roboto"))

# Exporting
ggsave(paste(filename, ".png", sep=""), limitsize=FALSE, units="in", width=9, height=6)

library(shiny)
library("shinyWidgets")
library(ggplot2)
## store the data
## data("iris")
species = iris$Species
spl = iris$Sepal.Length
spw = iris$Sepal.Width
ptl = iris$Petal.Length
ptw = iris$Petal.Width
unq_spcs = unique(species)

## computing the stats

## sepal length to width
sepal_ratio = spl / spw

## petal length to width
petal_ratio = ptl / ptw

avg_spl = mean(spl)
avg_spw = mean(spw)
avg_ptl = mean(ptl)
avg_ptw = mean(ptw)

## setosa diagrams
setosa = iris[species == unq_spcs[1], ]
sts_avg_spl = mean(setosa$Sepal.Length)
sts_avg_spw = mean(setosa$Sepal.Width)
sts_avg_ptl = mean(setosa$Petal.Length)
sts_avg_ptw = mean(setosa$Petal.Width)

## histogram for setosa sepal length
hist(setosa$Sepal.Length, breaks = 8:16 / 2, xlim = c(4, 8))

## histogram for setosa sepal.width
hist(setosa$Sepal.Width, breaks = 4:10 / 2, xlim = c(2, 5))

## setosa sepal length width ratio
setosa_sepal = cbind(setosa$Sepal.Length, setosa$Sepal.Width)
sts_sepal_ratio = apply(setosa_sepal, 1, function(x) x[1]/x[2])
sts_sepal_length = setosa$Sepal.Length
sts_model1 = lm(data = setosa, formula = sts_sepal_ratio~sts_sepal_length)

## versicolor diagrams
versicolor = iris[species == unq_spcs[2], ]
vsc_avg_spl = mean(versicolor$Sepal.Length)
vsc_avg_spw = mean(versicolor$Sepal.Width)
vsc_avg_ptl = mean(versicolor$Petal.Length)
vsc_avg_ptw = mean(versicolor$Petal.Width)

## histogram for versicolor sepal length
hist(versicolor$Sepal.Length, breaks = 8:16 / 2, xlim = c(4, 8))

## histogram for versicolor sepal.width
hist(versicolor$Sepal.Width, breaks = 4:10 / 2, xlim = c(2, 5))

## versicolor sepal length width ratio
versicolor_sepal = cbind(versicolor$Sepal.Length, versicolor$Sepal.Width)
vsc_sepal_ratio = apply(versicolor_sepal, 1, function(x) x[1]/x[2])
vsc_sepal_length = versicolor$Sepal.Length
vsc_model1 = lm(data = versicolor, formula = vsc_sepal_ratio~vsc_sepal_length)


## virginica diagrams
virginica = iris[species == unq_spcs[3], ]
vgc_avg_spl = mean(virginica$Sepal.Length)
vgc_avg_spw = mean(virginica$Sepal.Width)
vgc_avg_ptl = mean(virginica$Petal.Length)
vgc_avg_ptw = mean(virginica$Petal.Width)

## histogram for virginica sepal length
hist(virginica$Sepal.Length, breaks = 8:16 / 2, xlim = c(4, 8))

## histogram for virginica sepal.width
hist(virginica$Sepal.Width, breaks = 4:10 / 2, xlim = c(2, 5))

## virginica sepal length width ratio
virginica_sepal = cbind(virginica$Sepal.Length, virginica$Sepal.Width)
vgc_sepal_ratio = apply(virginica_sepal, 1, function(x) x[1]/x[2])
vgc_sepal_length = virginica$Sepal.Length
vgc_model1 = lm(data = virginica, formula = vgc_sepal_ratio~vgc_sepal_length)

model_sepal = lm(data = iris, formula = sepal_ratio ~ sepallength)
model_petal = lm(data = iris, formula = petal_ratio ~ petallength)

sepal_avgs <- c(sts_avg_spl, sts_avg_spw, vsc_avg_spl, vsc_avg_spw, vgc_avg_spl, vgc_avg_spw)
sepal_class <- c("setosa sepal length", "setosa sepal width", "versicolor sepal length", "versicolor sepal width", "virginica sepal length", "versicolor sepal width")

petal_avgs <- c(sts_avg_ptl, sts_avg_ptw, vsc_avg_ptl, vsc_avg_ptw, vgc_avg_ptl, vgc_avg_ptw)
petal_class <- c("setosa petal length", "setosa petal width", "versicolor petal length", "versicolor petal width", "virginica petal length", "versicolor petal width")

## get the top 50 individuals with highest length to width ratio
temp = cbind(iris, petal_ratio)
temp = temp[order(temp$petal_ratio),]
temp = head(temp, 50)
top50petal = c(length(temp[temp$Species == "setosa",1]), length(temp[temp$Species == "versicolor", 1]), length(temp[temp$Species == "virginica", 1]))
group = unique(iris$Species)

temp = cbind(iris, sepal_ratio)
temp = temp[order(temp$sepal_ratio),]
temp = head(temp, 50)
top50sepal = c(length(temp[temp$Species == "setosa", 1]), length(temp[temp$Species == "versicolor", 1]), length(temp[temp$Species == "virginica", 1]))

top_50petal = data.frame(
  Spc = group,
  number_in_top50 = top50petal
)

top_50sepal = data.frame(
  Spc = group,
  number_in_top50 = top50sepal
)


ui <- fluidPage(
  actionButton(inputId = "sepal", label = "Sepal scatter plot"),
  plotOutput("sepal_plot",
             click = "plot_click1",
             brush = "plot_brush1"),
  verbatimTextOutput("info1"),  
  
  actionButton(inputId = "petal", label = "Petal scatter plot"),
  plotOutput("petal_plot",
             click = "plot_click2",
             brush = "plot_brush2"),
  verbatimTextOutput("info2"),
  
  sliderTextInput(
    inputId = "size",
    label = "comparison of the average size",
    choices = c("sepal", "petal")
  ),
  plotOutput("draw"),
  
  sliderTextInput(
    inputId = "ratio",
    label = "top 50 individuals with highest length to width ratio",
    choices = c("sepal", "petal")
  ),
  plotOutput("extra")
)



server <- function(input, output) {

  
  ##data = eventReactive(input$sts, {setosa$Sepal.Length})
  output$draw <- renderPlot({
    if (input$size == "sepal") {
      barplot(height = sepal_avgs, names.arg = sepal_class, main = "difference of sepal size among species")
    } else {
      barplot(height = petal_avgs, names.arg = petal_class, main = "difference of petal size among species")
    }
  })
  
  
  output$extra <- renderPlot({
    if (input$ratio == "sepal") {
      ggplot(top_50sepal, aes(x="", y=number_in_top50, fill=Spc)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
    } else {
      ggplot(top_50petal, aes(x="", y=number_in_top50, fill=Spc)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
    }
  })

  
  output$sepal_plot <- renderPlot({
    if (input$sepal %% 2 == 1) {
      p <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=species, shape = species), main = "scatter plot for sepal size")+geom_point(size=3)
      p + ggtitle("scatter plot for sepal size")
      ##class = unique(species)
      ##displ = c(sts_avg_ptl, vsc_avg_ptl, vgc_avg_ptl)
      ##barplot(displ)
      ##hist(setosa$Sepal.Width, breaks = 4:10 / 2, xlim = c(2, 5), main = "Setosa Sepal width distribution")
    } 
  })
  
  output$info1 <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    if (input$sepal %% 2 == 1) {
      paste0(
        "scatter plot for sepal size: \n",
        "click: ", xy_str(input$plot_click1),
        "brush: ", xy_range_str(input$plot_brush1)
      )
    }
  })
  
  output$petal_plot <- renderPlot({
    if (input$petal %% 2 == 1) {
      p <- ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, color=species, shape = species))+geom_point(size=3)
      p + ggtitle("scatter plot for petal size")
      } 
  })
  
  output$info2 <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    if (input$petal %% 2 == 1) {
      paste0(
        "scatter plot for petal size: \n",
        "click: ", xy_str(input$plot_click2),
        "brush: ", xy_range_str(input$plot_brush2)
      )
    }
  })
}

shinyApp(ui, server)

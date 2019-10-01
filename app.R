library(shiny)
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
sts_sepal_ratio = mean(apply(setosa_sepal, 1, function(x) x[1]/x[2]))


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
vsc_sepal_ratio = mean(apply(versicolor_sepal, 1, function(x) x[1]/x[2]))


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
vgc_sepal_ratio = mean(apply(virginica_sepal, 1, function(x) x[1]/x[2]))


ui <- fluidPage(
  plotOutput("plot1",
             click = "plot_click",
             dblclick = "plot_dblclick",
             hover = "plot_hover",
             brush = "plot_brush"
  ),
  actionButton(inputId = "sepal", label = "Sepal scatter plot"),
  actionButton(inputId = "patel", label = "Petal scatter plot"),
  verbatimTextOutput("info"),
  plotOutput("sepal_plot"),
  plotOutput("patel_plot")
)



server <- function(input, output) {

  
  ##data = eventReactive(input$sts, {setosa$Sepal.Length})
  
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
  
  output$patel_plot <- renderPlot({
    if (input$patel %% 2 == 1) {
      p <- ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, color=species, shape = species))+geom_point(size=3)
      p + ggtitle("scatter plot for patel size")
      } 
  })
  
  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
}

shinyApp(ui, server)

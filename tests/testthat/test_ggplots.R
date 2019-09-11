context("ggplots")

data("mtcars")
mtcars$cyl <- factor(mtcars$cyl)

test_that("FT boxplot plots correctly", {
  vdiffr::expect_doppelganger("one-way boxplot", 
    iNZightPlotGG(mtcars, "gg_boxplot", y = "hp", print = FALSE)
  )
  
  vdiffr::expect_doppelganger("two-way boxplot", 
    iNZightPlotGG(mtcars, "gg_boxplot", x = "cyl", y = "hp", print = FALSE)
  )
  
  vdiffr::expect_doppelganger("boxplot fill colour", 
    iNZightPlotGG(mtcars, "gg_boxplot", y = "hp", fill = "darkred", print = FALSE)  
  )
  
  vdiffr::expect_doppelganger("boxplot line width", 
    iNZightPlotGG(mtcars, "gg_boxplot", y = "hp", extra_args = list(gg_lwd = 2), print = FALSE)
  )
})

test_that("FT density plots correctly", {
  vdiffr::expect_doppelganger("one-way density plot", 
    iNZightPlotGG(mtcars, "gg_density", y = "hp", print = FALSE)                      
  )
  
  vdiffr::expect_doppelganger("two-way density plot", 
    iNZightPlotGG(mtcars, "gg_density", x = "cyl", y = "hp", print = FALSE)                      
  )
  
  vdiffr::expect_doppelganger("density plot fill colour", 
    iNZightPlotGG(mtcars, "gg_density", y = "hp", fill = "darkred", print = FALSE)                      
  )
  
  vdiffr::expect_doppelganger("one-way density plot transparency", 
    iNZightPlotGG(mtcars, "gg_density", y = "hp", extra_args = list(alpha = 0.5), print = FALSE)                      
  )
  
  vdiffr::expect_doppelganger("two-way density plot transparency", 
    iNZightPlotGG(mtcars, "gg_density", x = "cyl", y = "hp", extra_args = list(alpha_densitygroup = 0.5), print = FALSE)                      
  )
  
  vdiffr::expect_doppelganger("two-way density plot transparency ignores one-way alpha", 
    iNZightPlotGG(mtcars, "gg_density", x = "cyl", y = "hp", extra_args = list(alpha = 1.0, alpha_densitygroup = 0.5), print = FALSE)                      
  )
  
  vdiffr::expect_doppelganger("density plot smoothness", 
    iNZightPlotGG(mtcars, "gg_density", y = "hp", extra_args = list(adjust = 2), print = FALSE)                      
  )
})

test_that("FT pie plots correctly", {
  vdiffr::expect_doppelganger("pie plot",
    iNZightPlotGG(mtcars, "gg_pie", fill = "cyl", print = FALSE)
  )
})

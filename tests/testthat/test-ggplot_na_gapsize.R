context("ggplot_na_gapsize")

test_that("Check that plot is running without error",
          {
            expect_true( is.recursive(ggplot_na_gapsize(tsAirgap)))
            expect_true( is.list(ggplot_na_gapsize(tsAirgap)))
            expect_true( is.list(ggplot_na_gapsize(tsAirgap, ranked_by = "total")))
            expect_true( is.list(ggplot_na_gapsize(tsNH4, limit = 2)))
            expect_true( is.list(ggplot_na_gapsize(tsAirgap, legend = F)))
            expect_true( is.list(ggplot_na_gapsize(tsAirgap, orientation =  "horizontal")))
            expect_true( is.list(ggplot_na_gapsize(tsAirgap, include_total = F)))
            expect_true( is.list(ggplot_na_gapsize(tsAirgap, color_occurrence = "blue")))

            ## input not univariate
            x <- data.frame(
              x = runif(10,0,10),
              y = runif(10,0,10)
            )
            expect_error(ggplot_na_gapsize(x))
            
            ## input not numeric
            x <- c("a", 1, 2, 3)
            expect_error(ggplot_na_gapsize(x))
            
            ## No NA values
            x <- 1:10
            expect_error(ggplot_na_gapsize(x))
            
          })


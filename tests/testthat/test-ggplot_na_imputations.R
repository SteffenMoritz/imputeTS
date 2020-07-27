context("ggplot_na_imputations")

test_that("Check that plot is running without error",
          {

            expect_true( is.list(ggplot_na_imputations(tsAirgap,tsAirgapComplete)))
            expect_true( is.list(ggplot_na_imputations(tsAirgap,na_mean(tsAirgap), tsAirgapComplete)))
            expect_true( is.recursive(ggplot_na_imputations(tsAirgap,tsAirgapComplete)))
            expect_true( is.recursive(ggplot_na_imputations(tsAirgap,na_mean(tsAirgap), tsAirgapComplete)))

            
            ## input not univariate
            x <- data.frame(
              x = runif(10,0,10),
              y = runif(10,0,10)
            )
            expect_error(ggplot_na_imputations(x, tsAirgapComplete))
            expect_error(ggplot_na_imputations(tsAirgapComplete, x))
            expect_error(ggplot_na_imputations(tsAirgap, tsAirgapComplete, x))
            
            ## input not numeric
            x <- c("a", 1, 2, 3)
            expect_error(ggplot_na_imputations(x, tsAirgapComplete))
            expect_error(ggplot_na_imputations(tsAirgapComplete, x))
            expect_error(ggplot_na_imputations(tsAirgap, tsAirgapComplete, x))
            
          })

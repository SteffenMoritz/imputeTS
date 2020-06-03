context("statsNA")


test_that("Test that function works and prints output",
          {
            expect_that( statsNA(tsAirgap, print_only = T), prints_text())
          })


test_that("Test results of function",
          {
            expect_that(statsNA(tsAirgap, print_only = F)$numberNAs, equals(13))
            expect_that(statsNA(tsAirgap, print_only = F)$naGapLongest, equals(3))
            expect_that(statsNA(tsAirgap, print_only = F)$naGapMostOverallNAs, equals(1))
            expect_that(statsNA(tsAirgap, print_only = F)$naGapMostFrequent, equals(1))
            expect_that(statsNA(tsAirgap, print_only = F)$lengthTimeSeries, equals(144))
            
          })

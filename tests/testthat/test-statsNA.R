context("statsNA")


test_that("Test that function works and prints output",
          {
            expect_that( statsNA(tsAirgap, printOnly = T), prints_text())
          })


test_that("Test results of function",
          {
            expect_that(statsNA(tsAirgap, printOnly = F)$numberNAs, equals(13))
            expect_that(statsNA(tsAirgap, printOnly = F)$naGapLongest, equals(3))
            expect_that(statsNA(tsAirgap, printOnly = F)$naGapMostOverallNAs, equals(1))
            expect_that(statsNA(tsAirgap, printOnly = F)$naGapMostFrequent, equals(1))
            expect_that(statsNA(tsAirgap, printOnly = F)$lengthTimeSeries, equals(144))
            
          })

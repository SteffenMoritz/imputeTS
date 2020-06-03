context("statsNA")


test_that("Test that function works and prints output",
          {
            expect_that( statsNA(tsAirgap, print_only = T), prints_text())
          })


test_that("Test results of function",
          {
            expect_that(statsNA(tsAirgap, print_only = F)$number_NAs, equals(13))
            expect_that(statsNA(tsAirgap, print_only = F)$longest_na_gap, equals(3))
            expect_that(statsNA(tsAirgap, print_only = F)$most_weighty_na_gap, equals(1))
            expect_that(statsNA(tsAirgap, print_only = F)$most_frequent_na_gap, equals(1))
            expect_that(statsNA(tsAirgap, print_only = F)$length_series, equals(144))
            expect_that(statsNA(tsAirgap, print_only = F)$number_na_gaps, equals(11))
            expect_that(statsNA(tsNH4, print_only = F)$average_size_na_gaps, equals(5.696774))
          })

#context("Coverage")
library(testthat)
library(rfinterval)

#test_check("rfinterval")

BeijingPM25 <- BeijingPM25[sample.int(n=nrow(BeijingPM25), nrow(BeijingPM25)), ]

#devtools::use_data(BeijingPM25, overwrite = TRUE)

output <- rfinterval(pm2.5~.,
                     train_data = BeijingPM25[1:1000, ],
                     test_data = BeijingPM25[1001:2000, ],
                     method = c("oob", "split-conformal", "quantreg"),
                     symmetry = TRUE,
                     seed = 2019,
                     alpha = 0.1)

y <- BeijingPM25[1001:2000, "pm2.5"]
oob_coverage <- mean(output$oob_interval$lo < y & output$oob_interval$up > y)
sc_coverage <- mean(output$sc_interval$lo < y & output$sc_interval$up > y)
quantreg_coverage <- mean(output$quantreg_interval$lo < y & output$quantreg_interval$up > y)

test_that("Check coverage",{
  expect_true(oob_coverage>0.1)
  expect_true(sc_coverage>0.1)
  expect_true(quantreg_coverage>0.1)
})


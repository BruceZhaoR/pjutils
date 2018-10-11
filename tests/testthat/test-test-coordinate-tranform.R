context("test-test-coordinate-tranform")

# WGS84:google earth: 31.1601435400,121.4295896700
# GCJ02:google map: 31.1582660190,121.4342191628
# GCJ02:gaode/tencent: 31.1582470000,121.4342040000
# BD09:baidu map: 31.1646093426,121.4406138425

wgs_lon <- 121.4295896700
wgs_lat <- 31.1601435400

test_that("wgs2gcj works",{
  tmp <- wgs2gcj(wgs_lon, wgs_lat)
  expect_true(abs(tmp[[1]] - 121.4342191628) < 0.00001)
  expect_true(abs(tmp[[2]] - 31.1582660190) < 0.00001)
})

test_that("wgs2bd works",{
  tmp <- wgs2bd(wgs_lon, wgs_lat)
  expect_true(abs(tmp[[1]] - 121.4406138425) < 0.00001)
  expect_true(abs(tmp[[2]] - 31.1646093426) < 0.00001)
})


context("test-coordinate-tranform")

# WGS84:google earth: 31.1601435400,121.4295896700
# GCJ02:google map: 31.1582660190,121.4342191628
# GCJ02:gaode/tencent: 31.1582470000,121.4342040000
# BD09:baidu map: 31.1646093426,121.4406138425

wgs_lon <- 121.4295896700
wgs_lat <- 31.1601435400
gcj_lon <- 121.4342191628
gcj_lat <- 31.1582660190
bd_lon <- 121.4406138425
bd_lat <- 31.1646093426

test_that("coordinate-tranform works",{
  tmp <- wgs2gcj(wgs_lon, wgs_lat)
  expect_true(abs(tmp[[1]] - gcj_lon) < 0.00001)
  expect_true(abs(tmp[[2]] - gcj_lat) < 0.00001)

  tmp <- wgs2bd(wgs_lon, wgs_lat)
  expect_true(abs(tmp[[1]] - bd_lon) < 0.00001)
  expect_true(abs(tmp[[2]] - bd_lat) < 0.00001)

  tmp <- gcj2wgs(gcj_lon, gcj_lat)
  expect_true(abs(tmp[[1]] - wgs_lon) < 0.0001)
  expect_true(abs(tmp[[2]] - wgs_lat) < 0.0001)

  tmp <- gcj2bd(gcj_lon, gcj_lat)
  expect_true(abs(tmp[[1]] - bd_lon) < 0.00001)
  expect_true(abs(tmp[[2]] - bd_lat) < 0.00001)

  tmp <- bd2wgs(bd_lon, bd_lat)
  expect_true(abs(tmp[[1]] - wgs_lon) < 0.0001)
  expect_true(abs(tmp[[2]] - wgs_lat) < 0.0001)

  tmp <- bd2gcj(bd_lon, bd_lat)
  expect_true(abs(tmp[[1]] - gcj_lon) < 0.00001)
  expect_true(abs(tmp[[2]] - gcj_lat) < 0.00001)

  expect_true(out_of_china(1,1))
  expect_true(out_of_china(121,0))
  df <- data.frame(wgs_lon, wgs_lat, bd_lon, bd_lat)
  expect_silent(fmt_gps_json(df))
})


context("test-cal-gps-dist")

test_that("cal-gps-dist works", {
  expect_equal(0, cal_gps_dist(121.434174,31.158213,121.434174,31.158213))
  # 1.58km, earth two points distance
  expect_equal(1.58, round(cal_gps_dist(121.434174,31.158213,121.444366,31.169403) / 1000, 2))
})

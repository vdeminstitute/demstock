# test locf
test_that("locf", {
    x <- data.frame(
        a = c(1, NA, NA, 2))
    y <- data.frame(
        a = c(1, 1, 1, 2))
    out <- locf(x$a)

    expect_equal(out, y$a)

    # Sanity check that we cloned our object
    expect_false(identical(out, x$a))
})

# test subset_data
test_that("subset_data", {
    out <- subset_data(vdemdata::vdem, var = c("v2x_polyarchy", "v2x_libdem"), add = c("histname", "country_name"))   
    expect_true(is.data.frame(out))
    
    # check for correct subsetting
    expect_true(all(c("country_id", "year", "v2x_polyarchy", "v2x_libdem", "histname", "country_name") %in% names(out)))

    # check for correct filtering of historical states
    expect_false(any(out$country_id %in% c(349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,373)))
})

# test expand_data
test_that("expand_data", {
    input <- subset_data(vdemdata::vdem, var = "v2x_polyarchy", add = "histname")
    out <- expand_data(input)
    expect_true(is.data.frame(out))

    # check for correct expansion
    expect_false(nrow(out) == nrow(input))
    expect_equal(nrow(out), length(unique(input$country_id)) * length(unique(input$year)))
})

# test create_stock_id
test_that("create_stock_id", {
    input <- expand_data(subset_data(vdemdata::vdem, var = "v2x_polyarchy", add = "histname"))
    out <- create_stock_id(input, stock_tt)
    expect_true(is.data.frame(out))

    # check for correct creation of stock_id
    expect_true("stock_id" %in% names(out))
    expect_false(anyNA(out$stock_id))
    expect_false(anyNA(out$country_id))
    expect_equal(nrow(out), nrow(input))
})

# test create_antecedence
test_that("create_antecedence", {
    input <- create_stock_id(expand_data(subset_data(vdemdata::vdem, var = "v2x_polyarchy", add = "histname")), stock_tt)
    out <- create_antecedence(input, var = "v2x_polyarchy", stock_tt)
    expect_true(is.data.frame(out))

    # check for correct creation of antecedence measure
    expect_true("v2x_polyarchy_stock" %in% names(out))
    expect_false(anyNA(out$country_id))
    expect_equal(nrow(out), nrow(input))
    
})

# test fill_data
test_that("fill_data", {
  input <- create_antecedence(create_stock_id(expand_data(subset_data(
    vdemdata::vdem, var = "v2x_polyarchy", add = "histname")), stock_tt), var = "v2x_polyarchy", stock_tt)

  out0 <- fill_data(input, var = "v2x_polyarchy", fill = 0)
  out5 <- fill_data(input, var = "v2x_polyarchy", fill = 5)

  # check that correctly returns expected structure
  expect_true(is.data.frame(out0))
  expect_true(is.data.frame(out5))
  expect_equal(nrow(out0), nrow(input))
  expect_equal(nrow(out5), nrow(input))
  expect_true("v2x_polyarchy_stock" %in% names(out0))
  expect_true("v2x_polyarchy_stock" %in% names(out5))

  # Filling forward should not increase NA count
  expect_lte(sum(is.na(out5$v2x_polyarchy_stock)), sum(is.na(out0$v2x_polyarchy_stock))) 
})

# test calc_stock
test_that("calc_stock", {
  input <- fill_data(create_antecedence(create_stock_id(expand_data(subset_data(
    vdemdata::vdem, var = "v2x_polyarchy", add = "histname")), stock_tt), var = "v2x_polyarchy", stock_tt), var = "v2x_polyarchy", fill = 5)

  out <- calc_stock(input, var = "v2x_polyarchy", val = 0.95)

  # check structure
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), nrow(input))
  expect_true("v2x_polyarchy_stock_raw" %in% names(out))
  expect_true("v2x_polyarchy_stock_weighted" %in% names(out))

  # check for stock values (not all NA) and that weighted is correctly calculated from raw
  idx <- which(!is.na(out$v2x_polyarchy_stock_raw) & !is.na(out$v2x_polyarchy_stock_weighted))
  expect_true(length(idx) > 0)
  expect_equal(
    out$v2x_polyarchy_stock_weighted[idx],
    out$v2x_polyarchy_stock_raw[idx] * (1 - 0.95), tolerance = 1e-8)
})

# test assign_names
test_that("assign_names", {
  if (exists("test_name", envir = .GlobalEnv)) rm(list = "test_name", envir = .GlobalEnv)

  x1 <- data.frame(a = 1)
  x2 <- data.frame(a = 2)

  # creates and overwrites object in global env
  assign_names(x1, "test_name")
  expect_true(exists("test_name", envir = .GlobalEnv))
  expect_equal(get("test_name", envir = .GlobalEnv), x1)

  # correctly overwrites with warning
  expect_warning(assign_names(x2, "test_name"), "already exists")
  expect_equal(get("test_name", envir = .GlobalEnv), x2)

  rm(list = "test_name", envir = .GlobalEnv)
})
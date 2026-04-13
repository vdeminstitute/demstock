
# get_stock tests
test_that("test correct output", {
    # test for expected output
    get_stock(var = "v2x_libdem", add = c("histname", "v2x_suffr"), val = 0.99, fill = 5, name = "vdem_out")
    expect_true(is.data.frame(vdem_out))
    expect_true(all(c("country_id", "country_text_id", "year", "v2x_libdem", "v2x_libdem_stock_raw", "v2x_libdem_stock_weighted", "histname", "v2x_suffr") %in% names(vdem_out)))  
    expect_true(exists("vdem_out"))

    out <- get_stock(var = c("v2x_delibdem", "v2csgender"), add = c("v2eltype_6"), val = 0.75, fill = 8)
    expect_true(is.data.frame(out))
    expect_true(all(c("country_id", "country_text_id", "year", "v2x_delibdem", "v2csgender", "v2eltype_6", "v2x_delibdem_stock_raw", "v2x_delibdem_stock_weighted", "v2csgender_stock_raw", "v2csgender_stock_weighted") %in% names(out)))
    expect_true(exists("out"))

})

test_that("test unexpected input", {
    # test for unexpected input
    expect_error(get_stock(var = c("v2x_libdem", "v2x_notvar")))
    expect_error(get_stock(var = "v2x_libdem", add = c("histname", "notvar")))
    expect_error(get_stock(var = "v2x_libdem", val = -0.5))
    expect_error(get_stock(var = "v2x_libdem", val = 1.5))
    expect_error(get_stock(var = "v2x_libdem", fill = -1))
    expect_error(get_stock(var = "v2x_libdem", val = "0.5"))
    expect_error(get_stock(var = "v2x_libdem", fill = -1))
    expect_error(get_stock(var = "v2x_libdem", fill = 2.5))
    expect_error(get_stock(var = "v2x_libdem", fill = "5"))
    expect_error(get_stock(var = "v2x_libdem", name = 5))
    expect_error(get_stock(var = "v2x_libdem", name = name_file))

    vdem_out <- data.frame()
    expect_warning(get_stock(var = "v2x_libdem", name = "vdem_out"))
})

test_that("test error messages", {
    expect_error(get_stock(var = c("v2x_libdem", "v2x_notvar")), "var must be a variable in the V-Dem dataset.")
    expect_error(get_stock(var = "v2x_libdem", add = c("histname", "notvar")), "all elements of add must be variables in the V-Dem dataset.")
    expect_error(get_stock(var = "v2x_libdem", val = -0.5), "val must be between 0 and 1.")
    expect_error(get_stock(var = "v2x_libdem", val = 1.5), "val must be between 0 and 1.")
    expect_error(get_stock(var = "v2x_libdem", fill = -1), "fill must be a single whole number greater than or equal to zero.")
    expect_error(get_stock(var = "v2x_libdem", fill = 2.5), "fill must be a single whole number greater than or equal to zero.")
    expect_error(get_stock(var = "v2x_libdem", fill = "5"), "fill must be a single whole number greater than or equal to zero.")
})
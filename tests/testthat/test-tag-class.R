test_that("tag class initialization errors when inputs are not valid", {
    # invalid category
    expect_error(Tag$new(category = "notexist"), regexp = "^Category notexist is not valid")

    # global category together with a value
    expect_error(Tag$new(category = "internal", value = "doesnotexist"), regexp = "^Value doesnotexist is not valid for category internal. Valid options are: \nNA")

    # invalid combination
    expect_error(Tag$new(category = "lc", value = "process"), regexp = "^Value process is not valid for category lc. Valid options are:.+?")


})

test_that("tag class initialization works", {
    # tag without value
    tag <- Tag$new(category = "internal")
    tag_df <- tag$to_tibble()
    expect_equal(tag_df$category, "internal")
    expect_equal(tag_df$value, NA)
    expect_equal(tag_df$tag_id, 8)

    # tag with value
    tag <- Tag$new(category = "data", value = "process")
    tag_df <- tag$to_tibble()
    expect_equal(tag_df$category, "data")
    expect_equal(tag_df$value, "process")
    expect_equal(tag_df$tag_id, 6)
})

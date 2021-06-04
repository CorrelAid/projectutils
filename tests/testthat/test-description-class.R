test_that("description class errors when inputs are not valid", {
    # invalid category
    description <- Description$new('2020-02-FOO')
    #missing german
    expect_error(description$problem <- list(en = "foo"), regexp = "Must be equal to set \\{'en','de'\\}")

    #one too much
    expect_error(description$problem <- list(en = "foo", de = "bar", fr = "foo"), regexp = "Must be equal to set \\{'en','de'\\}")

    #missing german
    expect_error(description$problem <- list(en = 1, de = "foo"), regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'numeric'.")

})

test_that("description class initialization works", {
    # description without value
    description <- Description$new('2020-02-FOO')
    description$problem  <- list(en = 'a problem', de = 'ein problem')
    description_df <- description$to_tibble()

    expect_equal(description_df$problem[[1]], list(en = 'a problem', de = 'ein problem'))

})

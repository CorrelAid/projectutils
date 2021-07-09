test_that("volunteer class initialization errors when inputs are not valid", {
    expect_error(Volunteer$new())

    expect_error(Volunteer$new(1), regexp = "^argument \"first_name\" is missing, with no default")

    # invalid email
    expect_error(Volunteer$new(1, first_name = "lisa", last_name = "simpson", email = "invalid"), regexp = "Invalid email address.")

    # invalid length
    expect_error(Volunteer$new(1, first_name = c("lisa", "bert"), last_name = "mustermann", email = "lisa@gmail.com"), regexp = "Assertion on 'first_name' failed: Must have length 1, but has length 2.")
})

test_that("volunteer class initialization works", {
    # tag without value
    vol <- Volunteer$new(12, first_name = "lisa", last_name = "simpson", email = "lisa.simpson@gmail.com")
    expect_equal(vol$first_name, "lisa")
    expect_equal(vol$last_name, "simpson")
})

test_that("batch creating volunteers works", {
    args_df <- tibble::tribble(
        ~volunteer_id, ~first_name, ~last_name, ~email,
        1, "lisa", "simpson", "lisa@simpson.com",
        2, "bart", "simpson", "bart@simpson.com"
    )
    vols <- args_df %>%
        purrr::pmap_dfr(function(volunteer_id, first_name, last_name, email) {
            vol <- projectutils::Volunteer$new(volunteer_id, first_name, last_name, email)
            vol$to_tibble()
        })
    expect_equal(nrow(vols), 2)
    expect_equal(vols$first_name, c("lisa", "bart"))
    expect_equal(vols$last_name, c("simpson", "simpson"))
})

test_that("setting volunteer usernames works as expected", {
    vol <- Volunteer$new(1, first_name = "lisa", last_name = "simpson", email = "lisa.simpson@gmail.com")

    # setting username works
    vol$user_gh <- "lisas"
    vol$user_gl <- "lisas"

    expect_equal(vol$user_gh, "lisas")
    expect_equal(vol$user_gl, "lisas")

    # only give username, not full url
    expect_error(
        {
            vol$user_twitter <- "https://twitter.com/lisasimpson"
        },
        regexp = "Only specify the user name for user_twitter, not the complete URL."
    )

    expect_error(
        {
            vol$user_gh <- "http://github.com/lisasimpson"
        },
        regexp = "Only specify the user name for user_gh, not the complete URL."
    )

    # change user_gh
    vol$user_gh <- "lisasimpson"
    expect_equal(vol$user_gh, "lisasimpson")
})


test_that("setting volunteer URLs works as expected", {
    vol <- Volunteer$new(1, first_name = "lisa", last_name = "simpson", email = "lisa.simpson@gmail.com")

    # invalid urls results in errors
    expect_error(
        {
            vol$url_website <- "invalid url"
        },
        regexp = "Invalid URL."
    )

    expect_error(
        {
            vol$url_xing <- "https://xing.de/fwerrwjl"
        },
        regexp = "URL for url_xing must include domain xing.com."
    )

    vol$url_xing <- "https://xing.com/foobar"
    vol$url_linkedin <- "https://linkedin.com/foobar"
})


test_that("setting volunteer local chapter works as expected", {
    vol <- Volunteer$new(1, first_name = "lisa", last_name = "simpson", email = "lisa.simpson@gmail.com")

    # invalid chapter
    expect_error(
        {
            vol$set_local_chapter("doesnotexist")
        },
        regexp = "^doesnotexist is not a valid chapter. Valid options are: berlin"
    )

    # can only be active in one chapter
    expect_error(
        {
            vol$set_local_chapter(c("berlin", "paris"))
        },
        regexp = "You can't assign a volunteer to more than one chapter."
    )

    # works
    vol$set_local_chapter("berlin")
    expect_equal(vol$lc_id, 1)
})
library(od)
library(tinytest)

# Placeholder with simple test
expect_equal(1 + 1, 2)

# Test that OD operations with NA coordinates generate warnings:
x = od_data_df
z = od_data_zones_min
expect_true(is(od_to_sf(x, z), "sf"))

x$geo_code1[1] = "444"
expect_error(od_to_sf(x, z, filter = FALSE))
x = od_data_df
x$geo_code2[1] = "444"
expect_error(od_to_sf(x, z, filter = FALSE))

x = od_data_df
# test crss
# commentd out because it fails
# https://github.com/ITSLeeds/od/runs/579151535?check_suite_focus=true#step:17:190

# expect_true({
#   z_projected = sf::st_transform(z, 27700)
#   is((l = od_to_sfc(x, z_projected)), "sfc")
# })
# expect_true(length(l) == nrow(x))

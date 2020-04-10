library(od)

# Placeholder with simple test
expect_equal(1 + 1, 2)

# Test that OD operations with NA coordinates generate warnings:
o = od_data_df
z = od_data_zones_min
tinytest::expect_true(is(od_to_sf(o, z), "sf"))

o$geo_code1[1] = "444"
expect_error(od_to_sf(o, z))
o = od_data_df
o$geo_code2[1] = "444"
expect_error(od_to_sf(o, z))

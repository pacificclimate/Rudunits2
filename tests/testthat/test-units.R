library("udunits2")

test_that("a unit system is loaded", {
	expect_equal(ud.have.unit.system(), TRUE)
})

test_that("ud.is.parseable determines whether a unit string is understandable", {
			expect_equal(ud.is.parseable("banana"), FALSE)
			expect_equal(ud.is.parseable("hour"), TRUE)
			expect_equal(ud.is.parseable("meter"), TRUE)
			expect_equal(ud.is.parseable("m"), TRUE)
})

test_that("ud.get.name returns canonical names for unit strings", {
			expect_equal(ud.get.name("kg"), ud.get.name("kilogram"))
			expect_equal(ud.get.name("degC"), ud.get.name("celsius"))
			expect_equal(ud.get.name("ohm"), "ohm")
			expect_error(ud.get.name("banana"))
})

test_that("ud.are.convertable determines convertable units", {
	expect_equal(ud.are.convertible("miles", "mi"), TRUE)	
	expect_equal(ud.are.convertible("miles", "feet"), TRUE)
	expect_equal(ud.are.convertible("miles", "cm"), TRUE)
	expect_equal(ud.are.convertible("miles", "gallon"), FALSE)
	expect_equal(ud.are.convertible("miles", "bananas"), FALSE)
	expect_equal(ud.are.convertible("bananas", "bananas"), FALSE)			
})

test_that("ud.convert converts values with units", {
	expect_equal(ud.convert(5, "miles", "mi"), 5)		
	expect_equal(ud.convert(5, "inches", "centimeters"), 12.7)
	expect_error(ud.convert(5, "bananas", "miles"))
	expect_error(us.convert(10, "minutes", "miles"))
})

test_that("ud.set.encoding sets values for non-ASCII characters", {
	ud.set.encoding("latin1")
	expect_equal(ud.get.symbol("ohm"), "")
	
	ud.set.encoding("utf8")
	expect_equal(ud.get.symbol("ohm"), "Ω")
})

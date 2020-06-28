
#**************************************
#    1. PVs          #####
#**************************************


#' Calculates actuarial PV for given age, initial benefit,
#' cola assumption, and mortality
#'
#' @param age_fn Current age
#' @param benefit_init Current benefit payment
#' @param cola_assumed Assumed future annual cola
#' @param i Discount rate
#' @param decrement Decrement table to use
#' @param age_max_ Maximum age
#'
#' @return Actuarial present value of future benefits
#' @export
#'
#' @examples
get_PVB_retiree <- function(age_fn, benefit_init, cola_assumed, i, decrement = df_decrement, age_max_ = age_max) {
	# calculates actuarial PV for given age, initial benefit,
	# cola assumption, and mortality

	# age_fn: current age
	# benefit_init: current benefit payment
	# cola_assumed: assumed future annual cola
	# i: discount rate

	nyear_ret <- age_max_ - age_fn + 1

	decrement_fn <- filter(decrement, age >= age_fn, ea == min(ea))$qxm.r

	PVB <- sum(((1 + i)^-(0:(nyear_ret - 1))) * c(1, cumprod(1-decrement_fn)[-nyear_ret]) * (benefit_init * (1 + cola_assumed)^(0:(nyear_ret-1))))
}



#' Function for calculating PVB for actives
#'
#' @param age_fn Current age
#' @param ea_fn entry age
#' @param age_ret Retirement age
#' @param benefit_init Benefit payment at retirement age (age_ret)
#' @param cola_assumed Assumed COLA
#' @param i Discount rate
#' @param decrement Decrement table to use
#' @param age_max_ Maximum age
#'
#' @return Actuarial present value of active members
#' @export
#'
#' @examples
get_PVB_active <- function(age_fn, ea_fn, age_ret, benefit_init, cola_assumed, i, decrement = df_decrement, age_max_ = age_max) {
	# calculates actuarial PV for an active plan member.

	# age_fn: current age
	# benefit_init: benefit payment at age age_ret
	# cola_assumed: assumed future annual cola
	# i: discount rate
	# decrement: decrement table. qxm is the mortality rate for retirees, qxT is the total separation rate for actives (mortality included)

	# 	mortality_ = mortality
	# 	age_max_ = age_max
	#   i <- 0.075

	nyear_ret <- age_max_ - age_ret + 1

	decrement_ret <- filter(decrement, age >= age_ret, ea == ea_fn)$qxm
	decrement_act <- filter(decrement, age>= age_fn, age< age_ret, ea == ea_fn)$qxT

	PVB <- sum(((1 + i)^-(0:(nyear_ret - 1))) * c(1, cumprod(1-decrement_ret)[-nyear_ret]) * (benefit_init * (1 + cola_assumed)^(0:(nyear_ret-1)))) *
		((1 + i)^-((age_ret - age_fn)) * prod(1-decrement_act))

}



#**************************************
# 2. Amortization Functions       #####
#**************************************

#' Amortization function with constant payment amounts at each period
#'
#' @param p principle
#' @param i interest rate
#' @param n amortization period
#' @param end Logical. If TRUE, payments are made at the end of periods
#'
#' @return A vector of annual amortization payments
#' @export
#'
#' @import magrittr
#' @examples
pmt <- function(p, i, n, end = FALSE){

	if(end) p <- p*(1 + i)
	a_n <- (1 - (1 + i)^(-n))/(1 - 1/(1 + i))
	pmt <- p / a_n
	return(pmt)
}




#' Amortization function with constant-growth payments at each period
#'
#' @param p Principal
#' @param i interest rate
#' @param n Amortization period
#' @param g Constant growth rate of payments
#' @param end Logical. If TRUE, payments are made at the end of periods
#'
#' @return A vector of annual amortization payments
#' @export
#'
#' @examples
gaip <- function(p, i, n, g, end = FALSE){
	if(end) p <- p*(1 + i)
	k <- (1 + g)/(1 + i)
	a_sn <- (1 - k^n )/(1 - k)
	pmt <- p/a_sn
	return(pmt)
}



#' Constant dollar amortization method
#'
#' @param p Principle
#' @param i interest rate
#' @param m Amortization period
#' @param end Logical. If TRUE, payments are made at the end of periods
#' @param skipY1 Logial. If TRUE, year-1 payment is 0
#'
#' @return A vector of annual amortization payments
#' @export
#'
#' @examples
amort_cd <- function(p, i, m, end = FALSE, skipY1 = FALSE){
	# skipY1: if TRUE, payment of year 1 is skipped and then amort. basis is paid off in n-1 non-zero payments.

	if(!skipY1) rep(pmt(p, i, m, end), m)
	else        c(0, rep(pmt(p*(1+i), i, m-1, end), m-1))
}




#' Constant percent amortization method
#'
#' @param p Principle
#' @param i interest rate
#' @param m Amortization period
#' @param g Constant growth rate of payments
#' @param end Logical. If TRUE, payments are made at the end of periods
#' @param skipY1 Logial. If TRUE, year-1 payment is 0
#'
#' @return A vector of annual amortization payments
#' @export
#'
#' @examples
amort_cp <- function(p, i, m, g, end = FALSE, skipY1 = FALSE){
	# skipY1: if TRUE, payment of year 1 is skipped and then amort. basis is paid off in n-1 non-zero payments.

	if(!skipY1) gaip(p, i, m, g, end)*(g + 1)^(1:m - 1)
	else        c(0, gaip(p*(1 + i), i, m-1, g, end)*(g + 1)^(1:(m-1) - 1))
}



#' Strait line method
#'
#' @param p Principle
#' @param i interest rate
#' @param m Amortization period
#' @param end Logical. If TRUE, payments are made at the end of periods
#'
#' @return A vector of annual amortization payments
#' @export
#'
#' @examples
amort_sl <- function(p, i, m, end = FALSE){
	# Straitline amortization method
	# See Winklevoss(1993, p101)
	if(end){
		sl <- i*(p - p*(0:(m - 1))/m) + p/m
	} else {
		d <- 1/(1+i)
		sl <- d*(p - p*(1:m)/m) + p/m}
	return(sl)
}


#' Function for choosing amortization methods
#'
#' @param p Principle
#' @param i interest rate
#' @param m Amortization period
#' @param g Constant growth rate of payments
#' @param end Logical. If TRUE, payments are made at the end of periods
#' @param method Amortization method
#' @param skipY1 Logial. If TRUE, year-1 payment is 0
#'
#' @return A vector of annual amortization payments
#' @export
#'
#' @examples
amort_LG <- function(p, i, m, g, end = FALSE, method = "cd", skipY1 = FALSE){
	# amortize the gain/loss using specified amortization method
	switch(method,
				 cd = amort_cd(p, i ,m, end, skipY1),
				 cp = amort_cp(p, i, m, g, end, skipY1),
				 sl = amort_sl(p, i, m, end)
	)
}




#**************************************
# 2. Utility functions       #####
#**************************************


#' Turning NAs and NaNs to 0
#'
#' @param x A vector that may contain NAs or NaNs
#'
#' @return The vector with NAs and NaNs replaced by 0
#' @export
#'
#' @examples
na2zero <- function(x){replace(x, is.na(x), 0)}



#' Calculating geometric mean return
#'
#' @param x Vector of returns
#'
#' @return Geometric mean return
#' @export
#'
#' @examples
get_geoReturn <- function(x) {
	x <- x[!is.nan(x)]
	x <- x[!is.na(x)]
	prod(1 + x)^(1/length(x)) - 1}










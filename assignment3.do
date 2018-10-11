* Assignment 3 - stated preference
* Tiana Randriamaro

cd "C:\Users\mary.randriamaro\Downloads"

* Econometrics question 2
use Kolkatavaccine.dta

* Estimating WTP using parametric probit model for cholera and typhoid vaccines
* in the two neighborhoods with and without time to think (TTT)
probit yesno pricRs ttt til
* Getting coefficients
matrix b=e(b)
* Getting coefficients other than price coefficient 
matrix xbeta=b[.,2...]
* Isolating beta price
gen betap=_b[pricRs]

* Estimating WTP without TTT
replace ttt=0
matrix score y=xbeta
gen wtpChNT =(y/-betap)
display wtpChNT
drop y

* Estimating WTP with TT
replace ttt=1
matrix score y=xbeta
gen wtpChTTT=(y/-betap)
display wtpChTTT
drop y
drop betap

* Estimating the model without covariates
probit yesno pricRs ttt til
est sto nocov
matrix coef = e(b)
matrix xbeta = coef[.,2...]
gen betap = _b[pricRs]
matrix score y=xbeta
gen WTP =(y/-betap)
display WTP
drop coef
drop xbeta
drop y
drop betap

* Estimating the model with covariates for gender, education, age, and knowing
* someone who has had the disease
gen young = 0
	replace young = 1 if age < 25
probit yesno pricRs ttt til male young edu2 edu3 edu4 KNOW
est sto withcov
matrix coef = e(b)
matrix xbeta = coef[.,2...]
gen betap = _b[pricRs]
matrix score y=xbeta
gen WTPcov =(y/-betap)
display WTPcov

esttab nocov withcov using probitres.tex, se

* Mean expected WTP for the two neighborhoods (Beliaghata and Tiljala)
* Cholera No Time to Think Tiljala
probit yesno pricRs ttt til chol male young edu2 edu3 edu4 KNOW
matrix b=e(b)
matrix xbeta=b[.,2...]
gen betap=_b[pricRs]
replace til = 1
replace ttt=0
replace chol = 1
matrix score y=xbeta
gen TilcholNTT =(y/-betap)
display TilcholNTT
drop y

* Cholera Time to Think Tiljala
replace chol = 1
replate til = 1
replace ttt=1
matrix score y=xbeta
gen TilcholTTT =(y/-betap)
display TilcholTTT
drop y

* Typhoid NTT Tiljala
replace til = 1
replace chol = 0
replace ttt=0
matrix score y=xbeta
gen TiltyphNTT =(y/-betap)
display TiltyphNTT
drop y


* Typhoid TTT Tiljala
replace til = 1
replace chol = 0
replace ttt=1
matrix score y=xbeta
gen TiltyphTTT =(y/-betap)
display TiltyphTTT
drop y

* Cholera No Time to Think Beliaghata
replace chol = 1
replace til = 0
replace ttt=0
matrix score y=xbeta
gen BelicholNTT =(y/-betap)
display BelicholNTT
drop y

* Cholera Time to Think Beliaghata
replace chol = 1
replace til = 0
replace ttt=1
matrix score y=xbeta
gen BelicholTTT =(y/-betap)
display BelicholTTT
drop y

* Typhoid NTT Beliaghata
replace chol = 0
replace til = 0
replace ttt=0
matrix score y=xbeta
gen BelityphNTT =(y/-betap)
display BelityphNTT
drop y

* Typhoid TT Beliaghata
replace chol = 0
replace til = 0
replace ttt=1
matrix score y=xbeta
gen BelityphTTT =(y/-betap)
display BelityphTTT
drop y

* Econometrics question 3
* Conditional logit of vaccine demand ignoring the two TTT treatments
use vietnamchoice.dta
egen newid = seq(), f(1) t(2400) b(3)
asclogit choice price eff70 eff99 dure20 vacc_ch asc,case(newid) alternatives(alternative) noconstant
est sto m1
// command clogit choice ..., group(newid) gets the same results 

* Estimating the model by interacting the coefficients  
for any price asc eff70 eff99 dure20 vacc_ch: gen tttX = ttt*X
asclogit choice price tttprice eff70 ttteff70 eff99 ttteff99 dure20 tttdure vacc_ch tttvacc_ch asc tttasc, case(newid) alternatives(alternative) noconstant
est sto m2

esttab m1 m2 using clogit.tex, se

* Wald tests
test price = tttprice
test asc = tttasc
test eff70 = ttteff70
test eff99 = ttteff99
test dure20 = tttdure20
test vacc_ch = tttvacc_ch

* Part-worth WTP for increasing duration and effectiveness

* Econometrics question 4
* Mixed logit model allowing coefficient on price and the ASC to be normally
* distributed
mixlogit choice eff70 eff99 dure20 vacc_ch,group(newid) rand(price asc) nrep(500) id(respid) iterate(400) technique(nr)
est sto mixlo
esttab mixlog using mixlogit.tex, se

mixlbeta price asc, saving(betapasc)
use betapasc.dta
hist price
hist asc

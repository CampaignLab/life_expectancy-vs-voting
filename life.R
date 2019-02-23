
#
# yougov? data hack 2015
#

plot_loess=function(pol_party, pol_col)
{
pol_lmod=loess(le ~ margin, data=pol_party, span = 1/3)
pol_pred=predict(pol_lmod, se=TRUE)

lines(pol_party$margin, pol_pred$fit, col=pol_col)
lines(pol_party$margin, pol_pred$fit-1.96*pol_pred$se.fit, col="grey")
lines(pol_party$margin, pol_pred$fit+1.96*pol_pred$se.fit, col="grey")
}


plot_party=function(con_p, lab_p, gender="")
{
plot(con_p$margin, con_p$le, col="blue",
	xlab="Victory margin over 2nd place", ylab="Life expectancy",
	xlim=c(0, 55), ylim=c(74, 86))
points(lab_p$margin, lab_p$le, col="red")
text(25, 75, gender)
plot_loess(con_p, "blue")
plot_loess(lab_p, "red")
}



# Life expectancy vs margin of victory in 2015 UK general election

# Update path for your situation
life=read.csv("/Users/plagchk/hackathons/gov15-hack//lifeexp-extrafac.csv", as.is=TRUE)

life$party[grepl("Co-oper", life$party)]="Conservative and Unionist Party"
life$party[grepl("The Speaker", life$party)]="Labour Party"
life$Married=life$Married/100
life$Single=life$Single/100
life$Divorced=life$Divorced/100
life$Widowed=life$Widowed/100

m=life
m$female.le=NULL
m$le=m$male.le
m$male.le=NULL
m$gender="male"
f=life
f$male.le=NULL
f$gender="female"
f$le=f$female.le
f$female.le=NULL
life=rbind(m, f)

life=life[order(life$margin), ]


# life_mod=glm(le ~ (margin+party)^2, data=life)

life_mod=glm(le ~ (margin+party)^2+Married+Single+gender, data=life)

# "Single..never.married.or.never.registered.a.same.sex.civil.partnership.","Married"
summary(life_mod)

con_party=subset(life, grepl("Conserv", party))
lab_party=subset(life, grepl("Labour", party))

m_con=subset(con_party, gender == "male")
f_con=subset(con_party, gender == "female")
m_lab=subset(lab_party, gender == "male")
f_lab=subset(lab_party, gender == "female")

par(pch="*")
par(las=1)

# plot_party(con_party, lab_party)

plot_party(m_con, m_lab, "Male")
plot_party(f_con, f_lab, "Female")


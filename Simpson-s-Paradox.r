#Simpson's paradox: the impact of omitted variables

#create data set
x1 = data.frame(supervisor = c(rep('Wallace',3+293+255+1247),rep('Lundvall',12+307+75+359),rep('Jones',131+2368+81+123)));
x2 = data.frame(region = c(rep('Domestic',3+293),rep('Overseas',255+1247),rep('Domestic',12+307),rep('Overseas',75+359),rep('Domestic',131+2368),rep('Overseas',81+123)));
#x3 = data.frame(defective = c(rep('Defective',3),rep('Nondefective',293),rep('Defective',255),rep('Nondefective',1247),rep('Defective',12),rep('Nondefective',307),rep('Defective',75),rep('Nondefective',359),rep('Defective',131),rep('Nondefective',2368),rep('Defective',81),rep('Nondefective',123)));

#x2 = data.frame(region = c(rep(0,3+293),rep(1,255+1247),rep(0,12+307),rep(1,75+359),rep(0,131+2368),rep(1,81+123)));
x3 = data.frame(defective = c(rep(1,3),rep(0,293),rep(1,255),rep(0,1247),rep(1,12),rep(0,307),rep(1,75),rep(0,359),rep(1,131),rep(0,2368),rep(1,81),rep(0,123)));


simpson = cbind(x1,x2,x3);
detach(simpson);
#crosstabs to see different relationships
attach(simpson);

  #crosstab_1 for overall defect rate
  addmargins(round(prop.table(xtabs(~supervisor+defective),1),3));
  #one could see Jones has the lowest defect rate overall whereas Wallace has the highest

  #crosstab_2 for defect rate of domestic vs oversee
  print('Domestic defective rate');
  addmargins(round(prop.table(xtabs(~supervisor+defective, subset = region == 'Domestic'),1),3));
  #one could see Wallace has the lowest defect rate where Jones has the worst performance in domestic market

  print('Overseas defective rate');
  addmargins(round(prop.table(xtabs(~supervisor+defective, subset = region == 'Overseas'),1),3));
  #once again, Wallace performs better than Jones does in overseas market

  interaction.plot(simpson$supervisor, simpson$region, simpson$defective, col = 1:2, xlab = 'Supervisor', ylab = 'Defective Rate', trace.label = 'Region')

#conclusion: Wallace did a better job.
#explanation: Simpson's paradox -- 'region' plays a decisive role in the relationship between defect rate and supervisor. If one omitted this variable, he or she may reach to a wrong conclusion.

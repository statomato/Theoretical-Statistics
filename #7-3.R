part3 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/7주차/Shark.csv")
colnames(part3) <- c("Sex","Tag.Date","Recapture.Date","Days","PCL1","PCL2")
attach(part3)

#(a)
t = Days/365.25
dL = (PCL2-PCL1)/t
Lbar = (PCL1+PCL2)/2
lmresult = lm(dL~Lbar)
alpha = as.numeric(lmresult$coefficients[1])
beta = as.numeric(lmresult$coefficients[2])
k = -beta
L0 = alpha/k

nls(PCL2~ a-(a-PCL1)*exp(-b*t),start=list(a=L0,b=k),data=part3)

#(b)
t0 = log(1-51.5/L0)/k
tseq = seq(-0.5,30.5,by=1)
L = L0*(1-exp(-k*(tseq-t0)))
plot(tseq,L,type="l")

#(c)
part3m <- part3 %>% filter(Sex == "M")
part3f <- part3 %>% filter(Sex == "F")

m_t = part3m$Days/365.25
m_dL = (part3m$PCL2-part3m$PCL1)/m_t
m_Lbar = (part3m$PCL1+part3m$PCL2)/2
m_lmresult = lm(m_dL~m_Lbar)
m_alpha = as.numeric(m_lmresult$coefficients[1])
m_beta = as.numeric(m_lmresult$coefficients[2])
m_k = -m_beta
m_L0 = m_alpha/m_k
m_L = m_L0*(1-exp(-m_k*(tseq-t0)))

f_t = part3f$Days/365.25
f_dL = (part3f$PCL2-part3f$PCL1)/f_t
f_Lbar = (part3f$PCL1+part3f$PCL2)/2
f_lmresult = lm(f_dL~f_Lbar)
f_alpha = as.numeric(f_lmresult$coefficients[1])
f_beta = as.numeric(f_lmresult$coefficients[2])
f_k = -f_beta
f_L0 = f_alpha/f_k
f_L = f_L0*(1-exp(-f_k*(tseq-t0)))

ggplot()+geom_line(aes(tseq,m_L),col="blue")+geom_line(aes(tseq,f_L),col="red")

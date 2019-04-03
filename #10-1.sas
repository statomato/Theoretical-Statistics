DATA part1; 
 INFILE 'C:\Users\LEEEUNJIN\Desktop\2018-1\대학원\이론통계학1\10주차/part1.txt'; 
 INPUT x1 x2 n y; 
RUN;

DATA part1;
 SET part1;
 ybar = y/n; logybar = log(y/n); a = (37+5*x1); loga = log(a); logn = log(n);
RUN;

/*(a)*/
PROC SGPLOT DATA=part1; 
 SCATTER X=logybar Y=a; 
RUN;

PROC SGPLOT DATA=part1; 
 SCATTER X=logybar Y=loga; 
RUN;

PROC SGPLOT DATA=part1; 
 SCATTER X=logybar Y=x2; 
RUN;

/*(b)*/
PROC GENMOD DATA=part1;
 CLASS x1 x2 / REF=FIRST;
 MODEL y = logn loga x2 / DIST = POISSON LINK = LOG;
RUN;

PROC GENMOD DATA=part1;
 CLASS x1 x2 / REF=FIRST;
 MODEL y = logn x1 x2 / DIST = POISSON LINK = LOG;
RUN;

/*(c)*/
PROC GENMOD DATA=part1 PLOTS=ALL;
 CLASS x1 x2  /REF=FIRST;
 MODEL y = loga x2 / DIST = POISSON LINK = LOG OFFSET=logn RESIDUAL;
RUN;

/*(e)*/
PROC HPGENSELECT DATA=part1;
 CLASS x1 x2 /  REF=FIRST;
 MODEL  y = loga  x1 x2 x1*x2 / DISTRIBUTION = POISSON LINK = LOG OFFSET=logn;
												SELECTION  METHOD= backward(sls = 0.15 choose=aic) DETAILS = all ; 
RUN;

PROC GENMOD DATA=part1;
 CLASS x1 x2 /  REF=FIRST;
 MODEL y = loga x2 / DIST = POISSON LINK = LOG OFFSET=logn;
RUN;

/*(f)*/
PROC HPGENSELECT DATA=part1;
 CLASS x1 x2 /  REF=FIRST;
 MODEL  y/n = loga  x1 x2 x1*x2 / DISTRIBUTION = BINOMIAL LINK = LOGIT OFFSET=logn;
												SELECTION  METHOD= backward(sls = 0.15 choose=aic) DETAILS = all ; 
RUN;

PROC HPGENSELECT DATA=part1;
 CLASS x1 x2 /  REF=FIRST;
 MODEL  y/n = loga  x1 x2 / DISTRIBUTION = BINOMIAL LINK = LOGIT OFFSET=logn;
												SELECTION  METHOD= backward(sls = 0.15 choose=aic) DETAILS = all ; 
RUN;


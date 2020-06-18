/* PROJECT CODE */
/* GROUP 12 */
/* MEMBERS */
/* ANEESH KAMAT */
/* ARAVIND KALLIAPPAN */
/* ARCHANA RAFEEQ */
/* AYUSH SHARMA */
/* SHIVANJALI CHAMOLI */
/* SHUBHA HANUMAN */

DATA A1;
INFILE "H:\Project\mayo\mayo_groc_1114_1165.txt" firstobs = 2;
INPUT IRI_KEY WEEK SY GE VEND ITEM UNITS DOLLARS F $ D PR;
RUN;

PROC PRINT data=A1(obs = 10); RUN;

PROC IMPORT DATAFILE= "H:\Project\mayo\prod_mayo.xls"
			OUT= WORK.B1 
            DBMS=XLS REPLACE;
     GETNAMES=YES;
RUN;
	
PROC PRINT data=B1(obs = 10); RUN;

data A2;
set A1;
UPC=catx("-",put(SY,z2.),put(GE,z2.),put(VEND,z5.),put(ITEM,z5.));
run;

PROC SQL;
create table FINAL as
  select * from A2, B1
where A2.UPC=B1.UPC;
run;

PROC CONTENTS data=FINAL; RUN;
PROC PRINT data=FINAL(obs = 10); RUN;

DATA C1;
INFILE "H:\Project\mayo\Delivery_Stores.txt" firstobs = 2;
INPUT IRI_KEY OU $ EST_ACV Market_Name & $25. Open Clsd MskdName $;
RUN;

PROC SQL;
create table FINAL_1 as
select * from FINAL join C1
on FINAL.IRI_KEY = C1.IRI_KEY;
RUN;

PROC PRINT data=FINAL_1(obs = 10); RUN;

PROC SQL;
create table Q1 as
select L5 as Brand_Name , sum(DOLLARS) as Total_Sales
from FINAL
group by L5;
RUN;

PROC SORT data = Q1; BY DESCENDING Total_Sales; RUN;
data Q1_1;
set Q1;
if _N_<=6;
run;
PROC SQL;
create table Q1_2 as
select *, Total_Sales/sum(Total_Sales)*100 as Market_Share
from Q1_1;
RUN;
PROC PRINT data=Q1_2; RUN;


PROC SQL;
create table Brands_T4 as
select * 
from FINAL_1
where L5 in ('KRAFT MIRACLE WHIP', 'HELLMANNS', 'BEST FOODS', 'KRAFT MAYO', 'PRIVATE LABEL');

/*Area Spread*/

PROC SQL;
create table B_T4_Area as
select Market_Name, sum(DOLLARS) as Total_Sales
from FINAL_1
group by Market_Name;

PROC SORT data=B_T4_Area; by DESCENDING Total_Sales; RUN;

PROC PRINT data=B_T4_Area(obs=10); RUN;

PROC SQL;
create table B_T4_Area as
select *
from Brands_T4
where Market_Name in ('NEW YORK', 'LOS ANGELES', 'SAN FRANCISCO', 'NEW ENGLAND', 'PHILADELPHIA', 'BOSTON', 'CHICAGO', 'PORTLAND,OR', 'DALLAS, TX', 'WASHINGTON, DC');


PROC TABULATE data=B_T4_Area;
CLASS Market_Name L5;
VAR DOLLARS;
TABLE Market_Name, DOLLARS * L5 * ( n sum) ;
RUN;  

/*Product Type*/

PROC IMPORT DATAFILE= "H:\Project\mayo\prod_mayo1.xls"
			OUT= WORK.D1 
            DBMS=XLS REPLACE;
     GETNAMES=YES;
     RUN;

PROC SQL;
create table V1 as
select f.*, v.VOLUME
from FINAL_1 f join D1 v
on f.UPC=v.UPC;

DATA V2;
SET V1;
PPV = DOLLARS/(VOLUME*UNITS);
PPU = DOLLARS/UNITS;
PPUV = DOLLARS/UNITS/VOLUME;
RUN;

PROC SQL;
create table temp as
select L5, mean(PPU) as PPU
from V2
where VOLUME=32
group by L5;

PROC SORT data=temp; by DESCENDING PPU; RUN;
PROC PRINT data=temp(obs=100); RUN;

PROC SQL;
create table T5B as
select *
from V2 
where L5 in ('KRAFT MIRACLE WHIP', 'HELLMANNS', 'BEST FOODS', 'KRAFT MAYO', 'PRIVATE LABEL');

PROC SQL;
create table z as
select distinct L5,Product_Type from T5B group by L5,Product_Type;
/*Brand=Kraft Mayo*/
/*Region*/

PROC SQL;
create table KM as
select * from V2
where L5 = 'KRAFT MAYO';

PROC SQL;
create table KM_Region as
select Market_Name, sum(DOLLARS) as SALES
from KM
group by Market_Name;

PROC SORT data=KM_Region; by DESCENDING Sales; RUN;
PROC PRINT data=KM_Region(obs=100); RUN;

PROC SQL;
create table KM_UPC as
select Market_Name, L5, sum(DOLLARS) as Sales
from V2
where Market_Name in ('NEW YORK', 'NEW ENGLAND', 'BOSTON', 'PHILADELPHIA')
group by Market_Name, L5;

PROC SORT data=KM_UPC; by Market_Name DESCENDING Sales; RUN;
PROC PRINT data=KM_UPC(obs=107); RUN;


/*Packaging*/
PROC SQL;
create table packaging as
select Market_Name, L5, PACKAGE, sum(DOLLARS) as SALES
from V2
where Market_Name in ('NEW YORK', 'NEW ENGLAND', 'BOSTON', 'PHILADELPHIA') and L5 in ('HELLMANNS', 'KRAFT MAYO')
group by Market_Name, L5, PACKAGE;

PROC SORT data=packaging; by Market_Name L5 PACKAGE DESCENDING Sales; RUN;
PROC PRINT data=packaging(obs=107); RUN;


/*Sugar + Fat*/
PROC SQL;
create table fat as
select Market_Name, FAT_CONTENT, sum(DOLLARS) as Sales
from V2
where Market_Name in ('NEW YORK', 'NEW ENGLAND', 'BOSTON', 'PHILADELPHIA')
group by Market_Name, FAT_CONTENT;

PROC SQL;
create table fat1 as
select *, Sales/sum(Sales)*100 as MS
from fat
group by Market_Name;

PROC SORT data=fat1; by Market_Name DESCENDING MS; RUN;
PROC PRINT data=fat1(obs=107); RUN;

PROC SQL;
create table KM_FAT as
select FAT_CONTENT, sum(DOLLARS) as Sales
from KM
group by FAT_CONTENT;

PROC SQL;
create table KM_FAT as
select *, Sales/sum(Sales)*100 as MS
from KM_FAT;

PROC SORT data=KM_FAT; by DESCENDING MS; RUN;
PROC PRINT data=KM_FAT(obs=107); RUN;


/*Order Size*/
PROC SQL;
create table PR as
select *
from V2
where Market_Name in ('NEW YORK', 'NEW ENGLAND', 'PHILADELPHIA', 'BOSTON') and L5 in ('HELLMANNS', 'KRAFT MAYO');

PROC TABULATE data=PR;
CLASS L5;
VAR UNITS;
TABLE UNITS * L5 * ( n mean) ;
RUN;  

PROC TABULATE data=PR;
CLASS L5 PR;
VAR UNITS;
TABLE PR, UNITS * L5 * ( n mean) ;
RUN; 

PROC SQL;
create table PR as
select L5, sum(PR)
from B_T4_Area
group by L5;

PROC PRINT data=PR; RUN;

PROC SQL;
create table units as
select UNITS, count(UNITS)
from V2
group by UNITS;

PROC SORT data=Units; by DESCENDING UNITS; RUN; 
PROC PRINT data=Units(obs=100); RUN;

PROC SQL;
create table BigOrders as
select * from V2 
where L5 in ('HELLMANNS', 'KRAFT MAYO', 'PRIVATE LABEL') and Market_Name in ('NEW YORK', 'NEW ENGLAND', 'BOSTON', 'PHILADELPHIA');

DATA BigOrders1;
set BigOrders;
BO=0;
if Units >50 then BO = 1;
if Units >100 then BO=2;
RUN;

PROC TABULATE data=BigOrders1;
CLASS BO L5;
VAR DOLLARS;
TABLE BO, DOLLARS * L5 * (n sum) ;
RUN;

/*Store Level*/
PROC SQL;
create table stores as
select Market_Name, count(IRI_KEY) as count
from V2
group by Market_Name;

PROC SORT data=stores; by DESCENDING count; RUN;
PROC PRINT data=stores(obs=107); RUN;

PROC SQL;
create table temp as
select Market_Name, L5, IRI_Key, DOLLARS, PPU, PPV, PPUV
from V2
where L5 in ('HELLMANNS', 'KRAFT MAYO') and Market_Name in ('NEW YORK', 'NEW ENGLAND', 'BOSTON', 'PHILADELPHIA');

PROC TABULATE data=temp;
CLASS Market_Name L5;
VAR DOLLARS;
TABLE Market_Name, DOLLARS * L5 * (n sum) ;
RUN;

PROC SQL;
create table StoreChains as
select MskdName, count(distinct(IRI_KEY)) as count
from V2
group by MskdName;

PROC SORT data=StoreChains; by DESCENDING count; RUN;
PROC PRINT data=StoreChains(obs=107); RUN;

PROC SQL;
create table temp as select * from V2 where L5 in ('HELLMANNS', 'KRAFT MAYO');

PROC TABULATE data=temp;
CLASS Market_Name L5;
VAR DOLLARS;
TABLE  Market_Name, DOLLARS * L5 * (n sum) ;
RUN;

/*NY Store Level*/
PROC SQL;
create table NY as select * from V2 where Market_Name='NEW YORK' and L5 in ('HELLMANNS', 'KRAFT MAYO', 'PRIVATE LABEL');

PROC CONTENTS data=NY; RUN;

PROC SQL;
create table BestChains as
select MskdName, sum(DOLLARS) as Sales
from NY
group by MskdName;

PROC TABULATE data=NY;
CLASS D MskdName L5;
VAR DOLLARS;
TABLE D, MskdName, DOLLARS * L5 * (n sum) ;
RUN;

/*DTX Store Level*/
PROC SQL;
create table DTX as 
select * from V2 
where Market_Name='DALLAS, TX' and L5 in ('HELLMANNS', 'KRAFT MAYO');

PROC CONTENTS data=DTX; RUN;

PROC SQL;
create table BestChains as
select MskdName, sum(DOLLARS) as Sales
from DTX
group by MskdName;

PROC TABULATE data=DTX;
CLASS D MskdName L5;
VAR DOLLARS;
TABLE D, MskdName, DOLLARS * L5 * (n sum) ;
RUN;

/*libname out 'Z:\Project\mayo';
data out.FINAL_DATA; 
set V2;
run;*/

/* Regression */
PROC SQL;
create table reg as
select L5 as BRAND_NAME, WEEK, UNITS, DOLLARS, F, D, PR, VOLUME, PPU, PPV, PPUV 
from V2
where BRAND_NAME = 'KRAFT MAYO';

data reg;
set reg;
if D ~= 0 then D = 1;
if F = 'NONE' then F1=0;
if F ~= 'NONE' then F1=1; 
RUN;

PROC PRINT data=reg(obs=50); RUN;

PROC SQL;
create table reg1 as
select WEEK, avg(DOLLARS) as Sales, avg(PPV) as APPV, avg(F1) as Average_Feature,
avg(D) as Average_Display, avg(PR) as Averege_Price_Reduction
from reg
group by WEEK;

PROC PRINT data=reg1(obs=50); RUN;

PROC REG data=reg1;
model Sales=APPV Average_Feature Average_Display Averege_Price_Reduction / VIF COLLIN;
RUN;


/* Household */

PROC IMPORT DATAFILE= "H:\Project\mayo\mayo_PANEL_GR_1114_11651.csv"
			OUT= WORK.P1 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
RUN;

PROC IMPORT DATAFILE= "H:\Project\mayo\Panel_Demo.csv"
			OUT= WORK.P2 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
RUN;

/*PROC PRINT data=panel(obs=10); RUN;
PROC CONTENTS data=P2; RUN;*/

PROC SQL;
create table P1_edit as
select PANID, WEEK, sum(UNITS) as UNITS, OUTLET,sum(DOLLARS) as DOLLARS, IRI_KEY, COLUPC
from P1
group by PANID, WEEK, OUTLET, IRI_KEY, COLUPC;

PROC SQL;
create table Panel as
select * from
P1_edit left join P2
on PANID=PANELIST_ID;

DATA V3;
set V2;
temp1 = SUBSTR(UPC,5,1);
temp2 = SUBSTR(UPC,7,5);
temp3 = SUBSTR(UPC,13,5);
temp = cats(temp1, temp2, temp3);
COLUPC=input(temp, 11.);
RUN;

PROC SQL;
create table z as
select count(*) 
from Panel
group by IRI_KEY, WEEK, COLUPC, DOLLARS;

PROC SQL;
create table FullData as
select * from 
Panel p left join V3 v
on p.IRI_KEY=v.IRI_KEY and p.WEEK=v.WEEK and p.COLUPC=v.COLUPC;

PROC CONTENTS data=FullData; RUN;
PROC PRINT data=FullData (obs=100); RUN;

PROC SQL;
create table z as
select PANID, count(UNITS) as count 
from FullData group by PANID;
PROC PRINT; RUN;

/*** RFM ANALYSIS ***/

PROC SQL;
create table KM as
select * from V3
where L5 = 'KRAFT_MAYO';

PROC SQL;
create table Panel_KM as
select * from
Panel p join V3 v
on p.IRI_KEY=v.IRI_KEY and p.WEEK=v.WEEK and p.COLUPC=v.COLUPC;

PROC SQL;
CREATE TABLE RFM_FINAL AS
SELECT PANID AS CUSTOMER , MAX(WEEK) AS RECENCY, SUM(DOLLARS) as MONETARY, COUNT(WEEK) AS FREQUENCY 
FROM Panel_KM 
GROUP BY PANID;
RUN;

PROC PRINT DATA = RFM_FINAL(OBS = 5);RUN;

PROC CORR DATA=RFM_FINAL;
VAR MONETARY FREQUENCY RECENCY;
RUN;

PROC MEANS DATA=RFM_FINAL MIN P20 P40 P60 P75 MAX;
VAR RECENCY FREQUENCY MONETARY;
RUN;

PROC SQL;
CREATE TABLE TOP20 AS 
SELECT * FROM RFM_FINAL 
WHERE MONETARY >= 14 OR FREQUENCY >=5 OR RECENCY >=1162;
RUN;

DATA Cust_Categories;
set RFM_FINAL;
IF MONETARY >= 14 then M=1; ELSE M=0;
IF FREQUENCY >= 5 then F=1; ELSE F=0;
IF RECENCY >= 1162 then R=1; ELSE R=0;
IF R = 0 and F = 0 and M = 0 then CAT='None';
IF R = 1 and F = 1 and M = 0 then CAT='RF';
IF R = 1 and F = 0 and M = 1 then CAT='RM';
IF R = 0 and F = 1 and M = 1 then CAT='FM';
IF R = 1 and F = 0 and M = 0 then CAT='R';
IF R = 0 and F = 1 and M = 0 then CAT='F';
IF R = 0 and F = 0 and M = 1 then CAT='M';
IF R = 1 and F = 1 and M = 1 then CAT='RFM';
SCORE = R + F + M;
IF SCORE >= 2 then BEST = 1; ELSE BEST = 0;
RUN;

PROC SGPLOT DATA = Cust_Categories;
VBAR Score / GROUP=CAT /*GROUPDISPLAY = CLUSTER*/;
RUN;

PROC SQL;
create table subset as
select * from Cust_Categories where SCORE > 0;

PROC SGPLOT DATA = subset;
VBAR SCORE / GROUP=CAT /*GROUPDISPLAY = CLUSTER*/;
RUN;

PROC SQL;
CREATE TABLE TOP20 AS 
SELECT * FROM Cust_Categories 
WHERE SCORE >=2; 
RUN;

PROC PRINT DATA = CUST_Categories(OBS = 10);RUN;
PROC CONTENTS DATA = TOP20;RUN;

PROC MEANS DATA = TOP20 SUM;
VAR MONETARY;
RUN;

/* Demographics*/
PROC SQL;
create table Best_Cust as
select *
from Top20 T left join P2 P
on T.CUSTOMER = P.PANELIST_ID;

PROC SQL;
create table All_Cust as
select *
from Cust_Categories T left join P2 P
on T.CUSTOMER = P.PANELIST_ID;

/* proc ANOVA data=Best_Cust;
title Example of one-way ANOVA;
class Combined_Pre_Tax_Income_of_HH;
model BEST = Combined_Pre_Tax_Income_of_HH;
means Combined_Pre_Tax_Income_of_HH /hovtest welch;
run;

proc freq data = Best_Cust;
tables BEST*Combined_Pre_Tax_Income_of_HH 
/chisq 
;
run;

proc ttest data=Best_Cust;
var Combined_Pre_Tax_Income_of_HH;
class BEST;
run;*/

PROC SGPLOT DATA = Best_Cust;
VBAR Combined_Pre_Tax_Income_of_HH /* GROUP=CAT /*GROUPDISPLAY = CLUSTER*/;
RUN;

PROC SGPLOT DATA = All_Cust;
VBAR Combined_Pre_Tax_Income_of_HH /* GROUP=CAT /*GROUPDISPLAY = CLUSTER*/;
RUN;

PROC SGPLOT DATA = Best_Cust;
VBAR HH_AGE /* GROUP=CAT /*GROUPDISPLAY = CLUSTER*/;
RUN;

PROC SGPLOT DATA = All_Cust;
VBAR HH_AGE /* GROUP=CAT /*GROUPDISPLAY = CLUSTER*/;
RUN;

PROC SGPLOT DATA = Best_Cust;
VBAR HH_EDU /* GROUP=CAT /*GROUPDISPLAY = CLUSTER*/;
RUN;

PROC SGPLOT DATA = All_Cust;
VBAR HH_EDU /* GROUP=CAT /*GROUPDISPLAY = CLUSTER*/;
RUN;


/* Panel Regression */
PROC SQL;
create table PANEL_HOUSEHOLD as
select PANID, WEEK, sum(UNITS) as UNITS, sum(DOLLARS) as DOLLARS 
from P1 group by PANID, WEEK;

PROC SQL;
create table PANEL_HH as
select *,
from PANEL_HOUSEHOLD PH left join P2 P
on PH.PANID=P.PANELIST_ID;

PROC SORT data=PANEL_HOUSEHOLD; by DESCENDING count; RUN;
PROC PRINT data = PANEL_HOUSEHOLD (obs=100); RUN;

DATA V4;
set V3;
if D ~= 0 then D1 = 1; else D1 = 0;
if F = 'NONE' then F1=0;
if F ~= 'NONE' then F1=1;
RUN;

PROC SQL;
create table Panel_Store as
select IRI_KEY, WEEK, sum(UNITS*VOLUME) as TOT_VOL, sum(UNITS) as TOT_UNITS, sum(DOLLARS) as SALES,
avg(D1) as AVG_DISP, avg(F1) as AVG_FEATURE, avg(PR) as AVG_PR, avg(PPV) as AVG_PPV
from V4
group by IRI_KEY, WEEK;

PROC SORT data=Panel_Store; by IRI_KEY WEEK; RUN;

proc panel data=Panel_Store;
      id IRI_KEY WEEK;
      model TOT_VOL = AVG_PPV AVG_DISP AVG_FEATURE AVG_PR / fixtwo;
   run;


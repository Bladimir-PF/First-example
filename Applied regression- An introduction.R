ibrary()
library()
library()
library()
library('dplyr')
library()
library()
install.packages('paramtest', "multcomp", 'lsmeans', 'emmeans', "car", "gmodels")


dat <- read.table(header=TRUE, text='
Respondent	Education	Income
1	8	26,430
2	8	37,449
3	10	34,182
4	10	25,479
5	10	47,034
6	12	37,656
7	12	50,265
8	12	46,488
9	12	52,480
10	14	32,631
11	14	49,968
12	14	64,926
13	15	37,302
14	16	38,586
15	16	55,878
16	16	59,499
17	16	55,782
18	16	63,471
19	17	60,068
20	18	54,840
21	18	62,466
22	19	56,019
23	19	65,142
24	20	56,343
25	20	54,672
26	20	61,629
27	20	82,726
28	21	71,202
29	21	73,542
30	22	56,322
31	22	70,044
32	24	79,227')

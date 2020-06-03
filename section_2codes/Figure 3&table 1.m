clc;clear;close all;
%load our data from year 2014-year 2019.
load('.\2014data\maxEveryday2014.mat')
load('.\2015data\maxEveryday2015.mat')
load('.\2016data\maxEveryday2016.mat')
load('.\2017data\maxEveryday2017.mat')
load('.\2018data\maxEveryday2018.mat')
load('.\2019data\maxEveryday2019.mat')
k=find(maxEveryday2014==0)
maxEveryday2014(k)=maxEveryday2014(k-2);
k=find(maxEveryday2015==0)
if(k(1)==1)
    maxEveryday2015(1)=maxEveryday2015(2);
    %     k=k(2:end)
end
% maxEveryday2015(k)=(maxEveryday2015(k-1)+maxEveryday2015(k+1))/2;
k=find(maxEveryday2016==0)
maxEveryday2016(k)=(maxEveryday2016(k-1)+maxEveryday2016(k+1))/2;
k=find(maxEveryday2017==0)
maxEveryday2017(k)=(maxEveryday2017(k-1)+maxEveryday2017(k+1))/2;
k=find(maxEveryday2018==0)
maxEveryday2018(k)=(maxEveryday2018(k-1)+maxEveryday2018(k+1))/2;
k=find(maxEveryday2019==0)
maxEveryday2019(k)=(maxEveryday2019(k-1)+maxEveryday2019(k+1))/2;
figure
hold on
plot(maxEveryday2014,'b-')
plot(maxEveryday2015,'r-')
plot(maxEveryday2016,'g-')
plot(maxEveryday2017,'y-')
plot(maxEveryday2018,'k-')
plot(maxEveryday2019,'m-')
legend('Q2014','Q2015','Q2016','Q2017','Q2018','Q2019','FontSize',14);
xlim([1,366])
xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
xt=(1:12)*366/12;
xlabel('month','FontSize',14)
set(gca,'xtick',xt);
set(gca,'XTickLabel',xtl,'FontSize',14);
grid on
hold off
'mean'
[mean(maxEveryday2014),mean(maxEveryday2015),mean(maxEveryday2016),mean(maxEveryday2017),mean(maxEveryday2018),mean(maxEveryday2019)]
'median'
[median(maxEveryday2014),median(maxEveryday2015),median(maxEveryday2016),median(maxEveryday2017),median(maxEveryday2018),median(maxEveryday2019)]
'max'
[max(maxEveryday2014),max(maxEveryday2015),max(maxEveryday2016),max(maxEveryday2017),max(maxEveryday2018),max(maxEveryday2019)]
'min'
[min(maxEveryday2014),min(maxEveryday2015),min(maxEveryday2016),min(maxEveryday2017),min(maxEveryday2018),min(maxEveryday2019)]
'std dev'
[std(maxEveryday2014),std(maxEveryday2015),std(maxEveryday2016),std(maxEveryday2017),std(maxEveryday2018),std(maxEveryday2019)]
'skewness'
[skewness(maxEveryday2014),skewness(maxEveryday2015),skewness(maxEveryday2016),skewness(maxEveryday2017),skewness(maxEveryday2018),skewness(maxEveryday2019)]
'kurtosis'
[kurtosis(maxEveryday2014),kurtosis(maxEveryday2015),kurtosis(maxEveryday2016),kurtosis(maxEveryday2017),kurtosis(maxEveryday2018),kurtosis(maxEveryday2019)]


box20141=maxEveryday2014(1:90);    box20142=maxEveryday2014(91:181);    box20143=maxEveryday2014(182:273);    box20144=maxEveryday2014(274:365);
box20151=maxEveryday2015(1:90);    box20152=maxEveryday2015(91:181);    box20153=maxEveryday2015(182:273);    box20154=maxEveryday2015(274:365);
box20161=maxEveryday2016(1:91);    box20162=maxEveryday2016(92:182);    box20163=maxEveryday2016(183:274);    box20164=maxEveryday2016(275:366);
box20171=maxEveryday2017(1:90);    box20172=maxEveryday2017(91:181);    box20173=maxEveryday2017(182:273);    box20174=maxEveryday2017(274:365);
box20181=maxEveryday2018(1:90);    box20182=maxEveryday2018(91:181);    box20183=maxEveryday2018(182:273);    box20184=maxEveryday2018(274:365);
box20191=maxEveryday2019(1:90);    box20192=maxEveryday2019(91:181);    box20193=maxEveryday2019(182:273);    box20194=maxEveryday2019(274:365);



group = [zeros(size(box20141))',ones(size(box20151))',2*ones(size(box20161))',3*ones(size(box20171))',4*ones(size(box20181))',5*ones(size(box20191))'...
        6*ones(size(box20142))',7*ones(size(box20152))',8*ones(size(box20162))',9*ones(size(box20172))',10*ones(size(box20182))',11*ones(size(box20192))'...
       12*ones(size(box20143))',13*ones(size(box20153))',14*ones(size(box20163))',15*ones(size(box20173))',16*ones(size(box20183))',17*ones(size(box20193))',...
       18*ones(size(box20144))',19*ones(size(box20154))',20*ones(size(box20164))',21*ones(size(box20174))',22*ones(size(box20184))',23*ones(size(box20194))'];
     
boxData=[box20141',box20151',box20161',box20171',box20181',box20191',box20142', box20152',box20162',box20172',box20182',box20192',box20143', box20153',box20163',box20173',box20183',box20193', box20144', box20154', box20164',box20174',box20184',box20194'];
color = ['c'*ones(size([box20141',box20151',box20161',box20171',box20181',box20191']))...
    'y'*ones(size([box20142', box20152',box20162',box20172', box20182',box20192']))...
    'g'*ones(size([box20143', box20153',box20163',box20173', box20183',box20193']))...
    'b'*ones(size([box20144', box20154', box20164',box20174', box20184', box20194']))];
figure
boxplot(boxData,group,'colorgroup',color)
set(gca,'XTickLabelRotation',45,'FontSize',14)
set(gca,'XTickLabel',{'2014Q1','2015Q1','2016Q1','2017Q1','2018Q1','2019Q1','2014Q2','2015Q2','2016Q2','2017Q2','2018Q2','2019Q2','2014Q3','2015Q3','2016Q3','2017Q3','2018Q3','2019Q3','2014Q4','2015Q4','2016Q4','2017Q4','2018Q4','2019Q4'},'FontSize',14)

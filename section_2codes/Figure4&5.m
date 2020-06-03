load all2018.mat %weather extreme values in year 2018

xtempMin=min(reshape(xtemp,24,365)); %min temperature figure 5(b).
xtempMin(130)=xtempMin(129);
figure
plot(1:365,xtempMin,'k-');
% title('Minimum temprature ');
%xlabel('month'),ylabel('Minimum temprature')
xlim([1,365])
xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
xt=(1:12)*365/12;
set(gca,'xtick',xt);
set(gca,'XTickLabel',xtl);
set(gca,'FontName','Calibri Light','FontSize',11);
width=440;height=250;
left=200;bottem=600;
set(gcf,'position',[left,bottem,width,height])
grid off

xtemp1=xtemp; %max temperature figure 5(a).
xtemp1(find(xtemp1==1000))=-2000;
xtempMax=max(reshape(xtemp1,24,365));
xtempMax(130)=xtempMax(129);
figure
plot(1:365,xtempMax,'k-');
% title('Maximum temprature ');
%xlabel('month'),ylabel('Maximum temprature')
xlim([1,365])
xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
xt=(1:12)*365/12;
set(gca,'xtick',xt);
set(gca,'XTickLabel',xtl);
set(gca,'FontName','Calibri Light','FontSize',11);
width=440;height=250;
left=200;bottem=600;
set(gcf,'position',[left,bottem,width,height])
grid off


figure %max humidity Figure 5(f)
xhumiMax=max(reshape(xhumi,24,365));
xhumiMax(130)=xhumiMax(129);
plot(1:365,xhumiMax,'k-');
%  title('humi Max');
%xlabel('month'),ylabel('maximum humidity')
xlim([1,365])
xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
xt=(1:12)*365/12;
set(gca,'xtick',xt);
set(gca,'XTickLabel',xtl);
set(gca,'FontName','Calibri Light','FontSize',11);
width=440;height=250;
left=200;bottem=600;
set(gcf,'position',[left,bottem,width,height])
grid off

figure %min humidity figure 5(e)
xhumiMin=min(reshape(xhumi1,24,365));
xhumiMin(130)=xhumiMin(129);
plot(1:365,xhumiMin,'k-');
% title('humi Min');
% xlabel('month'),ylabel('minimum humidity')
xlim([1,365])
xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
xt=(1:12)*365/12;
set(gca,'xtick',xt);
set(gca,'XTickLabel',xtl);
set(gca,'FontName','Calibri Light','FontSize',11);
width=440;height=250;
left=200;bottem=600;
set(gcf,'position',[left,bottem,width,height])
grid off

%figure  %daily difference of humidity
%plot(1:365,xhumiMax-xhumiMin,'k-');
% title('humidity difference');
% xlabel('month'),ylabel('humidity difference')
%xlim([1,365])
%xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
%xt=(1:12)*365/12;
%set(gca,'xtick',xt);
%set(gca,'XTickLabel',xtl);
%set(gca,'FontName','Calibri Light','FontSize',11);
%width=440;height=250;
%left=200;bottem=600;
%set(gcf,'position',[left,bottem,width,height])
%grid off



%Figure 5(c)maximum wind level
[xwindlevelMax,a]=max(reshape(xwindlevel,24,365));
xwindlevelMax(130)=xwindlevelMax(129);
figure
plot(1:365,xwindlevelMax,'k*', 'MarkerSize',4);
%    title('wind level Max');
% xlabel('month'),ylabel('wind level')
xlim([1,365])
xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
xt=(1:12)*365/12;
set(gca,'xtick',xt);
set(gca,'XTickLabel',xtl);
set(gca,'FontName','Calibri Light','FontSize',11);
width=440;height=250;
left=200;bottem=600;
set(gcf,'position',[left,bottem,width,height])
grid off

figure %figure 5(d).Wind direction
xwinddirectemp=reshape(xwinddirection,24,365);
idx=[a',(1:365)'];
xwindDirc=xwinddirection(a+((1:365)-1)*24);
[row,col]=find(strcmp(xwindDirc,'??' ));
xwinddirtemp(col)=1;
[row,col]=find(strcmp(xwindDirc,'???' ));
xwinddirtemp(col)=2;
[row,col]=find(strcmp(xwindDirc,'??' ));
xwinddirtemp(col)=3;
[row,col]=find(strcmp(xwindDirc,'???' ));
xwinddirtemp(col)=4;
[row,col]=find(strcmp(xwindDirc,'??' ));
xwinddirtemp(col)=5;
[row,col]=find(strcmp(xwindDirc,'???' ));
xwinddirtemp(col)=6;
[row,col]=find(strcmp(xwindDirc,'??' ));
xwinddirtemp(col)=7;
[row,col]=find(strcmp(xwindDirc,'???' ));
xwinddirtemp(col)=8;
xwinddirtemp(130)=xwinddirtemp(129);
plot(1:365,xwinddirtemp,'k*', 'MarkerSize',4);
%    title('wind direction');
% xlabel('month'),ylabel('wind direction')
xlim([1,365])
xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
xt=(1:12)*365/12;
set(gca,'xtick',xt);
set(gca,'XTickLabel',xtl);
set(gca,'FontName','Calibri Light','FontSize',11);
width=440;height=250;
left=200;bottem=600;
set(gcf,'position',[left,bottem,width,height])
grid off

  load x_2018
    x=x(:,1:12);%±±¾©
    x=sum(x')'./12;
    x=reshape(x,24,365);
%     pm25=sum(x)./24;
    pm25=max(x);
    figure
    plot(1:365,pm25,'k-');
   % title('pm2.5 average');
   % xlabel('month'),ylabel('PM2.5')
    xlim([1,365])
    xtl={'1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '10'; '11'; '12'};
    xt=(1:12)*365/12;
    set(gca,'xtick',xt);    
    set(gca,'XTickLabel',xtl);
   set(gca,'FontName','Calibri Light','FontSize',11)
    width=440;height=250;
    left=200;bottem=600;
    set(gcf,'position',[left,bottem,width,height])
    grid off

clc
clear 
close all
%%%%%%figure 1 a b
dataFile=['Figure 1_1data.xlsx']
[num,txt,raw]=xlsread(dataFile,1,'A2:D937');
Tsh1=num(:,3);
Tsh2=num(:,4);
Stationlat=num(:,2);
Stationlon=num(:,1);

Xxi=linspace(min(Stationlon),max(Stationlon),100);
Yyi=linspace(min(Stationlat),max(Stationlat),100);
Zzi=griddata(Stationlon,Stationlat,Tsh1,Xxi,Yyi','cubic'); 
right=max(Tsh1);
left=min(Tsh1);
Zzi=right.*(Zzi>right)+Zzi.*(Zzi<=right);
Zzi=left.*(Zzi<=left)+Zzi.*(Zzi>left); 
a=shaperead('.\map\bou2_4l.shp');
lon=[a(:).X];
lat=[a(:).Y];
load cmap_TshMap
figure
hold on 
contourf(Xxi,Yyi,Zzi);
caxis([0 500])
plot(lon,lat,'b','LineWidth', 1.2);
axis off
colorbar('position', [0.92 0.2 0.03 0.7]);%x,y,width, heigh
set(gcf,'colormap',cmap)
set(gcf, 'position', [50,50,700,550])
hold off

Xxi=linspace(min(Stationlon),max(Stationlon),100);
Yyi=linspace(min(Stationlat),max(Stationlat),100);
Zzi=griddata(Stationlon,Stationlat,Tsh2,Xxi,Yyi','cubic'); 
right=max(Tsh2);
left=min(Tsh2);
Zzi=right.*(Zzi>right)+Zzi.*(Zzi<=right);
Zzi=left.*(Zzi<=left)+Zzi.*(Zzi>left); 
a=shaperead('.\map\bou2_4l.shp');
lon=[a(:).X];
lat=[a(:).Y];
load cmap_TshMap
figure
hold on 
contourf(Xxi,Yyi,Zzi);
caxis([0 500])
plot(lon,lat,'b','LineWidth', 1.2);
axis off
colorbar('position', [0.92 0.2 0.03 0.7]);%x,y,width, heigh
set(gcf,'colormap',cmap)
set(gcf, 'position', [30,30,700,550])
hold off
       
clc
clear 
close all
%%%%%%figure 1 c d
dataFile=['Figure 1_2data.xlsx']
[num,txt,raw]=xlsread(dataFile,1,'A2:D1067');
Tsh1=num(:,3);
Tsh2=num(:,4);
Stationlat=num(:,2);
Stationlon=num(:,1);

Xxi=linspace(min(Stationlon),max(Stationlon),100);
Yyi=linspace(min(Stationlat),max(Stationlat),100);
Zzi=griddata(Stationlon,Stationlat,Tsh1,Xxi,Yyi','cubic'); 
right=max(Tsh1);
left=min(Tsh1);
Zzi=right.*(Zzi>right)+Zzi.*(Zzi<=right);
Zzi=left.*(Zzi<=left)+Zzi.*(Zzi>left); 
a=shaperead('.\map\bou2_4l.shp');
lon=[a(:).X];
lat=[a(:).Y];
load cmap_TshMap
figure
hold on 
contourf(Xxi,Yyi,Zzi);
caxis([0 500])
plot(lon,lat,'b','LineWidth', 1.2);
axis off
colorbar('position', [0.92 0.2 0.03 0.7]);%x,y,width, heigh
set(gcf,'colormap',cmap)
set(gcf, 'position', [50,50,700,550])
hold off

Xxi=linspace(min(Stationlon),max(Stationlon),100);
Yyi=linspace(min(Stationlat),max(Stationlat),100);
Zzi=griddata(Stationlon,Stationlat,Tsh2,Xxi,Yyi','cubic'); 
right=max(Tsh2);
left=min(Tsh2);
Zzi=right.*(Zzi>right)+Zzi.*(Zzi<=right);
Zzi=left.*(Zzi<=left)+Zzi.*(Zzi>left); 
a=shaperead('.\map\bou2_4l.shp');
lon=[a(:).X];
lat=[a(:).Y];
load cmap_TshMap
figure
hold on 
contourf(Xxi,Yyi,Zzi);
caxis([0 500])
plot(lon,lat,'b','LineWidth', 1.2);
axis off
colorbar('position', [0.92 0.2 0.03 0.7]);%x,y,width, heigh
set(gcf,'colormap',cmap)
set(gcf, 'position', [30,30,700,550])
hold off
       

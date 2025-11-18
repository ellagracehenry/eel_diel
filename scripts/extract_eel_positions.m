cd('/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/diel_cycle_garden_eel/scripts')

%load in image
img = imread('shape_Ne3PDWkGy26S-dRxs5bjE at 25-10-18 15.02.08.png');
imshow(img);
[coordsC, coordsR] = getpts() %click on eel bases
imgcoordsRC = [coordsR coordsC] %put coordinates in one file
imgcoordsRC = [imgcoordsRC [1:93]'] %add ids
writematrix(imgcoordsRC, 'garden_eel_diel-310525-D4-cam2.csv')  %save coordinate file CHANGE NAME
%load in image
img = imread('cam1.png');
imshow(img);
[coordsC, coordsR] = getpts() %click on eel bases
imgcoordsRC = [coordsR coordsC]; %put coordinates in one file
imgcoordsRC = [[1:length(imgcoordsRC)]' imgcoordsRC]; %add ids

img = imread('cam2.png');
imshow(img);
[coordsC, coordsR] = getpts() %click on eel bases
imgcoordsRC1 = [coordsR coordsC]; %put coordinates in one file
imgcoordsRC3 = [imgcoordsRC imgcoordsRC1;] %add ids

% Your column headers (cell array of strings/char vectors)
headers = {'ID', 'x_cam1', 'y_cam1','x_cam2','y_cam2'};

% Create a table
T = array2table(imgcoordsRC3, 'VariableNames', headers);

writetable(T, 'annotations3.csv')






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%i = 1
%for k = 1:(length(imgcoordsRC)/2)
%    imgcoordsRC_ind(k,:) = [k, imgcoordsRC(i,1), imgcoordsRC(i,2), imgcoordsRC(i+1,1), imgcoordsRC(i+1,2)]
%    i = i + 2;
%end

% Your column headers (cell array of strings/char vectors)
%headers = {'ID', 'x_head', 'y_head','x_tail','y_tail'};

% Create a table
%T = array2table(imgcoordsRC_ind, 'VariableNames', headers);

%writetable(T, 'AO_disorder.csv')  %save coordinate file CHANGE NAME

%img = imread('AO_order.png');
%imshow(img);
%hold on

% Plot IDs on image at the head positions
%for k = 1:height(T)
%    y = T.x_head(k);
%    x = T.y_head(k);
%    id = T.ID(k);
%    text(x, y, num2str(id), 'Color', 'r', 'FontSize', 12, 'FontWeight', 'bold');
%end
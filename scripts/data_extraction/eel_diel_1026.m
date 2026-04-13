%initialise file path
cd /scratch/alpine/elhe2720/eel_diel/garden_eel_diel-090625-D2-cam2/
folderPath = 'frames';
filePattern = fullfile(folderPath, 'frames_*');
folderFiles = dir(filePattern);
segment_name_list = {folderFiles.name};
segment_name_list = string(segment_name_list);

%Load in coords
coordsPath = "garden_eel_diel-020625-F1-cam2.csv";
imgcoordsRC = readmatrix(coordsPath);


%OR get points
ss = 4
idx = 1
k = 1
imageFolder = 'frames';
filePattern = fullfile(imageFolder, segment_name_list{ss}, '*.png');
imagesFiles = dir(filePattern);
imagesFiles = imagesFiles(idx);
baseFileName = imagesFiles(k).name;
fullFileName = fullfile(imageFolder, segment_name_list(ss), baseFileName);
I = imread(fullFileName);
imshow(I)
[coordsC coordsR] = getpts()
imgcoordsRC = [coordsR coordsC]
imgcoordsRC = [imgcoordsRC [1:length(coordsR)]']

writematrix(imgcoordsRC, "garden_eel_diel-020625-L4-cam2.csv")

%Turn coordinates into box
ss = 9
idx = 48
k = 1
imageFolder = 'frames';
filePattern = fullfile(imageFolder, segment_name_list{ss}, '*.png');
imagesFiles = dir(filePattern);
imagesFiles = imagesFiles(idx);
baseFileName = imagesFiles(k).name;
fullFileName = fullfile(imageFolder, segment_name_list(ss), baseFileName);
I = imread(fullFileName);
clear bbox
for lc = 1:size(imgcoordsRC,1)

bwidth = 20;
bheight = 20; 
yCenter = imgcoordsRC(lc,1);
xCenter = imgcoordsRC(lc,2);
xLeft = xCenter - bwidth/2;
yTop = yCenter - 2*bheight/3;

bbox(lc,1) = xLeft;
bbox(lc,2) = yTop;
bbox(lc,3) = bwidth;
bbox(lc,4) = bheight;
bbox(lc,5) = imgcoordsRC(lc,3);
end

annotatedImage = insertShape(I,"rectangle",bbox(:,1:4),"LineWidth",1);
annotatedImageText = insertText(I, bbox(:,1:2), bbox(:,5), 'FontSize',25); %adding labels
imshow(annotatedImageText) %check the boxes align with the first frame 

display_frame_function(65, 457, segment_name_list, imgcoordsRC)
writematrix(imgcoordsRC,'garden_eel_diel-310525-F2-cam1.csv')  %save coordinate file CHANGE NAME

transitions_All(28, 26438:36865) = 0;

%Check and change
idx = 1
idx1 = 250
ss = 4
k = 1
imbin = 0.45

for ss = 1:length(segment_name_list)
    if mod(ss,1)==0
        imageFolder = 'frames';
        filePattern = fullfile(imageFolder, segment_name_list{ss}, '*.png');
        imagesFiles = dir(filePattern);

        imagesFiles = imagesFiles(idx);
        baseFileName = imagesFiles(k).name;
        fullFileName = fullfile(imageFolder, segment_name_list(ss), baseFileName);
        I = imread(fullFileName);
        imgcoordsRC = displayAndEditPointsImages_1010(I,imgcoordsRC, bbox, imbin);

        % imageFolder = 'frames';
        % filePattern = fullfile(imageFolder, segment_name_list{ss}, '*.png');
        % imagesFiles = dir(filePattern);
        % 
        % imagesFiles = imagesFiles(idx1);
        % baseFileName = imagesFiles(k).name;
        % fullFileName = fullfile(imageFolder, segment_name_list(ss), baseFileName);
        % I = imread(fullFileName);
        % imgcoordsRC = displayAndEditPointsImages_1010(I,imgcoordsRC, bbox, imbin);
    else
    end
end


%step 2 - get transitions out
if isempty(gcp('nocreate'))
    parpool('local');
end

%%batch process
transitions = zeros(size(bbox,1), 512, length(segment_name_list));
imbin = 0.47
batchSize = 1; 
numSegments = length(segment_name_list);
numBatches = ceil(numSegments / batchSize);

%use this!
for b =41:73
    disp(['Processing batch ', num2str(b), ' of ', num2str(numBatches)]);
    
    % Determine which segments belong to this batch
    startIdx = (b-1)*batchSize + 1;
    endIdx = min(b*batchSize, numSegments);
    batchIndices = startIdx:endIdx;

    % After batch finishes, check in visually (once per batch)
    % You could show the first frame of the first segment in the batch, for example
    checkFrame = 1;
    segToDisplay = batchIndices(1)
    imageFolder = 'frames';
    filePattern = fullfile(imageFolder, segment_name_list{segToDisplay}, '*.png');
    imagesFiles = dir(filePattern);
    if ~isempty(imagesFiles)
        I = imread(fullfile(imageFolder, segment_name_list{segToDisplay}, imagesFiles(checkFrame).name));
        imgcoordsRC = displayAndEditPointsImages_0126(I, imgcoordsRC, bbox, imbin);
    end

    % segToDisplay = batchIndices(batchSize)
    checkFrame = 500;
    imageFolder = 'frames';
    filePattern = fullfile(imageFolder, segment_name_list{segToDisplay}, '*.png');
    imagesFiles = dir(filePattern);
    if ~isempty(imagesFiles)
        I = imread(fullfile(imageFolder, segment_name_list{segToDisplay}, imagesFiles(checkFrame).name));
        imgcoordsRC = displayAndEditPointsImages_0126(I, imgcoordsRC, bbox, imbin);
    end

    % Parallelize within this batch
    for ss = batchIndices

    disp(['Starting iteration ss = ', num2str(ss)]);
  

    imageFolder = 'frames';
    filePattern = fullfile(imageFolder, segment_name_list{ss}, '*.png');
    imagesFiles = dir(filePattern);

    %Parameters
    imbin = .47;

    %Load in frames
    numFrames = length(imagesFiles);
    frames = cell(numFrames, 1); % Pre-allocate a cell array to store frames

    bbox = zeros(length(imgcoordsRC), 6);

    %Turn coordinates into box
    for lc = 1:size(imgcoordsRC,1)

        bwidth = 20;
        bheight = 20; 
        yCenter = imgcoordsRC(lc,1);
        xCenter = imgcoordsRC(lc,2);
        xLeft = xCenter - bwidth/2;
        yTop = yCenter - 2*bheight/3;

        bbox(lc,1) = xLeft;
        bbox(lc,2) = yTop;
        bbox(lc,3) = bwidth;
        bbox(lc,4) = bheight;
        bbox(lc,5) = imgcoordsRC(lc,3);
    end

   local_transitions = zeros(size(bbox,1), 512);  % Temporary variable for this segment


    for img = 1:numFrames
    % if img == 250 
    %     baseFileName = imagesFiles(img).name;
    %     fullFileName = fullfile(imageFolder, segment_name_list{ss}, baseFileName);
    %     I = imread(fullFileName);
    %     imgcoordsRC = displayAndEditPointsImages_1010(I, imgcoordsRC, bbox, imbin);
    % 
    %     bbox = zeros(length(imgcoordsRC), 6);
    % 
    %     %Turn coordinates into box
    %     for lc = 1:size(imgcoordsRC,1)
    % 
    %         bwidth = 20;
    %         bheight = 20; 
    %         yCenter = imgcoordsRC(lc,1);
    %         xCenter = imgcoordsRC(lc,2);
    %         xLeft = xCenter - bwidth/2;
    %         yTop = yCenter - 2*bheight/3;
    % 
    %         bbox(lc,1) = xLeft;
    %         bbox(lc,2) = yTop;
    %         bbox(lc,3) = bwidth;
    %         bbox(lc,4) = bheight;
    %         bbox(lc,5) = imgcoordsRC(lc,3);
    %     end
    % 
    % end

    % if img == 350 
    %     baseFileName = imagesFiles(img).name;
    %     fullFileName = fullfile(imageFolder, segment_name_list{ss}, baseFileName);
    %     I = imread(fullFileName);
    %     imgcoordsRC = displayAndEditPointsImages_1010(I, imgcoordsRC, bbox, imbin);
    % 
    %     bbox = zeros(length(imgcoordsRC), 6);
    % 
    %     Turn coordinates into box
    %     for lc = 1:size(imgcoordsRC,1)
    % 
    %         bwidth = 20;
    %         bheight = 20; 
    %         yCenter = imgcoordsRC(lc,1);
    %         xCenter = imgcoordsRC(lc,2);
    %         xLeft = xCenter - bwidth/2;
    %         yTop = yCenter - 2*bheight/3;
    % 
    %         bbox(lc,1) = xLeft;
    %         bbox(lc,2) = yTop;
    %         bbox(lc,3) = bwidth;
    %         bbox(lc,4) = bheight;
    %         bbox(lc,5) = imgcoordsRC(lc,3);
    %     end
    % 
    % end

    baseFileName = imagesFiles(img).name;
    fullFileName = fullfile(imageFolder, segment_name_list{ss}, baseFileName);
    I = imread(fullFileName);
    frame = I;
    II = rgb2gray(frame);
    %Icontrast = adapthisteq(II, ...
    %'ClipLimit', 0.005, ...       % VERY low = gentle
    %'NumTiles', [4 4], ...        % fewer tiles = less local boosting
    %'Distribution', 'uniform'); 
     % 2. Background subtraction
     %background = imgaussfilt(Icontrast, 4);
     %Icor = im2double(Icontrast) - im2double(background);
     %Icor = mat2gray(Icor);
     Icor = imadjust(II, [0.1 0.8]);
     %imshow(Icor)
    %Icontrast = imadjust(II, [0.1 0.8]);
    BW = imbinarize(Icor, 'adaptive', ...
        'ForegroundPolarity', 'dark', 'Sensitivity', imbin);
    BW1 = ~BW;
    BW2 = bwareaopen(BW1, 20, 4);

        bbox = floor(bbox);

        for lbb = 1:size(bbox,1)
            burrow_px = BW2(bbox(lbb,2):bbox(lbb,2)+bbox(lbb,4),bbox(lbb,1):bbox(lbb,1)+bbox(lbb,3));
            wt_burrow_px = sum(sum(burrow_px));
            tot_burrow_px = length(burrow_px)*width(burrow_px);
            prop_wt_burrow_px = wt_burrow_px/tot_burrow_px; %proportion of white pixels in that box
            %imshow(burrow_px)

               %if lbb == 3
                %    if prop_wt_burrow_px >= 0.3125 %0.05 %0.0096 %0.06 %0.1 %0.01225 0.025 

                 %       local_transitions(lbb, img) = 1; %out

                  %  else

                   %     local_transitions(lbb, img) = 0; %not out

                    %end

               %else
                    if prop_wt_burrow_px >= 0.03 %0.05 %0.0096 %0.06 %0.1 %0.01225 0.025 

                        local_transitions(lbb, img) = 1; %out

                    else

                        local_transitions(lbb, img) = 0; %not out

                    end
               %end


        end


    end

    % After processing the segment, assign the result back to the global 'transitions'
    transitions(:, :, ss) = local_transitions;
    end

    disp(['--- Batch ', num2str(b), ' complete. Checking coordinates... ---']);
   
end

transitions_10 = transitions(:,:,:);
transitions_shaped = reshape(transitions, size(transitions, 1), []);
transitions_All = [bbox(:, 5), transitions_shaped];

transitions_All = transitions_All(:,1:22562);

transitions_total = [transitions_old, transitions];

transitions_total = cat(3, transitions_old, transitions);
transitions_total_shaped = reshape(transitions_total, size(transitions_total, 1), []);
transitions_total_All = [bbox(:, 5), transitions_shaped];

% Create a zero row
zero_row = zeros(1, 512, 66);
% Append it along the first dimension
A_new = cat(1, transitions, zero_row);

%when finished a segment
writematrix(transitions_All,'transitions_L1_020625_complete.csv') %CHANGE the filename bit to whichever video you have been editing

writematrix(transitions_total_All, 'transitions_L1_290525_complete.csv')

%mid save
save('transitions_upto40');
load('transitions_upto36');
save('start_ss26')

transitions = readmatrix("transitions_L1_050625_complete.csv");

transitions_up_to_30 = transitions(:,:,1:30);
transitions_31to56 = transitions(:,:,33:72);

transitions = cat(3,transitions_up_to_30, transitions_31to56);

A = transitions(:,2:36353);         
transitions = reshape(A,58,512,71);

top_IDs = setdiff(bottom_trans_ID, topimgcoords);

logicalIndex = ismember(bottom_transitions(:,1),top_IDs);

bottom_transitions_only = bottom_transitions(logicalIndex,:);

transitions_both = vertcat(bottom_transitions_only,transitions_All);
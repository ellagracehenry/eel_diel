function display_frame_function(ss, idx, segment_name_list, imgcoordsRC)
    imageFolder = 'frames';
    filePattern = fullfile(imageFolder, segment_name_list{ss}, '*.png');
    imagesFiles = dir(filePattern);
    imagesFiles = imagesFiles(idx);
    baseFileName = imagesFiles(1).name;
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
    annotatedImageText = insertText(I, bbox(:,1:2), bbox(:,5), 'FontSize',15); %adding labels
    imshow(annotatedImageText) 
end
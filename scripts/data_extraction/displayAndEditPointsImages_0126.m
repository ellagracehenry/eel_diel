function updatedImgcoordsRC = displayAndEditPointsImages_1010(I, imgcoordsRC, bbox, imbin)
    % ===============================================================
    % displayAndEditPointsImages_1010
    % Display image with draggable boxes that can be batch-shifted
    % ===============================================================

    frame = I;
    II = rgb2gray(frame);
    % Icontrast = adapthisteq(II, ...
    % 'ClipLimit', 0.001, ...       % VERY low = gentle
    % 'NumTiles', [4 4], ...        % fewer tiles = less local boosting
    % 'Distribution', 'uniform'); 
     % 2. Background subtraction
     % background = imgaussfilt(II, 4);
     % Icor = im2double(II) - im2double(background);
     % Icor = mat2gray(Icor);
     Icor = imadjust(II, [0.1 0.8]);
     %imshow(Icor)
    %Icontrast = imadjust(II, [0.1 0.8]);
    BW = imbinarize(Icor, 'adaptive', ...
        'ForegroundPolarity', 'dark', 'Sensitivity', imbin);
    BW1 = ~BW;
    BW2 = bwareaopen(BW1, 20, 4);

    % Turn coordinates into boxes
    clear bbox
    for lc = 1:size(imgcoordsRC, 1)
        bwidth = 20;
        bheight = 20;
        yCenter = imgcoordsRC(lc, 1);
        xCenter = imgcoordsRC(lc, 2);
        xLeft = xCenter - bwidth / 2;
        yTop = yCenter - 2 * bheight / 3;
        bbox(lc, :) = [xLeft, yTop, bwidth, bheight, imgcoordsRC(lc, 3)];
    end

    % --- Display setup ---
    h_fig = figure('Name', 'Edit Points');
    h_img = imshow(imoverlay(I, BW2, 'red'));
    title('Drag boxes, then click Save Points or Apply Transformation');

    set(h_fig, 'KeyPressFcn', @(src, event) moveImage(src, event, h_img));

    y = imgcoordsRC(:, 1);
    x = imgcoordsRC(:, 2);
    ids = imgcoordsRC(:, 3);

    rectHandles = cell(length(x), 1);
    textHandles = cell(length(x), 1);

    firstRectMoved = false;
    firstMovedIndex = 0;
    firstRectOriginalPos = [];

    % --- Draw boxes and make draggable ---
    for i = 1:length(x)
        xLeft = bbox(i, 1);
        yTop = bbox(i, 2);
        bwidth = bbox(i, 3);
        bheight = bbox(i, 4);

        h = imrect(gca, [xLeft, yTop, bwidth, bheight]);
        setPositionConstraintFcn(h, @(pos) constrainRect(pos, size(frame)));
        rectHandles{i} = h;

        textHandles{i} = text(x(i), y(i) - 10, num2str(ids(i)), ...
            'Color', 'yellow', 'FontSize', 10, 'HorizontalAlignment', 'center');

        addNewPositionCallback(h, @(pos) updateRectangle(h, textHandles{i}, pos, i));
        addNewPositionCallback(h, @(pos) trackFirstMovedRect(i));
    end

    % --- Draw old boxes as dashed red rectangles ---
    for i = 1:size(bbox, 1)
        rectangle('Position', bbox(i,1:4), 'EdgeColor', 'g', 'LineStyle', '--', 'LineWidth', 1.5);
    end

    ax = gca;
    set(gcf, 'WindowScrollWheelFcn', @(src, event) zoomWithScroll(src, event, ax));

    % --- Buttons ---
    uicontrol('Style', 'pushbutton', 'String', 'Save Points', ...
        'Position', [20 20 100 30], 'Callback', @savePoints);

    uicontrol('Style', 'pushbutton', 'String', 'Apply Transformation to All Points', ...
        'Position', [140 20 200 30], 'Callback', @applyTransformation);

    uiwait(gcf);

    % ===============================================================
    % Nested Functions
    % ===============================================================

    function savePoints(~, ~)
        newPositions = zeros(length(rectHandles), 2);
        for j = 1:length(rectHandles)
            rectPos = getPosition(rectHandles{j});
            newPositions(j, :) = [rectPos(1) + rectPos(3) / 2, rectPos(2) + 2 * rectPos(4) / 3];
        end
        updatedImgcoordsRC = [newPositions(:, 2), newPositions(:, 1), ids];
        set(gcf, 'UserData', updatedImgcoordsRC);
        disp(' Points updated');
        uiresume(gcf);
        close(gcf);
    end

    function trackFirstMovedRect(index)
        if ~firstRectMoved
            firstRectMoved = true;
            firstMovedIndex = index;
            firstRectOriginalPos = getPosition(rectHandles{index});
            disp(['Tracking first moved rectangle (index ' num2str(index) ')']);
        end
    end

    function applyTransformation(~, ~)
        if ~firstRectMoved
        disp('No rectangle has been moved yet.');
        return;
        end

        % Get the original and current rectangle positions in data coordinates
        origPos = firstRectOriginalPos; 
        currPos = getPosition(rectHandles{firstMovedIndex});

        % Compute translation delta (data units, same as image coordinates)
        delta = currPos(1:2) - origPos(1:2);

        % Apply that same translation to every rectangle
        for j = 1:length(rectHandles)
            if j == firstMovedIndex
                continue;  % Skip the rectangle that was already moved manually
            end
            rectPos = getPosition(rectHandles{j});
            newPos = [rectPos(1:2) + delta, rectPos(3:4)];
            setPosition(rectHandles{j}, newPos);
            updateRectangle(rectHandles{j}, textHandles{j}, newPos, j);
        end

         disp(['All rectangles shifted by x = ', num2str(delta(1)), ...
              ', y = ', num2str(delta(2)), ' (data units).']);
     end


    function updateRectangle(rectHandle, textHandle, pos, ~)
        newX = pos(1) + pos(3) / 2;
        newY = pos(2) + 2 * pos(4) / 3 - 10;
        set(textHandle, 'Position', [newX, newY]);
    end

    function constrainedPos = constrainRect(pos, frameSize)
        xLim = [1, frameSize(2) - pos(3)];
        yLim = [1, frameSize(1) - pos(4)];
        constrainedPos = [min(max(pos(1), xLim(1)), xLim(2)), ...
                          min(max(pos(2), yLim(1)), yLim(2)), ...
                          pos(3), pos(4)];
    end

    function zoomWithScroll(~, event, ax)
        xlim = get(ax, 'XLim');
        ylim = get(ax, 'YLim');
        cp = get(ax, 'CurrentPoint');
        xm = cp(1, 1);
        ym = cp(1, 2);
        zf = 1.5;
        if event.VerticalScrollCount > 0 % zoom in
            newX = [xm - (xm - xlim(1)) / zf, xm + (xlim(2) - xm) / zf];
            newY = [ym - (ym - ylim(1)) / zf, ym + (ylim(2) - ym) / zf];
        else
            newX = [xm - (xm - xlim(1)) * zf, xm + (xlim(2) - xm) * zf];
            newY = [ym - (ym - ylim(1)) * zf, ym + (ylim(2) - ym) * zf];
        end
        set(ax, 'XLim', newX, 'YLim', newY);
    end

    function moveImage(~, event, h_img)
        xData = get(h_img, 'XData');
        yData = get(h_img, 'YData');
        step = 10;
        switch event.Key
            case 'leftarrow'
                set(h_img, 'XData', xData - step);
            case 'rightarrow'
                set(h_img, 'XData', xData + step);
            case 'uparrow'
                set(h_img, 'YData', yData - step);
            case 'downarrow'
                set(h_img, 'YData', yData + step);
        end
    end
end

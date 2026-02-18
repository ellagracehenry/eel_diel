%Load in data
coord_filename = "annotations_L1_290525_3D.xlsx" %ADD NAME OF BALL DROP FILE WITH 3D COORDS
imgcoordsRC = readtable(coord_filename);
%imgcoordsRC = Tall;

figure
hold on
grid on

for k = 1:size(imgcoordsRC,1)
    % Extract coordinates for base, head, and ball points
    base_x = imgcoordsRC{k,15};
    base_y = imgcoordsRC{k,16};
    base_z = imgcoordsRC{k,17};
    

    % Skip if any key coordinates are missing
    if any(isnan([base_x, base_y, base_z]))
        continue
    end
    % --- Plot the eel base ---
    scatter3(base_x, base_y, base_z, 15, [0.65, 0.16, 0.16], 'filled') % brown base

    % --- Add label (optional) ---
    text(base_x, base_y, base_z, num2str(imgcoordsRC{k,2}), ...
         'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right', ...
         'Color', 'b', 'FontSize', 6)

end



xlabel('X'); ylabel('Y'); zlabel('Z');
title('PC Ball Drop and Eel 3D Visualization');
axis equal
xlim([-2 2.5])
ylim([-1.2 0.8])
hold off


sqrt((imgcoordsRC{28,56} - imgcoordsRC{33,56})^2 + (imgcoordsRC{28,57} - imgcoordsRC{33,57})^2 + (imgcoordsRC{28,58} - imgcoordsRC{33,58})^2)


Tref = readtable("garden_eel_diel-280525_D2/D2_28-05-25_annotations_3d.xlsx")
Tnew = readtable("garden_eel_diel-090625-D2/D2_09-06-25_annotations_3d.xlsx")

idsToExclude = [6 7 9 8 12 11 23 24];

Tnew = Tnew(~ismember(Tnew.ID, idsToExclude), :);

% Find shared IDs
[commonIDs, ia, ib] = intersect(Tref.ID, Tnew.ID);

% Reference coordinates
Xref = [Tref.positions_X(ia), ...
        Tref.positions_Y(ia), ...
        Tref.positions_Z(ia)];

% Coordinates from the second table (same IDs)
Xnew_common = [Tnew.positions_X(ib), ...
               Tnew.positions_Y(ib), ...
               Tnew.positions_Z(ib)];

[d, ~, transform] = procrustes(Xref, Xnew_common, 'Scaling', false);

% Find rows that are NOT shared (the 2 new IDs)
idx_new = ~ismember(Tnew.ID, commonIDs);

Xnew_only = [Tnew.positions_X(idx_new), ...
             Tnew.positions_Y(idx_new), ...
             Tnew.positions_Z(idx_new)];

% Transform into reference frame
Xnew_only_ref = transform.b * Xnew_only * transform.T + transform.c(ones(size(Xnew_only,1),1),:);



Tnew_ref = Tnew(idx_new, :);
Tnew_ref.positions_X = Xnew_only_ref(:,1);
Tnew_ref.positions_Y = Xnew_only_ref(:,2);
Tnew_ref.positions_Z = Xnew_only_ref(:,3);

Tall = [Tref; Tnew_ref];

writetable(Tall, 'D2_IDs1-28.csv')
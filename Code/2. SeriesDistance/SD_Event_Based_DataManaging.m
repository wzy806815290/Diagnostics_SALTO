%% Start from scratch
clc;
clear all;
close all;

%% specify paths and parameters, read inputs
cd('Y:/Home/wangzhen/Eval_SALTO/');  % set working directory
addpath(genpath(pwd)); % add working directory to searchpath

% check all the MATLAB result files
fn = dir(fullfile('./Out/lumped_SD_1979_2002_KGE_Balanced/', '*.mat'));

% initialize the parameters
PreEvent_days = 10;
n_Ev_type = 16;
E_type_list = ["SM" "ROI" "ROS" "MIX" "R.WET.INT.LOC" "R.WET.INT.EXT" "R.WET.VOL.LOC.!OVER" "R.WET.VOL.LOC.OVER" ...
                "R.WET.VOL.EXT.!OVER" "R.WET.VOL.EXT.OVER" "R.DRY.INT.LOC.STD" "R.DRY.INT.!STD" "R.DRY.INT.EXT.STD" ...
                "R.DRY.VOL.LOC" "R.DRY.VOL.EXT.STD" "R.DRY.VOL.EXT.!STD"];


%% Classify the SD results based on the event types
% set workers 
%workers = parpool(20);
parfor i = 1:length(fn)
    % get catchment id
    Cat_id_i = erase(fn(i).name, ".mat")
    %Err_Ev(i).Cat_id = Cat_id_i;
    %Err_PreEv(i).Cat_id = Cat_id_i;

    % load SD results of each catchment
    Data = load(fn(i).name);
    %f_PlotConnectedSeries(obs, segs_obs_opt_all, sim, segs_sim_opt_all, connectors) 
    Qres = Data.Qres;
    connectors = Data.connectors;
    segs_obs_opt_all = Data.segs_obs_opt_all;

    e_sd_t_all = (connectors.x_match_obs_global - connectors.x_match_sim_global);
    e_sd_q_all = (connectors.y_match_obs - connectors.y_match_sim) ./ ((connectors.y_match_obs + connectors.y_match_sim) .* 0.5);

    % define rising and dropping limbs
    Qres.hydcase(:) = 0;
    Qres.et(:) = 0;
    [C, ia, ic] = unique(connectors.x_match_obs_global);
    Qres.et(connectors.x_match_obs_global(1):end) = interp1(C, e_sd_t_all(ia), connectors.x_match_obs_global(1):height(Qres));
    Qres.eq(:) = 0;
    Qres.eq(connectors.x_match_obs_global(1):end) = interp1(C, e_sd_q_all(ia), connectors.x_match_obs_global(1):height(Qres));
    for seg_i = 1:length(segs_obs_opt_all)
        hydcase = 0;
        starttime_seg_i = segs_obs_opt_all(seg_i).starttime_global;
        endtime_seg_i = segs_obs_opt_all(seg_i).endtime_global;
        length_seg_i = segs_obs_opt_all(seg_i).length;
        if length_seg_i > 1
            diff_q = Qres.obs(endtime_seg_i) - Qres.obs(starttime_seg_i);
            if diff_q > 0
                hydcase = 1;
            else
                hydcase = -1;
            end
            Qres.hydcase((starttime_seg_i + 1):(endtime_seg_i - 1)) = hydcase;
        end

        Qres.hydcase(starttime_seg_i) = -2 * hydcase;
        Qres.hydcase(endtime_seg_i) = 2 * hydcase;
    end
    dir = ['./Out/lumped_SD_ts_1979_2002_KGE_Balanced/' Cat_id_i '.txt' ];
    writetable(Qres, dir, 'Delimiter', ' ');
end
%  ====================================================================== 
%                 Series Distance Analysis (Continuous Mode) 
%  ======================================================================
%  last modifaction 16.08.2016

% This development release of the SeriesDistance (SD) method is applicable to continuous 
% observed (obs) and simulated (sim) discharge time series. It outputs a 2d-error 
% distribution for the entire time series. Contrary to the event based method the 
% continuous version does not differentiate periods of low-flow from periods of events.  
% Nevertheless, the entire hydrograph is classified into rising and falling limbs. The 
% interpretation of these classes is however only meaningful if the periods of low-flow 
% do not cover a significant portion of the hydrograph. Note: For plotting,
% the error distributions for rise and fall are combined.

% Further information:
% Seibert, S. P., Ehret, U., and Zehe, E., 2016: Disentangling timing and amplitude errors in streamflow simulations, Hydrol. Earth Syst. Sci.
% Ehret, U., Zehe, E., 2011. Series distance - An intuitive metric to quantify hydrograph similarity in terms of occurrence, amplitude and timing of hydrological events. Hydrol. Earth Syst. Sci. 15, 877896. doi:10.5194/hess-15-877-2011

% Dependencies
% All required functions are stored in folder \functions

% Input (ascii format)
%  - obs: [n,1] matrix with equidistant and NaN-free time series of observed discharge data.
%  - sim: [n,1] matrix with equidistant and NaN-free time series of simulated discharge data. 
%  - timeseries_splits: [n,1] optional matrix with points in time where the 'obs' and 'sim' time series will be split to increase computational speed.
%     It must include the first and the last timestep of 'obs' and 'sim'. E.g. if 'obs' and 'sim' are [1100,1],
%     then 'timeseries_splits' could be (1, 250, 800, 1100)

% Parameters are explained and specified in the parameter block

% Outputs (all stored within a single binary file .mat)
%  - obs:              observed discharge (smoothed)
%  - sim:              simulated discharge
%  - segs_obs_opt_all: optimized segments in obs
%  - segs_sim_opt_all: optimized segments in sim
%  - connectors:       SD connectors
%  - e_sd_q_all:       SD magnitude errors for entire time series
%  - e_sd_t_all:       SD timing errors for entire time series
%  - parameters:       all parameters, i.e. error_model, objective function weights, smoothing parameters,  
%                       are included in the outputfile

%% Start from scratch
clc;
clear all;
close all;

%% specify paths and parameters, read inputs
cd('F:/Zhenyu/Eval_SALTO')  % set working directory
addpath(genpath(pwd)); % add working directory to searchpath

%% Intializing
% smoothing options
smooth_flag = false;                % smooth both obs and sim (default=true)
nse_smooth_limit = 0.99;            % specifies degree of smoothing according to NSE criterion (default=0.99)

% specification of the magnitude error model
error_model = 'relative';           % 'relative' or 'standard'; (standard: (sim-obs), relative: [(sim-obs)/((sim + obs)/2)] (default='relative')

% options for time series splitting:
timeseries_split_by_user = false;   % 'true': time series splits provided by user in ascii file. 'false': splits will be placed by the program (default = 'false')
split_frequency = 250;              % only required if timeseries_split_by_user='false': this is the default distance between 2 splits (default=500)

% parametrization of the objective function 
weight_nfc = 1/7;                   % weights number of re-assigned hydrological cases (default= 1)    
weight_rds = 1/7;                   % weights the importance of the re-assigned segments (default=1) 
weight_sdt = 5/7;                   % weights the SD timing error component (default=5)
weight_sdv = 0/7;                   % weights the SD magnitude error component (default=0)

% set plot flags 
pf_input = false;                   % plots input time series ('obs' and 'sim')
pf_segs_cons_entireTS = false;      % plots obs, sim, colour-coded pairs of matching segments, SeriesDistance connectors for the entire time series
pf_errordist = false;               % plots SeriesDistance error distributions 

% file route
indir_par = './Out/lumped_par_1990_2000_KGE_Balanced/';
indir_q = './Out/lumped_qout_1979_2002_KGE_Balanced/';
outdir_sd = './Out/lumped_SD_1979_2002_KGE_Balanced/';
outdir_ts = './Out/lumped_ts_split_1979_2002_KGE_Balanced/';
% outdir_sd_old1 = './Out/lumped_SD_1979_2002_KGE_Balanced/';
% outdir_ts_old1 = './Out/lumped_ts_split_1979_2002_KGE_Balanced/';
% outdir_sd_old2 = './Out/lumped_SD_1979_2002_KGE_Balanced_Norm/';
% outdir_ts_old2 = './Out/lumped_ts_split_1979_2002_KGE_Balanced_Norm/';
%best_col = "ipop.best";     % lump: "ipop.best"; dist: "igroup.best"
goodme_thres = 0.9;
minparset = 5;
minME_MAX = 0.3;
minNumSamples = 5;
%% Start the loop for investigating all the catchment files    
% Read catchment Info 
infile_Cat = strcat(indir_q, 'cat_table_ME.txt');
CATtbl = Tbl_Reader(infile_Cat);

% Read paramter bounds
infile_parbound = './Data/SALTO_V2_parameter.txt';
Parbound = readtable(infile_parbound, 'Delimiter', ' ', 'ConsecutiveDelimitersRule', 'join');

% set workers 
%workers = parpool(50);

parfor i = 1:height(CATtbl)
%for i = 1:height(CATtbl)
    % get catchment id
    Cat_id_i = CATtbl(i, "ID").Variables;
    %i_best = CATtbl(i, best_col).Variables;
    nparset = CATtbl(i, "iparset").Variables;
    BESTparset = CATtbl(i, "ipop.best").Variables;
    ME_MAX = CATtbl(i, "ME_MAX").Variables;
    % input and output filenames
    infile_Par = strcat(indir_par, 'DDS_', string(Cat_id_i), '.txt');
    infile_Q = strcat(indir_q, string(Cat_id_i), '.txt');
    infile_Ev = strcat('./Out/event_type_1979_2002/', string(Cat_id_i), '.txt');

    % read input
    if ~isfile(infile_Par) || ~isfile(infile_Q) || ~isfile(infile_Ev) || ME_MAX < minME_MAX || nparset < minparset
        continue
    end

    Partbl = readtable(infile_Par);
    Qseries = Tbl_Reader(infile_Q);
    Eseries = Tbl_Reader(infile_Ev);
    
    % Select the best parset as well as the top minNumSamples parset with 
    % ME > goodme_thres * ME_MAX and large parameter difference (distance). 
    PartblDist = Partbl;
    Parmin = Parbound(:, "PAR_MIN").Variables;
    Parmin = repmat(Parmin, [1 height(PartblDist)]);
    Parrange = Parbound(:, "PAR_MAX").Variables - Parbound(:, "PAR_MIN").Variables;
    Parrange = repmat(Parrange, [1 height(PartblDist)]);
    PartblDist(:, 2:end).Variables = (PartblDist(:, 2:end).Variables - Parmin') ./ Parrange';
    PartblDist(:, 'SM_LAYER').Variables = repmat(0, [height(PartblDist) 1]);
    PartblDist(:, 'TS').Variables = repmat(0, [height(PartblDist) 1]);

    PartblDist.idx = (1:height(PartblDist))';
    PartblDist = PartblDist(find(PartblDist(:, "ME").Variables > (goodme_thres * ME_MAX)), :);
    if height(PartblDist) < minNumSamples
        parset = PartblDist.idx';
    else
        DistList = [];
        for iparset = 1:height(PartblDist)
            DistList = [DistList, mean(dist(PartblDist(iparset, 2:(end-1)).Variables, PartblDist(:, 2:(end-1)).Variables'))];
        end
        PartblDist.dist = DistList';
        PartblDist(find(PartblDist(:, "idx").Variables == BESTparset), "dist") = {max(PartblDist.dist) + 1};
        PartblDist = sortrows(PartblDist, "dist", 'descend');
        parset = PartblDist.idx(1:minNumSamples)';
    end

    for iparset = parset
        txt = ['Cat ' num2str(Cat_id_i) ': iparset ' num2str(iparset)];
        disp (txt)

        try
            outfile = strcat(outdir_sd, string(Cat_id_i), '_output', string(iparset), '.mat');
            %outfile_old1 = strcat(outdir_sd_old1, string(Cat_id_i), '_output', string(iparset), '.mat');
            %outfile_old2 = strcat(outdir_sd_old2, string(Cat_id_i), '_output', string(iparset), '.mat');
            ts_split_file = strcat(outdir_ts, string(Cat_id_i), '_ts_splits', string(iparset), '.csv');
            %ts_split_file_old1 = strcat(outdir_ts_old1, string(Cat_id_i), '_ts_splits', string(iparset), '.csv');
            %ts_split_file_old2 = strcat(outdir_ts_old2, string(Cat_id_i), '_ts_splits', string(iparset), '.csv');

            % pass if there has been already a result
            if isfile(outfile)
                continue
            end
%             if isfile(outfile_old1)
%                 copyfile(outfile_old1, outfile);
%                 copyfile(ts_split_file_old1, ts_split_file);
%                 continue
%             end
% 
%             if isfile(outfile_old2)
%                 copyfile(outfile_old2, outfile);
%                 copyfile(ts_split_file_old2, ts_split_file);
%                 continue
%             end
            
            %Qres_org = join(Qseries(:, ["date", "Qobs", strcat("Qsim", string(i_best))]), ...
            %                Eseries(:, ["date", "event_type", "event_count"]));
            Qres_org = join(Qseries(:, ["date", "Qobs", strcat("Qsim", string(iparset))]), ...
                            Eseries(:, ["date", "event_type", "event_count"]));
            Qres_org.Properties.VariableNames = ["date", "obs", "sim", "event_type", "event_count"];
            Qres = rmmissing(Qres_org);
        
            % check validation of the input
            if height(Qres) < 1000
                continue
            end
        
            obs = Qres.obs;
            sim = Qres.sim;
       
            %% Data manipulations and pre-processing
            
            % allows to select a (1:to) subset of the provided time series, the remainder is ignored.
            %     to = 2000;
            %     [obs, sim, obs_events, sim_events, obs_sim_pairing] = f_inputsubset(to, obs, sim, obs_events, sim_events, obs_sim_pairing);
            %     clear to
            
            % smooth if required (default=true)
            if smooth_flag == true
                obs_org = obs;
                sim_org = sim;
                [obs, sim] = f_smooth_DP(obs, sim, nse_smooth_limit);   
            end
             
            % replace identical neigbouring values to avoid problems with assignment of unique peaks and valleys
            obs = f_ReplaceEqualNeighbours(obs);
            sim = f_ReplaceEqualNeighbours(sim);
            
            % Define time series split points to improve coarse-graining performance
            if timeseries_split_by_user == false
                if isfile(ts_split_file) == false
                    timeseries_splits = f_FindSplitPoints(obs, sim, split_frequency);   % find split points if they are not provided by the user       
                    csvwrite(ts_split_file, timeseries_splits);                         % save splits
                else
                    timeseries_splits = dlmread(ts_split_file, ';');        % read splits defined by the last computation.
                end
            else
                timeseries_splits = dlmread(ts_split_file, ';');        % read splits defined by user.
            end
        
            % plot input data
            if pf_input == true
                f_PlotInput([], obs, [], [], sim, [], [], timeseries_splits);          % show time series splits
            end
        
            % cleanup
            % clear smooth_flag nse_smooth_limit pf_input timeseries_split_by_user split_frequency
        
            %% Apply coarse-graining and the SD method to the entire time series 
                % note: contrary to the event based method both, the coarse-graining and the SD calculation 
                % take place in the same function here due to the splitting of the time series. To this end 
                % the splitting is solved in a simplistic way and does not support separating the 
                % coarse-graining and SD calculation as in the event-based version.
            
            % apply coarse graining and SD calculation: determines optimal level of segment aggregation for entire time series and applies SD to it
            [segs_obs_opt_all, segs_sim_opt_all, connectors, e_sd_t_all, e_sd_q_all] = f_CoarseGraining_SD_Continuous(Cat_id_i, obs, sim, timeseries_splits, ...
                weight_nfc,weight_rds,weight_sdt,weight_sdv,error_model);        
                
            % plot time series with optimized segments and connectors in an own figure
            if pf_segs_cons_entireTS == true
               f_PlotConnectedSeries(obs, segs_obs_opt_all, sim, segs_sim_opt_all, connectors) 
            end
            
            % plot 2d-error distributions
            if pf_errordist == true
                f_PlotSDErrors_OnePanel(Data.e_sd_t_all, Data.e_sd_q_all); 
            end
        
            %% save output
            f_outsave(outfile, Qres, obs, sim, segs_obs_opt_all, segs_sim_opt_all, connectors, e_sd_t_all, e_sd_q_all, ...
                               weight_nfc, weight_rds, weight_sdt, weight_sdv, error_model);   
        catch
            %error(['Error in File ' num2str(Cat_id_i)])
            error(['Error in No. ' num2str(i) ': File ' num2str(Cat_id_i) ' iparset ' num2str(iparset)])
        end
    end
end   
% 
Data=load("Y:\Home\wangzhen\Eval_SALTO\Out\lumped_SD_1979_2002_KGE_Balanced\105_output17.mat");    
Qres = Data.Qres;
obs = Qres.obs;
sim = Qres.sim;
connectors = Data.connectors;
segs_obs_opt_all = Data.segs_obs_opt_all;
segs_sim_opt_all = Data.segs_sim_opt_all;

e_sd_t_all = (connectors.x_match_obs_global - connectors.x_match_sim_global);
e_sd_q_all = (connectors.y_match_obs - connectors.y_match_sim) ./ ((connectors.y_match_obs + connectors.y_match_sim) .* 0.5);
Qres.hydcase(:) = 0;
Qres.et(:) = 0;
[C, ia, ic] = unique(connectors.x_match_obs_global);
Qres.et(connectors.x_match_obs_global(1):end) = interp1(C, e_sd_t_all(ia), connectors.x_match_obs_global(1):height(Qres));
Qres.eq(:) = 0;
Qres.eq(connectors.x_match_obs_global(1):end) = interp1(C, e_sd_q_all(ia), connectors.x_match_obs_global(1):height(Qres));
connectors2 = connectors;
connectors2.x_match_obs_global = [connectors.x_match_obs_global(1):connectors.x_match_obs_global(end)];
connectors2.y_match_obs = obs(connectors2.x_match_obs_global)';
connectors2.x_match_sim_global = connectors2.x_match_obs_global - Qres.et(connectors2.x_match_obs_global)';
connectors2.y_match_sim = interp1(C, connectors.y_match_sim(ia), connectors.x_match_obs_global(1):height(Qres));

%f_PlotInput([], obs, [], [], sim, [], [], [])  

f_PlotInput([], obs(5879:6057), [], [], sim(5879:6057), [], [], [])       
f_PlotConnectedSeries_seg(obs, segs_obs_opt_all, sim, segs_sim_opt_all, connectors2, [5879:6057])     
f_PlotSDErrors_OnePanel(Qres.et(6038:6042), Qres.eq(6038:6042))

xline([5886 5891 5918 5926 5940 5984 5987 6011 6034 6038 6042],'--b') 




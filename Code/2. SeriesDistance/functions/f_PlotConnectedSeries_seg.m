function [ ] = f_PlotConnectedSeries_seq(obs,segs_obs,sim,segs_sim,connectors,ts)
% plots two related time series (obs and sim) and their series-distance connectors
% 19.Nov.2013 Uwe Ehret

if ~exist('showEventIndex', 'var'); % adds event numbers to the plot
    showEventIndex = false;
end
if isempty(connectors)
    show_connectors = false;
else
    show_connectors = true;
end

if length(ts) == 0
    ts = 1:1:size(obs,1);
elseif ts(end) > size(obs,1)
    ts = ts(1):1:size(obs,1);
end

%distance vectors between matching points obs/sim
ts_obs_connectors = [find(connectors.x_match_obs_global > ts(1), 1, "first"):find(connectors.x_match_obs_global < ts(end), 1, "last")];
ts_sim_connectors = [find(connectors.x_match_sim_global > ts(1), 1, "first"):find(connectors.x_match_sim_global < ts(end), 1, "last")];
ts_connectors = [max(ts_obs_connectors(1), ts_sim_connectors(1)):min(ts_obs_connectors(end), ts_sim_connectors(end))];
if show_connectors == true 
    u = connectors.x_match_sim_global(ts_connectors) - connectors.x_match_obs_global(ts_connectors);
    v = connectors.y_match_sim(ts_connectors) - connectors.y_match_obs(ts_connectors);
end

ff = figure();
hold on;

% Plot the timeseries
plot(ts,obs(ts),'Color', [0.2 0.2 0.2]);
plot(ts,sim(ts),'--','Color', [0.2 0.2 0.2]);

% Plot Feature Distance lines
%distance vectors between matching points obs/sim


if show_connectors == true 
   quiver(connectors.x_match_obs_global(ts_connectors), ...
       connectors.y_match_obs(ts_connectors), ...
       u,v,0,'k','ShowArrowHead','off','Color', [0.6 0.6 0.6]);   
end

% Plot the connected segments in unnique color
num_segs = length(segs_obs);
%cmap=[1 0 0;0 0.4 1;0 1 0;0.8 0 1;1 0.8 0.5]; % colormap
cmap=['#EFCC6B';'#A9D35A';'#BE93C6';'#979EC1']; % colormap

cmap_count = 1;
for z = 1: num_segs
    if segs_obs(z).starttime_global > ts(end) | segs_obs(z).endtime_global < ts(1)
        continue
    end
    xes_global = max(segs_obs(z).starttime_global, ts(1)) : min(segs_obs(z).endtime_global, ts(end));
    yes_global = obs(xes_global);
    plot(xes_global,yes_global,'-','Color',cmap(cmap_count,:),'LineWidth',2);

    if segs_sim(z).starttime_global > ts(end) | segs_sim(z).endtime_global < ts(1)
        continue
    end
    xes_global = max(segs_sim(z).starttime_global, ts(1)) : min(segs_sim(z).endtime_global, ts(end));
    yes_global = sim(xes_global);
    plot(xes_global,yes_global,'--','Color',cmap(cmap_count,:),'LineWidth',2); 

    cmap_count = cmap_count + 1;
    if cmap_count > size(cmap,1)
        cmap_count = 1;
    end
end

% Formatting
    if show_connectors == false
        legend('observation','simulation','Location','NorthEast');
        legend('boxoff');
    else
        legend('observation','simulation','connectors','Location','NorthEast');
        legend('boxoff');
    end    
ax1 = gca;
set(ax1,'Fontsize',11,'FontWeight','bold')    

% add event index to plot
if showEventIndex==true
    event_ind = [1 find(diff([segs_obs(:).eventID])==1)+1]; % row index
    for kk = 1:1:length(event_ind)
        text([segs_obs(event_ind(kk)).starttime_global], 0, strcat('# ', num2str(kk)))
    end
end
xline([5886 5891 5918 5926 5940 5984 5987 6011 6034 6038 6042],'--b') 
%box on;
ylabel("Steamflow [mm/s]");
xlim([ts(1) ts(end) + 1]);
%ylim([0 max(obs(ts), sim(ts)) + 1]);
hold off


end


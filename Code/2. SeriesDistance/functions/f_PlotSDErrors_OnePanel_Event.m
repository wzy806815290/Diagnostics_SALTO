function [ ] = f_PlotSDErrors_OnePanel_Event(t_errors, q_errors, hydcase, CI)
% Plots 2-d error distributions
    ff = figure('units','centimeters','position',[1,1,8,8]);
    set(gcf, 'PaperSize', [8 8]);
    pos = get(gca,'Position');
    ti = get(gca,'TightInset');
    alpha = 0.2;
    size = 10;
    line_wth = 1.5;
    colormap = [0.4940 0.1840 0.5560; 0 0.4470 0.7410; 0.8500 0.3250 0.0980; 0.9290 0.6940 0.1250];
    if isempty(CI)
        CI = 0;
    end

    hold on;    
    % magnitude errors
        % plot the error distribution for valleys
        scatter1 = scatter(t_errors(hydcase == -2), q_errors(hydcase == -2), size);
        set(scatter1,'MarkerFaceColor',colormap(1, :),'MarkerEdgeColor',colormap(1, :),'Marker','o','MarkerFaceAlpha',alpha,'MarkerEdgeAlpha',alpha);
        CE = f_confidence_ellipse(t_errors(hydcase == -2), q_errors(hydcase == -2), CI);
        set(CE,'LineWidth',line_wth,'Color',colormap(1, :));
        % plot the error distribution for drops
        scatter2 = scatter(t_errors(hydcase == -1), q_errors(hydcase == -1), size);
        set(scatter2,'MarkerFaceColor',colormap(2, :),'MarkerEdgeColor',colormap(2, :),'Marker','o','MarkerFaceAlpha',alpha,'MarkerEdgeAlpha',alpha);
        CE = f_confidence_ellipse(t_errors(hydcase == -1), q_errors(hydcase == -1), CI);
        set(CE,'LineWidth',line_wth,'Color',colormap(2, :));
        % plot the error distribution for rises
        scatter3 = scatter(t_errors(hydcase == 1), q_errors(hydcase == 1), size);
        set(scatter3,'MarkerFaceColor',colormap(3, :),'MarkerEdgeColor',colormap(3, :),'Marker','o','MarkerFaceAlpha',alpha,'MarkerEdgeAlpha',alpha);
        CE = f_confidence_ellipse(t_errors(hydcase == 1), q_errors(hydcase == 1), CI);
        set(CE,'LineWidth',line_wth,'Color',colormap(3, :));
        % plot the error distribution for peaks
        scatter4 = scatter(t_errors(hydcase == 2), q_errors(hydcase == 2), size);
        set(scatter4,'MarkerFaceColor',colormap(4, :),'MarkerEdgeColor',colormap(4, :),'Marker','o','MarkerFaceAlpha',alpha,'MarkerEdgeAlpha',alpha);
        CE = f_confidence_ellipse(t_errors(hydcase == 2), q_errors(hydcase == 2), CI);
        set(CE,'LineWidth',line_wth,'Color',colormap(4, :));         
        % set x- and ylim, but only if there is at least one non-NaN value in the plot
        if any(~isnan(t_errors));
            ylim([nanmax(nanmax(q_errors), abs(nanmin(q_errors)))*-1.1, nanmax(nanmax(q_errors), abs(nanmin(q_errors)))*1.1]);
            if(all(q_errors==0))
                xlim([-0.1,0.1])
            else
                xlim([nanmax(nanmax(t_errors), abs(nanmin(t_errors)))*-1.1, nanmax(nanmax(t_errors), abs(nanmin(t_errors)))*1.1]);    
            end
        end
        
        % add vertical and horizontal lines
        line(xlim, [0 0], 'LineStyle', '-.', 'LineWidth',1, 'Color',[0.65 0.65 0.65]);
        line([0 0],ylim, 'LineStyle', '-.', 'LineWidth',1, 'Color',[0.65 0.65 0.65]);
        
        % plot the mean (center) of the error distribution
        plot(nanmean(t_errors),nanmean(q_errors),'MarkerFaceColor','none','MarkerEdgeColor','k','MarkerSize',10,'Marker','+','LineStyle','none','linewidth',2);
        % plot the mean (center) of the error distribution for valleys
        plot(nanmean(t_errors(hydcase == -2)),nanmean(q_errors(hydcase == -2)),'MarkerFaceColor','none','MarkerEdgeColor',[0.4940 0.1840 0.5560],'MarkerSize',10,'Marker','+','LineStyle','none','linewidth',2);
        % plot the mean (center) of the error distribution for drops
        plot(nanmean(t_errors(hydcase == -1)),nanmean(q_errors(hydcase == -1)),'MarkerFaceColor','none','MarkerEdgeColor',[0 0.4470 0.7410],'MarkerSize',10,'Marker','+','LineStyle','none','linewidth',2);
        % plot the mean (center) of the error distribution for rises
        plot(nanmean(t_errors(hydcase == 1)),nanmean(q_errors(hydcase == 1)),'MarkerFaceColor','none','MarkerEdgeColor',[0.8500 0.3250 0.0980],'MarkerSize',10,'Marker','+','LineStyle','none','linewidth',2);
        % plot the mean (center) of the error distribution for peaks
        plot(nanmean(t_errors(hydcase == 2)),nanmean(q_errors(hydcase == 2)),'MarkerFaceColor','none','MarkerEdgeColor',[0.9290 0.6940 0.1250],'MarkerSize',10,'Marker','+','LineStyle','none','linewidth',2);

        % add labels
        xlabel('timing error (>0: obs later sim)');
        ylabel('magnitude error (>0: obs larger sim)');
        %titlestr = strcat('2d error distribution for rising segments, Q range:', {' '}, num2str(error_lvls(i)), {' '},'<= Qsim <', {' '},num2str(error_lvls(i+1))); 
        title('entire time series');

        % format the figure
        axis square;
        box on;
        hold off;
        legend([scatter1 scatter2 scatter3 scatter4],{'valleys','drops','rises','peaks'});
        legend('boxoff');
end

function CE = f_confidence_ellipse(x, y, p) 
    % Draw the confidence ellipse
    % Parameters:
    % x, y: inputs
    % p: confidence interval

    % the length of x and y should be same
    if length(x) ~= length(y)
        error('Length of x and y should be same');
    end
    
    covariance = cov([x, y]);
    [eigenvec,eigenval] = eig(covariance);

    [sortEigenval,index] = sort(diag(eigenval),'descend');
    sortEigenvec = eigenvec(:,index);

    largestEigenval = sortEigenval(1);
    smallestEigenval = sortEigenval(end); %find the minimum eigenvalue
    largestEigenvec = sortEigenvec(:,1); %find the maximum eigenvector

    angle = atan2(largestEigenvec(2), largestEigenvec(1)); %calculate the angle between x-axis and the maximum eigenvector, [-pi,pi]

    if(angle < 0) 
        angle = angle + 2*pi;
    end

    %configure the parameters of the confidence ellipse
    z = -norminv((1 - p) / 2); % standard normal z score
    thetaGrid = linspace(0, 2 * pi); 
    phi = angle; % rotation angle
    X0 = nanmean(x);
    Y0 = nanmean(y); 
    a = z * sqrt(largestEigenval); %the wheelbase length
    b = z * sqrt(smallestEigenval);
    
    ellipseXR = a * cos(thetaGrid); %onto rectangular axis
    ellipseYR = b * sin(thetaGrid);
    
    R = [cos(phi) sin(phi); -sin(phi) cos(phi)]; %rotation matrix
    
    rEllipse = [ellipseXR; ellipseYR]' * R; %rotation
    
    CE = plot(rEllipse(:,1) + X0, rEllipse(:,2) + Y0, '-'); %print
end
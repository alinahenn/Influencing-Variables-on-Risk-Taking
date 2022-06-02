%Alina Theresa Henn
    %skript_BART_Henn_Part_1
    % Date: 10/16/2020

%Explanation to part 1a and part 1b:
	%Part 1a and part 1b exist for the reason that the log file of the BART was changed again after 10 participants in such a way that the earned amounts
    %and the current account balance were not only rounded to four decimal places but also listed unrounded.

%Wiki:
    % 1. The path for L- and G-beard has to be adjusted depending on the data you want to analyze (line xy)
    % 2. Activate the S-numbers belonging to the respective BART version and decativate those of the other version (line xy)
    % 3. Attention the document Resp will be overwritten if the second analysis is calculated with the other version of BART. It must first be renamed.
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________

%%PART 1a.
        %Skript for Participants No:'S01','S05','S06','S07','S09','S42','S43','S45','S47','S48','S49','S50','S51','S53','S54','S55','S56','S57','S58','S59','S60','S61','S62','S63','S65'.
        
        %L-BART:
        %root = 'W:\Fmri_Forschung\Allerlei\Alina H\Pilot_Entscheidungsfindung\LOG BART\Daten_Analyse\LOG_L-BART_Verhalten';
        %sub = {'S01','S05','S06','S07','S09','S42','S43','S45','S47','S48','S49','S50','S51','S52','S53','S54','S55','S56','S57','S58','S59','S60','S61','S62','S63','S65','S44'}
               
        %G-BART:
         root = 'W:\Fmri_Forschung\Allerlei\Alina H\Pilot_Entscheidungsfindung\LOG BART\Daten_Analyse\LOG_G-BART_Verhalten';     
         %sub = {'S01','S05','S06','S07','S09','S42','S43','S45','S47','S48','S49','S50','S51','S53','S54','S55','S56','S57','S58','S59','S60','S61','S62','S63','S65','S44'}   %S052 missing for G-BART
           
         for i= 1:length(sub)
        %% **** Präsentation logfile auslesen und Arrays definieren **** %%
            subResponse = strcat(sub{i}, '.txt');
            fid = fopen(fullfile(root,subResponse));
            ResponseData= textscan(fid, '%n %n %s %s %n %n %n %n %n %n %n %n', 'Headerlines', 5); %%n= nummerisch, %s=string; def. der Spalten
            Risk = ResponseData {1,3};
            OM = ResponseData {1,4};%outcome magnitude (Mode G in Log file)
            Onset = ResponseData {1,5};
            RT= ResponseData {1,6};
                RTnew = RT;
                RTnew (RTnew == 0) = NaN; %% neue Variabel für RT: bei RT-Werten von Null hat der Proband nicht reagiert (daher als missing kodieren) und wird sonst als 0 mit in die Berechnung des means eingerechnet, was den mean verändert
            actualRisk= ResponseData {1,8};
            actualReward= ResponseData {1,9}; %Gewinn für einzelnen Trial "Gewinn"
            Outcome = ResponseData {1,10}; %0=verloren, 1=gewonnen; dh "Erg." 
            %Sum = ResponseData {1,11}; %aufsummierter Gewinn "Summe"
            Random = ResponseData {1,12}; %es  wird random (mehr oder weniger?) eine Zahl zwischen 0-1 gezogen (in Abhängigkeit der Größe des Ballons); wenn "Random" < "Risk" = Platzen. (nochmal checken!)  
        
        % recoding Strings Risk zu nummerisch
            strRisk = strrep(Risk, 'RISK_L', '0');
            SRisk = sprintf('%s*', strRisk{:});
            Risk = sscanf(SRisk, '%f*');
        
        % recoding Strings Reward zu nummerisch
            strOM = strrep(OM, 'GELD_L', '0');
            strOM = strrep(strOM, 'GELD_H', '1');
            strOM = sprintf('%s*', strOM{:});
            OM = sscanf(strOM, '%f*');
        
        % mean RT
            RT_L = RTnew(Risk == 0 & Reward == 0);
            RT_H = RTnew(Risk == 0 & Reward == 1);
        
        % %mean outcome
            Outcome_L = Outcome (Risk ==0 & Reward ==0);
            Outcome_H = Outcome (Risk ==0 & Reward ==1);
        
        % %mean earnings
            Earn_L = actualReward (Risk ==0 & Reward ==0);
            Earn_H = actualReward (Risk ==0 & Reward ==1);
        
        Resp(i,1) = nanmean(RT_L); %d.h. NaN Werte werden nicht beim Mittewert mit einbezogen. 
        Resp(i,2) = nanmean(RT_H);   
        Resp(i,3) = nanmean(Earn_L);
        Resp(i,4) = nanmean(Earn_H);
        Resp(i,5) = nanmean(Outcome_L);
        Resp(i,6) = nanmean(Outcome_H);
        Resp(i,7)= nansum (actualReward);
               
           save('Resp'); 
        end
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________

%%PART 1b.
       %Skript for Participants No:'S02','S03','S10','S46','S64','S04','S08','S11','S12','S13','S14','S16','S17','S19','S20','S21','S22','S24','S25','S26','S18','S34','S31','S33','S27','S28','S29','S35','S36','S37','S38','S41','S66','S67', 'S68','S71','S69','S40','S70','S39','S32','S43.1','S72','S52'.
        
        %Set path.
        %Part 2
        
        %L-BART:
         %root = 'G:\Promotion Alina Henn\Pilot_Entscheidungsfindung_ATH\LOG Daten + Paradigma BART\LOG BART\Daten_Analyse\LOG_L-BART_Verhalten';
         %sub = {'S02','S03','S10','S46','S64','S04','S08','S11','S12','S13','S14','S16','S17','S19','S20','S21','S22','S24','S25','S26','S18','S34','S31','S33','S27','S28','S29','S35','S36','S37','S38','S41','S66','S67', 'S68','S71','S69','S40','S70','S39','S32','S43.1','S72','S52'}
           
        %G-BART:
         root = 'G:\Promotion Alina Henn\Pilot_Entscheidungsfindung_ATH\LOG Daten + Paradigma BART\LOG BART\Daten_Analyse\LOG_G-BART_Verhalten';
         sub = {'S02','S03','S10','S46','S64','S04','S08','S11','S12','S13','S14','S16','S17','S19','S20','S21','S22','S24','S25','S26','S18','S34','S31','S33','S27','S28','S29','S35','S36','S37','S38','S41', 'S52','S66','S67', 'S68', 'S71','S69','S40','S70','S39','S32','S43.1','S72'}   
         
        for i= 1:length(sub)
        %% **** Präsentation logfile auslesen und Arrays definieren **** %%
            subResponse = strcat(sub{i}, '.txt');
            fid = fopen(fullfile(root,subResponse));
            ResponseData= textscan(fid, '%n %n %s %s %n %n %n %n %n %n %n %n %n %n', 'Headerlines', 5); %%n= nummerisch, %s=string; def. der Spalten
            Risk = ResponseData {1,3};
            Reward = ResponseData {1,4};
            Onset = ResponseData {1,5};
            RT= ResponseData {1,6};
                RTnew = RT;
                RTnew (RTnew == 0) = NaN; %% neue Variabel für RT: bei RT-Werten von Null hat der Proband nicht reagiert (daher als missing kodieren) und wird sonst als 0 mit in die Berechnung des means eingerechnet, was den mean verändert
            actualRisk= ResponseData {1,8};
            actualReward_r= ResponseData {1,9}; %Gewinn für einzelnen Trial "Gewinn_gerundet", rounded
            Outcome = ResponseData {1,10}; %0=verloren, 1=gewonnen; dh "Erg." 
            %Sum_nr = ResponseData {1,11}; %aufsummierter Gewinn "Summe_ungerundet";not rounded
            Random = ResponseData {1,12}; %es  wird random (mehr oder weniger?) eine Zahl zwischen 0-1 gezogen (in Abhängigkeit der Größe des Ballons); wenn "Random" < "Risk" = Platzen. (nochmal checken!)  
            %Sum_r = ResponseData {1,13}; %aufsummierter Gewinn "Summe_gerundet"; rounded
            %actualReward_nr= ResponseData {1,14}; %Gewinn für einzelnen Trial "Gewinn_ungerundet",not rounded
        
        % recoding Strings Risk zu nummerisch
            strRisk = strrep(Risk, 'RISK_L', '0');
            SRisk = sprintf('%s*', strRisk{:});
            Risk = sscanf(SRisk, '%f*');
        
        % recoding Strings Reward zu nummerisch
            strReward = strrep(Reward, 'GELD_L', '0');
            strReward = strrep(strReward, 'GELD_H', '1');
            SReward = sprintf('%s*', strReward{:});
            Reward = sscanf(SReward, '%f*');
        
        % mean RT
            RT_L = RTnew(Risk == 0 & Reward == 0);
            RT_H = RTnew(Risk == 0 & Reward == 1);
        
        % %mean outcome
            Outcome_L = Outcome (Risk ==0 & Reward ==0);
            Outcome_H = Outcome (Risk ==0 & Reward ==1);
        
        % %mean earnings
            Earn_L = actualReward_r (Risk ==0 & Reward ==0);
            Earn_H = actualReward_r (Risk ==0 & Reward ==1);
        
        Resp(i,1) = nanmean(RT_L); %d.h. NaN Werte werden nicht beim Mittewert mit einbezogen. 
        Resp(i,2) = nanmean(RT_H);   
        Resp(i,3) = nanmean(Earn_L);
        Resp(i,4) = nanmean(Earn_H);
        Resp(i,5) = nanmean(Outcome_L);
        Resp(i,6) = nanmean(Outcome_H);
        Resp(i,7)= nansum (actualReward_r);
        
         save('Resp'); 
        end
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________

%Alina Theresa Henn
%skript_BART_TrialByTrail
%Date: 07/01/2021

%Explanation to part 2a and part 2b and part 2c:
    %Part 2a and part 2b exist for the reason that the log file of the BART was changed again after 10 participants in such a way that the earned amounts
    %and the current account balance were not only rounded to four decimal places but also listed unrounded.
    % Within participant S52 the trial 60 is missing.

%Wiki:
    % 1. The path for L- and G-beard has to be adjusted depending on the data you want to analyze (line xy)
    % 2. Activate the S-numbers belonging to the respective BART version and decativate those of the other version (line xy)
    % 3. Attention the document Resp will be overwritten if the second analysis is calculated with the other version of BART. It must first be renamed.
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________
    

    %%PART 2a.
           %Skript for Participants No: 'S01','S05','S06','S07','S09','S42','S43','S45','S47','S48','S49','S50','S51','S53','S54','S55','S56','S57','S58','S59','S60','S61','S62','S63','S65'
        
        %Set path.
        
        %L-BART:
            %root = 'D:\Promotion Alina Henn\Pilot_Entscheidungsfindung_ATH\LOG Daten + Paradigma BART\LOG BART\Daten_Analyse\LOG_L-BART_Verhalten';
            %sub ={'S01','S05','S06','S07','S09','S42','S43','S44','S45','S47','S48','S49','S50','S51','S53','S54','S55','S56','S57','S58','S60','S61','S62','S63','S65','S52'};
        
        
        %G-BART:
            root = 'D:\Promotion Alina Henn\Pilot_Entscheidungsfindung_ATH\LOG Daten + Paradigma BART\LOG BART\Daten_Analyse\LOG_G-BART_Verhalten';
            sub = {'S01','S05','S06','S07','S09','S42','S43','S44','S45','S47','S48','S49','S50','S51','S53','S54','S55','S56','S57','S58','S60','S61','S62','S63','S65'}; %VP 52 missing for G-BART
        
        for i= 1:length(sub)
        subResponse = strcat(sub{i}, '.txt');
              fid = fopen(fullfile(root,subResponse));
               ResponseData= textscan(fid, '%n %n %s %s %n %n %n %n %n %n %n %n', 'Headerlines', 5); %n= nummerisch, %s=string; def. der Spalten
                    
                    orgReward = ResponseData {:,4};
                            %recoding Strings Reward zu nummerisch:
                            strReward = strrep(orgReward, 'GELD_L', '0');
                            strReward = strrep(strReward, 'GELD_H', '1');
                            SReward = sprintf('%s*', strReward{:});
                            Reward = sscanf(SReward, '%f*');
        
                    Onset = ResponseData {:,5};
                    RT = ResponseData {:,6};
                            RTnew = RT;
                            RTnew (RTnew == 0) = NaN;%% neue Variabel für RT: bei RT-Werten von Null hat der Proband nicht reagiert (daher als missing kodieren) und wird sonst als 0 mit in die Berechnung des means eingerechnet, was den mean verändert
                
                    Gewinn= ResponseData {:,9};%Gewinn für einzelnen Trial "Gewinn"
                    Outcome = ResponseData {:,10}; %0=verloren, 1=gewonnen; dh "Erg." 
                    Random = ResponseData {:,12}; %es wird random (mehr oder weniger?) eine Zahl zwischen 0-1 gezogen (in Abhängigkeit der Größe des Ballons); wenn "Random" < "Risk" = Platzen. (nochmal checken!)  
                    Summe = ResponseData {:,11}; %Einzelener Gewinn /Verlust pro Runde; "Summe"; ACHTUNG: unterschiedliche Zeile in Part1 und Part2
              
                    Reaktionszeit(1:60,i) = RTnew(1:60,1);
                    OM(1:60,i) = Reward(1:60,1); % outcome magnitude
                    Gewinn(1:60,i) = Gewinn(1:60,1);
                    Ergebnis(1:60,i) = Outcome(1:60,1);%gewonnen oder verloren
                    Zeit(1:60,i) = Onset(1:60,1); 
                    Summe (1:60,i) = Summe(1:60,1);
                
        end
        
        D(:,1) = reshape(Reaktionszeit,[],1);
        D(:,2) = reshape(OM,[],1);
        D(:,3) = reshape(Gewinn,[],1);
        D(:,4) = reshape(Ergebnis,[],1);
        D(:,5) = reshape(Zeit,[],1);
        D(:,6) = reshape(Summe,[],1);
        
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________

        %%PART 2b.
           %Skript for Participants No:'S02','S03','S10','S46','S64,'S04','S08','S11','S12','S13','S14','S16','S17','S19','S20','S21','S22','S24','S25','S26',
           %'S18','S34','S31','S33''S27','S28','S29','S35','S36','S37','S38','S41','S66','S67', 'S68'
       
        %Set path.
       
        %L-BART:
            root = 'D:\Promotion Alina Henn\Pilot_Entscheidungsfindung_ATH\LOG Daten + Paradigma BART\LOG BART\Daten_Analyse\LOG_L-BART_Verhalten';
            sub ={'S02','S03','S10','S46','S64','S04','S08','S11','S12','S13','S14','S16','S17','S19','S20','S21','S22','S24','S25','S26','S18','S31','S33','S27','S28','S29','S35','S36','S37','S38','S41','S66','S67', 'S68', 'S71','S69','S40','S70','S39','S32','S43.1', 'S72'}; %S34 von Analyse ausgeschlossen
        
        %G-BART:
             %root = 'D:\Promotion Alina Henn\Pilot_Entscheidungsfindung_ATH\LOG Daten + Paradigma BART\LOG BART\Daten_Analyse\LOG_G-BART_Verhalten';
           %sub = {'S02','S03','S10','S46','S64','S04','S08','S11','S12','S13','S14','S16','S17','S19','S20','S21','S22','S24','S25','S26','S18','S31','S33','S27','S28','S29','S35','S36','S37','S38','S41','S66','S67', 'S68', 'S71','S69','S40','S70','S39','S32','S43.1', 'S72'}; %S334 von Analyse ausgeschlossen
        
        for i= 1:length(sub)
        subResponse = strcat(sub{i}, '.txt');
              fid = fopen(fullfile(root,subResponse));
               ResponseData= textscan(fid, '%n %n %s %s %n %n %n %n %n %n %n %n %n %n', 'Headerlines', 5);%n= nummerisch, %s=string; def. der Spalten
                    
                    orgReward = ResponseData {:,4};
                            %recoding Strings Reward zu nummerisch:
                            strReward = strrep(orgReward, 'GELD_L', '0');
                            strReward = strrep(strReward, 'GELD_H', '1');
                            SReward = sprintf('%s*', strReward{:});
                            Reward = sscanf(SReward, '%f*');
        
                    Onset = ResponseData {:,5};
                    RT = ResponseData {:,6};
                            RTnew = RT;
                            RTnew (RTnew == 0) = NaN;%% neue Variabel für RT: bei RT-Werten von Null hat der Proband nicht reagiert (daher als missing kodieren) und wird sonst als 0 mit in die Berechnung des means eingerechnet, was den mean verändert
                
                    Gewinn= ResponseData {:,9};%Gewinn für einzelnen Trial "Gewinn"
                    Outcome = ResponseData {:,10}; %0=verloren, 1=gewonnen; dh "Erg." 
                    Random = ResponseData {:,12}; %es  wird random (mehr oder weniger?) eine Zahl zwischen 0-1 gezogen (in Abhängigkeit der Größe des Ballons); wenn "Random" < "Risk" = Platzen. (nochmal checken!)  
                    Summe = ResponseData {:,13}; %Einzelener Gewinn /Verlustbetrag pro Runde; "Summe_gerundet"; ACHTUNG: Unterschiedlich in Part1 und Part2
              
                    %Speicherung der Logfiles nebeneinander. Dh für jede Person werden alle Trials gespeichert.    
                    Reaktionszeit(1:60,i) = RTnew(1:60,1);
                    OM(1:60,i) = Reward(1:60,1);
                    Gewinn(1:60,i) = Gewinn(1:60,1);
                    Ergebnis(1:60,i) = Outcome(1:60,1);
                    Zeit(1:60,i) = Onset(1:60,1);
                    Summe (1:60,i) = Summe(1:60,1);
           
        end
        
        sub =sub'; %Zeilenvektor zu Spaltenvektor umkodieren
        ID = sub (1:1,1);
        rep=repmat(ID,60);
        
        %Reshape:die einzelnen Daten werden im Anschluss nun untereinander
        %gelistet. 
        %D(:,1) = reshape(ID,[],1);
        
        D(:,1) = reshape(Reaktionszeit,[],1);
        D(:,2) = reshape(OM,[],1);
        D(:,3) = reshape(Gewinn,[],1);
        D(:,4) = reshape(Ergebnis,[],1);
        D(:,5) = reshape(Zeit,[],1);
        D(:,6) = reshape(Summe,[],1);
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________
%___________________________________________________________________________________________________________________

%PART 2c
%**************************** für VP 59 ***********************************
% VP 59 has only 59 trials and not 60; therefore a separate calculation;last row is declaired with NA. 
        
        %L-BART:
            root = 'D:\Promotion Alina Henn\Pilot_Entscheidungsfindung_ATH\LOG Daten + Paradigma BART\LOG BART\Daten_Analyse\LOG_L-BART_Verhalten';
            sub = {'S59'};
        
        %G-BART:
            %root = 'D:\Promotion Alina Henn\Pilot_Entscheidungsfindung_ATH\LOG Daten + Paradigma BART\LOG BART\Daten_Analyse\LOG_G-BART_Verhalten';
            %sub = {'S59'}; 
        
        for i= 1:length(sub)
        subResponse = strcat(sub{i}, '.txt');
              fid = fopen(fullfile(root,subResponse));
               ResponseData= textscan(fid, '%n %n %s %s %n %n %n %n %n %n %n %n', 'Headerlines', 5); %n= nummerisch, %s=string; def. der Spalten
                    
                    orgReward = ResponseData {:,4};
                            %recoding Strings Reward zu nummerisch:
                            strReward = strrep(orgReward, 'GELD_L', '0');
                            strReward = strrep(strReward, 'GELD_H', '1');
                            SReward = sprintf('%s*', strReward{:});
                            Reward = sscanf(SReward, '%f*');
        
                    Onset = ResponseData {:,5};
                    RT = ResponseData {:,6};
                            RTnew = RT;
                            RTnew (RTnew == 0) = NaN;%% neue Variabel für RT: bei RT-Werten von Null hat der Proband nicht reagiert (daher als missing kodieren) und wird sonst als 0 mit in die Berechnung des means eingerechnet, was den mean verändert
                
                    Gewinn= ResponseData {:,9};%Gewinn für einzelnen Trial "Gewinn"
                    Outcome = ResponseData {:,10}; %0=verloren, 1=gewonnen; dh "Erg." 
                    Random = ResponseData {:,12}; %es wird random (mehr oder weniger?) eine Zahl zwischen 0-1 gezogen (in Abhängigkeit der Größe des Ballons); wenn "Random" < "Risk" = Platzen. (nochmal checken!)  
                    Summe = ResponseData {:,11}; %Einzelener Gewinn /Verlust pro Runde; "Summe"; ACHTUNG: unterschiedliche Zeile in Part1 und Part2
              
                %für G-BART
        %         Reaktionszeit(1:59,i) = RTnew(1:59,1);
        %         OM(1:59,i) = Reward(1:59,1); % outcome magnitude
        %         Gewinn(1:59,i) = Gewinn(1:59,1);
        %         Ergebnis(1:59,i) = Outcome(1:59,1);%gewonnen oder verloren
        %         Zeit(1:59,i) = Onset(1:59,1); 
        %         Summe (1:59,i) = Summe(1:59,1);
        
                 %für L-BART
                   Reaktionszeit(1:60,i) = RTnew(1:60,1);
                   OM(1:60,i) = Reward(1:60,1); % outcome magnitude
                   Gewinn(1:60,i) = Gewinn(1:60,1);
                   Ergebnis(1:60,i) = Outcome(1:60,1);%gewonnen oder verloren
                   Zeit(1:60,i) = Onset(1:60,1); 
                   Summe (1:60,i) = Summe(1:60,1);;         
         end
       
        D(:,1) = reshape(Reaktionszeit,[],1);
        D(:,2) = reshape(OM,[],1);
        D(:,3) = reshape(Gewinn,[],1);
        D(:,4) = reshape(Ergebnis,[],1);
        D(:,5) = reshape(Zeit,[],1);
        D(:,6) = reshape(Summe,[],1);
        
        
                   


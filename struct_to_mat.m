% read data for saga
alldat=[];
cd('/home/gregor/saga/Controls')
tmp1 = dir('K**_cfs*');
cd('/home/gregor/saga/Syn')
tmp2 = dir('S**_cfs*');
 
allfolder = {tmp1.folder, tmp2.folder};
allnames  = {tmp1.name, tmp2.name};
clear ergmat tmpmat groupcode

for j = 1:size(allnames, 2)
	load([allfolder{j}, '/', allnames{j}]);
	ergmat = CFS_Synesthesia.data;
  groupcode = j <= 18;
	tmpmat = [repmat(j,size(ergmat,1),1), repmat(groupcode,size(ergmat,1),1), ergmat];
  alldat = [alldat; tmpmat]; clear ergmat tmpmat groupcode
end


cd('/home/gregor/saga/')
save('-mat7-binary', 'alldat.mat', 'alldat');



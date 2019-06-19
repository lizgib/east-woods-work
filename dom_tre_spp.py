# I can't seem to get this to work in R so we're gonna do it here in python as a check

dat_all = open('data/species/dat.all.csv', errors = 'ignore')

header = dat_all.readline()
lines = dat_all.readlines()


trees07 = {}
trees18 = {}
for l in lines:
    ll = l.strip().split(',')
    if ll[5] == 'T':
        if ll[6] == '2007':
            if ll[0] not in trees07.keys():
                trees07[ll[0]] = {}
                if ll[7] not in trees07[ll[0]].keys():
                    trees07[ll[0]].update({ll[7]:0})
        if ll[6] == '2018':
            if ll[0] not in trees18.keys():
                trees18[ll[0]] = {}
                if ll[7] not in trees18[ll[0]].keys():
                    trees18[ll[0]].update({ll[7]:0})
print(trees07['HB138'])

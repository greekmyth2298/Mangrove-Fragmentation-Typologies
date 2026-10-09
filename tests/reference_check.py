"""Independent stdlib-Python source-data audit. Optional; no R required.
Does not substitute for running the R scripts in RStudio.
"""
from pathlib import Path
from collections import Counter, defaultdict
import csv, math, hashlib, sys
BASE=Path(__file__).resolve().parents[1]
S={str(i):v for i,v in enumerate(['Nibbling','Clearing','Shattering','Displacing','Expanding'])}
def read(path):
 with (BASE/path).open(encoding='utf-8-sig',newline='') as f: return list(csv.DictReader(f))
def city(x):
 s=x.strip().lower().replace(' ','')
 return {'vungtau':'Vungtau','krungthep':'Bangkok','bangkok':'Bangkok','chonburi':'Chon Buri'}.get(s,x.strip())
def H(rows,xnames,y='S_2015_2020', weights=None):
 data=defaultdict(Counter)
 for i,r in enumerate(rows): data[tuple(r[k] for k in xnames)][r[y]]+=1 if weights is None else weights[i]
 total=sum(sum(v.values()) for v in data.values())
 return -sum(n/total*sum(p/n*math.log2(p/n) for p in v.values() if p) for v in data.values() for n in [sum(v.values())])
def stat(rows,weights=None):
 h1=H(rows,['S_2007_2015'],weights=weights)
 h2=H(rows,['S_1996_2007','S_2007_2015'],weights=weights)
 delta=h1-h2
 return h1,h2,delta,2*(len(rows) if weights is None else sum(weights))*math.log(2)*delta
assert (BASE/'data/original/ALL CITIES COMPILED.csv').read_bytes() == (BASE/'data/processed/RECONSTRUCTED_TRAJECTORIES.csv').read_bytes()
assert (BASE/'reference/historical_code/PATH DEPENDENCE WITH URBAN AREA SUMMARY.R').read_bytes() == (BASE/'reference/historical_code/path_dependence_original_recovered.txt').read_bytes()
print('PASS Original ALL CITIES COMPILED.csv and historical R source archived byte-for-byte')
train=read('data/raw/TRAINING_DATA.csv'); ext=read('data/processed/EXTERNAL_VALIDATION.csv')
full=read('data/processed/PATCH_TRANSITIONS_1996_2020.csv'); r=read('data/processed/RECONSTRUCTED_TRAJECTORIES.csv')
assert (len(train),len(ext),len(full),len(r))==(1212,500,4584,3186)
manifest=read('data/interval_predictions/manifest.csv')
assert len(manifest)==30
first=Counter(); all_count=0
for m in manifest:
 f=BASE/m['file']; assert f.is_file()
 assert hashlib.sha256(f.read_bytes()).hexdigest()==m['sha256']
 rows=read(m['file']); assert len(rows)==int(m['expected_rows'])
 all_count+=len(rows)
 first_y=m['interval'].split('_')[0]
 second_y=m['interval'].split('_')[1]
 idp=[x for x in rows[0] if x.lower() in ('id_'+first_y,'patch_id_'+first_y)]
 idc=[x for x in rows[0] if x.lower() in ('id_'+second_y,'patch_id_'+second_y)]
 assert len(idp)==len(idc)==1
 for z in rows:
  assert z['Predicted_Typology'] in S
  ps=[float(z[f'Prob_{i}']) for i in range(5)]
  assert abs(sum(ps)-1)<.015
  if m['interval']=='1996_2007':
   first[(city(m['urban_area']),z[idp[0]],S[z['Predicted_Typology']])]+=1
assert all_count==8909
rec=Counter((city(z['City']),z['City_ID_1996'],z['S_1996_2007']) for z in r)
assert first==rec
assert sum(first.values())==3186
for a in r:
 assert not (a['S_1996_2007']=='Lost' and a['S_2007_2015']!='Lost')
 assert not (a['S_2007_2015']=='Lost' and a['S_2015_2020']!='Lost')
path=Counter(' -> '.join([z['S_1996_2007'],z['S_2007_2015'],z['S_2015_2020']]) for z in r)
ref3={'Shattering -> Lost -> Lost':632,'Clearing -> Lost -> Lost':101,
'Displacing -> Stabilizing -> Displacing':96,'Shattering -> Shattering -> Lost':72,
'Displacing -> Lost -> Lost':65,'Shattering -> Clearing -> Lost':57,
'Expanding -> Stabilizing -> Displacing':52,'Shattering -> Expanding -> Lost':46,
'Shattering -> Expanding -> Expanding':44,'Shattering -> Stabilizing -> Expanding':44}
ref4={'Shattering -> Lost -> Lost':632,'Clearing -> Lost -> Lost':101,
'Shattering -> Shattering -> Lost':72,'Displacing -> Lost -> Lost':65,
'Shattering -> Clearing -> Lost':57,'Shattering -> Expanding -> Lost':46,
'Displacing -> Stabilizing -> Lost':43,'Shattering -> Displacing -> Lost':41,
'Displacing -> Clearing -> Lost':27,'Expanding -> Lost -> Lost':23}
assert all(path[k]==v for k,v in ref3.items())
assert all(path[k]==v for k,v in ref4.items())
stat0=stat(r)
counts=Counter(z['City'] for z in r)
w=[1/counts[z['City']] for z in r]
statw=stat(r,w)
assert abs(stat0[0]-1.816)<.0006 and abs(stat0[1]-1.703)<.0006
assert abs(stat0[2]-.113)<.0006 and abs(stat0[3]-500.08)<.01
assert abs(statw[2]-.130)<.0006
samples=[r, [z for z in r if z['S_1996_2007']!='Lost' and z['S_2007_2015']!='Lost']]
hist=Counter((z['City'],z['S_1996_2007'],z['S_2007_2015']) for z in r)
samples.append([z for z in r if hist[z['City'],z['S_1996_2007'],z['S_2007_2015']]>=5])
samples.append([z for z in r if not z['Boundary'].strip().lower() in ('yes','y','true','1','t','boundary')])
assert [len(x) for x in samples]==[3186,2359,2882,2965]
for sample,target in zip(samples,[.113,.153,.126,.119]): assert abs(stat(sample)[2]-target)<.0006
print('PASS Input CSVs: 1212 training; 500 validation; 4584 long-interval; 3186 trajectory records')
print('PASS Manifest and SHA256: 30 source files; 8,909 intermediate interval predictions')
print('PASS First-interval RF predictions vs cohort: exact multiset agreement for 3,186 rows')
print('PASS Published Tables 3 and 4: all 20 reported trajectory counts')
print('PASS Published Table 5: pooled H and LR, pooled and equal-area delta-H')
print('PASS Published Table 6: all four sample sizes and delta-H point estimates')
print('Pooled entropies, delta-H, LR:',' '.join(f'{v:.6f}' for v in stat0))
print('Equal-city-weighted delta-H:',round(statw[2],6))
print('NOTE: this is a Python source-data audit; complete R execution must run locally.')

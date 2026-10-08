#!/usr/bin/env python3
"""Independent numerical checks of the supplied CSV against published tables.
Not an R runtime test. Uses only pandas/numpy/Python; no R code is executed.
"""
import pandas as pd
import numpy as np
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
raw = ROOT / 'data/processed/RECONSTRUCTED_TRAJECTORIES.csv'
d = pd.read_csv(raw, dtype={'City_ID_1996':str})
s1,s2,s3 = ['S_1996_2007','S_2007_2015','S_2015_2020']
assert len(d)==3186, f'Trajectory rows !=3186: {len(d)}'
assert d['City'].nunique()==10
assert d[[s1,s2,s3]].notna().all().all()
assert not ((d[s1]=='Lost')&(d[s2]!='Lost')).any()
assert not ((d[s2]=='Lost')&(d[s3]!='Lost')).any()
assert d['Boundary'].eq('YES').sum()==221
train=pd.read_csv(ROOT/'data/raw/TRAINING_DATA.csv')
valid=pd.read_csv(ROOT/'data/processed/EXTERNAL_VALIDATION.csv')
assert train.shape[0]==1212 and valid.shape[0]==500
assert train['City'].nunique()==5 and valid['City'].nunique()==5

paths=d[[s1,s2,s3]].agg(' -> '.join,axis=1)
counts=paths.value_counts()
ref3={
'Shattering -> Lost -> Lost':632,
'Clearing -> Lost -> Lost':101,
'Displacing -> Stabilizing -> Displacing':96,
'Shattering -> Shattering -> Lost':72,
'Displacing -> Lost -> Lost':65,
'Shattering -> Clearing -> Lost':57,
'Expanding -> Stabilizing -> Displacing':52,
'Shattering -> Expanding -> Lost':46,
'Shattering -> Expanding -> Expanding':44,
'Shattering -> Stabilizing -> Expanding':44}
ref4={
'Shattering -> Lost -> Lost':632,
'Clearing -> Lost -> Lost':101,
'Shattering -> Shattering -> Lost':72,
'Displacing -> Lost -> Lost':65,
'Shattering -> Clearing -> Lost':57,
'Shattering -> Expanding -> Lost':46,
'Displacing -> Stabilizing -> Lost':43,
'Shattering -> Displacing -> Lost':41,
'Displacing -> Clearing -> Lost':27,
'Expanding -> Lost -> Lost':23}
for k,v in ref3.items(): assert counts.get(k,0)==v, f'Table 3 {k}: {counts.get(k,0)} vs {v}'
for k,v in ref4.items(): assert counts.get(k,0)==v, f'Table 4 {k}: {counts.get(k,0)} vs {v}'

def Hgroup(data, cols, w=None):
    if w is None: w = np.ones(len(data))
    x=data.loc[:,cols+[s3]].copy()
    x['weight']=w
    agg=x.groupby(cols+[s3],dropna=False)['weight'].sum()
    prior=agg.groupby(level=list(range(len(cols)))).sum()
    p=agg.values/prior.reindex(agg.index.droplevel(-1)).values
    return float(-np.sum(agg.values*np.log2(p))/sum(w))

def calc(data,w=None):
    h0=Hgroup(data,[s2],w)
    h1=Hgroup(data,[s1,s2],w)
    n=len(data) if w is None else sum(w)
    return h0,h1,h0-h1,2*n*np.log(2)*(h0-h1),-n*np.log(2)*h0,-n*np.log(2)*h1

h0,h1,delta,lr,ll0,ll1=calc(d)
w=1/d['City'].map(d['City'].value_counts()).values
area_delta=calc(d,w)[2]
assert abs(h0-1.816)<0.0006
assert abs(h1-1.703)<0.0006
assert abs(delta-0.113)<0.0006
assert abs(lr-500.08)<0.01
assert abs(ll0-(-4010.9))<0.1
assert abs(ll1-(-3760.9))<0.1
assert abs(area_delta-0.130)<0.0006

mask1=(d[s1]!='Lost')&(d[s2]!='Lost')
ref_hist_n=d.groupby(['City',s1,s2])[s1].transform('size')
mask2=ref_hist_n>=5
mask3=~d['Boundary'].eq('YES')
subsets=[d,d[mask1],d[mask2],d[mask3]]
expected_ns=[3186,2359,2882,2965]
expected_deltas=[.113,.153,.126,.119]
for ss,n,dd in zip(subsets,expected_ns,expected_deltas):
    assert len(ss)==n, (len(ss),n)
    assert abs(calc(ss)[2]-dd)<.0006, (calc(ss)[2],dd)

cityacc=valid.groupby('City').apply(lambda df: (df.Manual_Typology==df.Predicted_Typology).mean(),include_groups=False)
pubacc=pd.Series({'Cotabato':.85,'Sandakan':.85,'Singapore-Johor Bahru':.86,'Surabaya':.92,'Vung Tau':.84})
report=[
 'Independent reference check (Python, not an R execution test)',
 f'Trajectory rows = {len(d)}, urban areas = {d.City.nunique()}',
 f'Training = {len(train)} rows; external validation = {len(valid)} rows',
 'Tables 3 and 4: all 10 exact trajectory counts PASSED',
 f'Pooled H_recent={h0:.6f} H_history={h1:.6f} DeltaH={delta:.6f}',
 f'LL_recent={ll0:.4f} LL_history={ll1:.4f} LR={lr:.4f}',
 f'Equal urban-area DeltaH={area_delta:.6f}',
 f'Table 6 counts and DeltaH: PASSED (sizes {expected_ns})',
 'External validation comparison:']
for city,acc in cityacc.items():
    report.append(f'  {city}: CSV {acc:.3f}, published {pubacc[city]:.2f}')
report += ['R syntax/execution not established by these tests.',
           'Script 04 cannot execute until ALL_PATCH_TRANSITIONS.csv is supplied.']
out='\n'.join(report)+'\n'
print(out)
(ROOT/'tests/reference_validation_report.txt').write_text(out)

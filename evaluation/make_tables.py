"""Combine ours_results.tsv and casaal_results.tsv into results.tsv and a
LaTeX table (results_table.tex)."""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))


def read(fn):
    with open(os.path.join(HERE, fn), encoding='utf-8') as f:
        return {r['id']: r for r in csv.DictReader(f, delimiter='\t')}


forms = []
for line in open(os.path.join(HERE, 'formulas.tsv'), encoding='utf-8'):
    if line.startswith('#') or not line.strip():
        continue
    fid, desc, ours, cas = line.rstrip('\n').split('\t')
    forms.append((fid, desc, ours, cas))

ours, cas = read('ours_results.tsv'), read('casaal_results.tsv')
cols = ['id', 'description', 'clocks_f', 'spot_st', 'spot_tr', 'comp_st', 'comp_tr',
        'opt_st', 'opt_tr', 'opt_clk', 'exp_st', 'exp_tr', 'exp_clk', 'exp_inv', 'exp_diff',
        'time_s', 'casaal_exact', 'casaal_st', 'casaal_tr', 'casaal_clk', 'casaal_s']
rows = []
for fid, desc, _, _ in forms:
    o, c = ours.get(fid, {}), cas.get(fid, {})
    rows.append([fid, desc, o.get('formula_clocks'), o.get('spot_states'), o.get('spot_trans'),
                 o.get('comp_locs'), o.get('comp_trans'), o.get('opt_locs'), o.get('opt_trans'),
                 o.get('opt_clocks'), o.get('exp_locs'), o.get('exp_trans'), o.get('exp_clocks'),
                 o.get('exp_invs'), o.get('exp_diffs'), o.get('total_s'), c.get('exact'),
                 c.get('states'), c.get('transitions'), c.get('clocks'), c.get('time_s')])
with open(os.path.join(HERE, 'results.tsv'), 'w', encoding='utf-8', newline='') as f:
    w = csv.writer(f, delimiter='\t')
    w.writerow(cols)
    w.writerows(rows)


def tex_escape(s):
    return s.replace('&', r'\&').replace('_', r'\_')


lines = [r'\begin{table*}[t]', r'\centering',
         r'\caption{Evaluation of \texttt{mtl2tba} and comparison with CASAAL. '
         r'$|X|$: distinct timed subformulas of the formula. Spot: B\"uchi automaton '
         r'returned by Spot (states/transitions, one transition per cube). Opt.: after '
         r'reset completion and the verified optimizations (locations/transitions/clocks). '
         r'Export: automaton for UPPAAL (locations/transitions/clocks/locations with an '
         r'invariant/difference constraints). Time: whole chain, in seconds. CASAAL: '
         r'states/transitions/clocks; $\dagger$: not equivalent (CASAAL has no hatted '
         r'operators).}',
         r'\label{tab:evaluation}', r'\small',
         r'\begin{tabular}{@{}llrrrrrrrrrrrrr@{}}', r'\toprule',
         r' & & & \multicolumn{2}{c}{Spot} & \multicolumn{3}{c}{Opt.} & '
         r'\multicolumn{5}{c}{Export} & & CASAAL\\',
         r'\cmidrule(lr){4-5}\cmidrule(lr){6-8}\cmidrule(lr){9-13}',
         r'Id & Property & $|X|$ & st & tr & loc & tr & clk & loc & tr & clk & inv & diff '
         r'& Time & st/tr/clk\\', r'\midrule']
for r in rows:
    (fid, desc, xf, sst, str_, cst, ctr, ost, otr, oclk, est, etr, eclk, einv, ediff,
     t, cex, cst2, ctr2, cclk, cs) = r
    casaal = '--' if cst2 in (None, '-', '') else f'{cst2}/{ctr2}/{cclk}' + (
        '$^\\dagger$' if cex == 'no' else '')
    lines.append(f'{fid} & {tex_escape(desc)} & {xf} & {sst} & {str_} & {ost} & {otr} & '
                 f'{oclk} & {est} & {etr} & {eclk} & {einv} & {ediff} & {float(t):.2f} & '
                 f'{casaal}\\\\')
lines += [r'\bottomrule', r'\end{tabular}', r'\end{table*}']
open(os.path.join(HERE, 'results_table.tex'), 'w', encoding='utf-8').write('\n'.join(lines) + '\n')
print('\n'.join('\t'.join(str(x) for x in r) for r in [cols] + rows))

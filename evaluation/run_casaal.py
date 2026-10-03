"""Run CASAAL on every formula of formulas.tsv and count states, transitions,
and clocks of the produced timed Buchi automaton (dot output).

Usage (Windows, from this folder):  python run_casaal.py <path-to-casaal-folder>
Writes casaal_results.tsv.
"""
import os, re, shutil, subprocess, sys, tempfile, time

HERE = os.path.dirname(os.path.abspath(__file__))
CASAAL_DIR = sys.argv[1] if len(sys.argv) > 1 else r'C:\Users\user\Desktop\casaal\casaal'


def formulas():
    for line in open(os.path.join(HERE, 'formulas.tsv'), encoding='utf-8'):
        if line.startswith('#') or not line.strip():
            continue
        fid, desc, ours, cas = line.rstrip('\n').split('\t')
        yield fid, desc, ours, cas


def count(dot):
    nodes = set(re.findall(r'^\s*(\d+)\s*\[label', dot, re.M)) - {'0'}
    edges = [e for e in re.findall(r'^\s*(\d+)\s*->\s*(\d+)', dot, re.M) if e[0] != '0']
    clocks = set(re.findall(r'\bx(\d+)\b', dot))
    return len(nodes), len(edges), len(clocks)


def main():
    work = tempfile.mkdtemp()
    for f in os.listdir(CASAAL_DIR):
        if f.endswith('.exe') or f.endswith('.dll'):
            shutil.copy(os.path.join(CASAAL_DIR, f), work)
    out = open(os.path.join(HERE, 'casaal_results.tsv'), 'w', encoding='utf-8')
    out.write('id\texact\tstates\ttransitions\tclocks\ttime_s\n')
    for fid, desc, ours, cas in formulas():
        if cas == '-':
            out.write(f'{fid}\t-\t-\t-\t-\t-\n')
            continue
        exact = 'no' if cas.startswith('~') else 'yes'
        cas = cas.lstrip('~')
        open(os.path.join(work, 'f.txt'), 'w').write(cas + '\n')
        dotf = os.path.join(work, 'dot_output.gv')
        if os.path.exists(dotf):
            os.remove(dotf)
        t0 = time.time()
        try:
            subprocess.run([os.path.join(work, 'casaal.exe'), 'f.txt'], cwd=work,
                           capture_output=True, timeout=600)
            dt = time.time() - t0
            s, t, c = count(open(dotf).read())
            out.write(f'{fid}\t{exact}\t{s}\t{t}\t{c}\t{dt:.2f}\n')
            print(fid, exact, s, t, c, f'{dt:.2f}s')
        except Exception as e:
            out.write(f'{fid}\t{exact}\terror\t\t\t\n')
            print(fid, 'error', e)
    out.close()


if __name__ == '__main__':
    main()

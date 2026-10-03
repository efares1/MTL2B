"""Removes the dead code of an extracted OCaml file.

Coq extracts the whole module of the real numbers (RbaseSymbolsImpl, with
the Dedekind construction and its positive/Z/Q arithmetic) as soon as the
type R is mentioned, even when every real operation is realized inline by
an OCaml float operation.  This script keeps only the top-level definitions
reachable from the given roots and deletes the others.

Usage: python3 prune_extraction.py file.ml root1 root2 ...
"""
import re
import sys

START = re.compile(r'^(let |type |module |include |exception )')
IDENT = re.compile(r"[A-Za-z_][A-Za-z0-9_']*")
COMMENT = re.compile(r'\(\*.*?\*\)', re.S)


def chunks(text):
    """Split into top-level items; a (** val ... **) comment and the blank
    lines before an item are attached to it, an [and] to its [let]/[type]."""
    lines = text.split('\n')
    items, cur, pending = [], [], []
    for line in lines:
        if START.match(line):
            if cur:
                items.append(cur)
            cur = pending + [line]
            pending = []
        elif line.startswith('(** val') or (line.strip() == '' and not cur):
            pending.append(line)
        elif line.strip() == '' or line.startswith('(**'):
            pending.append(line)
        else:
            cur.extend(pending)
            pending = []
            cur.append(line)
    if cur:
        items.append(cur)
    tail = pending
    return items, tail


def defined(item):
    """Names defined by an item: values, types, constructors, fields, modules."""
    text = '\n'.join(item)
    code = COMMENT.sub('', text)
    names = set()
    for m in re.finditer(r'^(?:let|and)(?: rec)? ([a-z_][A-Za-z0-9_\']*)', code, re.M):
        names.add(m.group(1))
    for m in re.finditer(r'^(?:type|and) (?:\'[a-z]+ |\([^)]*\) )?([a-z_][A-Za-z0-9_\']*)', code, re.M):
        names.add(m.group(1))
    for m in re.finditer(r'^module (?:type )?([A-Z][A-Za-z0-9_\']*)', code, re.M):
        names.add(m.group(1))
    if code.startswith('type') or '\ntype' in code:
        names |= set(re.findall(r'\|\s*([A-Z][A-Za-z0-9_\']*)', code))
        names |= set(re.findall(r'[{;]\s*([a-z_][A-Za-z0-9_\']*)\s*:', code))
    return names


def used(item):
    return set(IDENT.findall(COMMENT.sub('', '\n'.join(item))))


def main():
    path, roots = sys.argv[1], set(sys.argv[2:])
    text = open(path, encoding='utf-8').read()
    items, tail = chunks(text)
    defs = [defined(it) for it in items]
    uses = [used(it) - d for it, d in zip(items, defs)]
    keep = [bool(d & roots) for d in defs]
    changed = True
    while changed:
        changed = False
        needed = set().union(*[u for u, k in zip(uses, keep) if k])
        for i, d in enumerate(defs):
            if not keep[i] and d & needed:
                keep[i] = changed = True
    out = '\n'.join('\n'.join(it) for it, k in zip(items, keep) if k)
    out = re.sub(r'\n{3,}', '\n\n', out).strip('\n') + '\n'
    open(path, 'w', encoding='utf-8', newline='\n').write(out)
    print(f'{path}: kept {sum(keep)} of {len(items)} top-level items')


if __name__ == '__main__':
    main()

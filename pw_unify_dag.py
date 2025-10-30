# pw_unify_dag.py
from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Tuple, Iterable, Optional
import sys
import traceback
import networkx as nx

# ---------------------------
# Term surface helpers
# ---------------------------

@dataclass(frozen=True)
class Var:
    name: str
    def __repr__(self) -> str:
        return self.name

def fun(sym: str, *args: Any) -> Tuple:
    """Convenience for ('f', a, b, ...) with constants as arity-0 ('a',)."""
    return (sym, *args)

# Pretty-printer for Python term surface
def pp(x: Any) -> str:
    if isinstance(x, Var):
        return x.name
    assert isinstance(x, tuple) and len(x) >= 1
    sym = x[0]
    if len(x) == 1:
        return sym
    return f"{sym}(" + ", ".join(pp(a) for a in x[1:]) + ")"

# Apply a substitution {VarName -> term} to a Python-surface term
def apply_subst(term: Any, subst: Dict[str, Any]) -> Any:
    if isinstance(term, Var):
        return _deepcopy_term(subst.get(term.name, term))
    assert isinstance(term, tuple)
    if len(term) == 1:
        return term
    return (term[0], *[apply_subst(a, subst) for a in term[1:]])

def _deepcopy_term(t: Any) -> Any:
    if isinstance(t, Var):
        return Var(t.name)
    assert isinstance(t, tuple)
    if len(t) == 1:
        return (t[0],)
    return (t[0], *[_deepcopy_term(a) for a in t[1:]])

# ---------------------------
# DAG container
# ---------------------------

class TermDAG:
    """
    NetworkX MultiDiGraph where each node has:
      - kind: 'var' or 'fun'
      - sym:  variable name or function symbol
      - arity: int (0 for constants, 0 for vars)

    Edges: parent -> child, with attribute 'pos' = argument index (0..arity-1).
           Multi-edges allow repeated children like f(X, X).
    """
    def __init__(self):
        self.G = nx.MultiDiGraph()
        self._next_id = 0
        self._intern_fun: Dict[Tuple, int] = {}   # structural hash-consing for fun nodes
        self._intern_var: Dict[str, int] = {}     # interning vars by name

    def add_var(self, name: str) -> int:
        # Intern variables so the same variable name refers to the same node
        if name in self._intern_var:
            return self._intern_var[name]
        nid = self._alloc()
        self.G.add_node(nid, kind='var', sym=name, arity=0)
        self._intern_var[name] = nid
        return nid

    def add_fun(self, sym: str, child_ids: Iterable[int]) -> int:
        child_ids = tuple(child_ids)
        arity = len(child_ids)
        key = ('fun', sym, child_ids)
        # Intern function nodes by full structure (creates sharing within one DAG build)
        if key in self._intern_fun:
            return self._intern_fun[key]
        nid = self._alloc()
        self.G.add_node(nid, kind='fun', sym=sym, arity=arity)
        for i, c in enumerate(child_ids):
            # MultiDiGraph: multiple edges (nid -> c) allowed with different 'pos'
            self.G.add_edge(nid, c, pos=i)
        self._intern_fun[key] = nid
        return nid

    def add_from_python(self, term: Any) -> int:
        """
        Accepts:
          - Var('X')
          - ('f', t1, t2, ...)
          - constants as ('a',) or fun('a')
        Returns node id for the root.
        """
        if isinstance(term, Var):
            return self.add_var(term.name)
        if isinstance(term, tuple) and len(term) >= 1 and isinstance(term[0], str):
            sym = term[0]
            args = [self.add_from_python(t) for t in term[1:]]
            return self.add_fun(sym, args)
        raise TypeError(f"Unsupported term literal: {term!r}")

    # --- utilities ---
    def is_var(self, n: int) -> bool:
        return self.G.nodes[n]['kind'] == 'var'

    def is_fun(self, n: int) -> bool:
        return self.G.nodes[n]['kind'] == 'fun'

    def sym(self, n: int) -> str:
        return self.G.nodes[n]['sym']

    def arity(self, n: int) -> int:
        return self.G.nodes[n]['arity']

    def children(self, n: int) -> List[int]:
        """
        Return children in argument order (pos 0..arity-1), preserving duplicates.
        """
        # For MultiDiGraph we must ask for keys and data to get all parallel edges.
        outs = self.G.out_edges(n, keys=True, data=True)
        # Build array of size arity (since pos is 0..arity-1). Some graphs could be
        # malformed; we guard by sorting and then collecting by pos.
        by_pos = sorted([(d.get('pos', 0), v) for (_, v, _k, d) in outs], key=lambda t: t[0])
        return [v for (_pos, v) in by_pos]

    def _alloc(self) -> int:
        nid = self._next_id
        self._next_id += 1
        return nid

# ---------------------------
# Union-Find (Disjoint Set)
# ---------------------------

class UnionFind:
    def __init__(self):
        self.parent: Dict[int, int] = {}
        self.rank: Dict[int, int] = {}

    def make(self, x: int):
        if x not in self.parent:
            self.parent[x] = x
            self.rank[x] = 0

    def find(self, x: int) -> int:
        px = self.parent.get(x, x)
        if px != x:
            self.parent[x] = self.find(px)
        else:
            self.parent.setdefault(x, x)
            self.rank.setdefault(x, 0)
        return self.parent[x]

    def union(self, a: int, b: int) -> int:
        ra, rb = self.find(a), self.find(b)
        if ra == rb:
            return ra
        if self.rank[ra] < self.rank[rb]:
            ra, rb = rb, ra
        self.parent[rb] = ra
        if self.rank[ra] == self.rank[rb]:
            self.rank[ra] += 1
        return ra

    def attach_rep_to(self, child_rep: int, parent_rep: int) -> int:
        """
        Forcefully attach the representative 'child_rep' under 'parent_rep',
        making 'parent_rep' the class representative (used for var -> term).
        """
        rc, rp = self.find(child_rep), self.find(parent_rep)
        if rc == rp:
            return rp
        self.parent[rc] = rp
        self.rank[rp] = max(self.rank.get(rp, 0), self.rank.get(rc, 0))
        return rp

# ---------------------------
# Paterson–Wegman Unification
# ---------------------------

class UnifyError(Exception):
    pass

def occurs_check_uf(dag: TermDAG, var_rep: int, term_rep: int, uf: UnionFind) -> bool:
    """True if variable rep appears inside term rep (following UF-compressed children)."""
    if var_rep == term_rep:
        return True
    if not dag.is_var(var_rep):
        return False
    seen = set()
    stack = [term_rep]
    while stack:
        n = uf.find(stack.pop())
        if n in seen:
            continue
        seen.add(n)
        if n == var_rep:
            return True
        if dag.is_fun(n):
            for c in dag.children(n):
                stack.append(uf.find(c))
    return False

def pw_unify(dag: TermDAG, a: int, b: int) -> Dict[str, Any]:
    """
    Paterson–Wegman style linear unification on a DAG with union–find.
    Returns a substitution dict: {var_name: term_as_python}.
    Raises UnifyError on clash or occurs-check failure.
    """
    uf = UnionFind()
    reachable = _collect_reachable_nodes(dag, [a, b])
    for n in reachable:
        uf.make(n)

    stack = [(a, b)]
    while stack:
        x, y = stack.pop()
        rx, ry = uf.find(x), uf.find(y)
        if rx == ry:
            continue

        kx, ky = dag.G.nodes[rx]['kind'], dag.G.nodes[ry]['kind']

        # Variable vs function/constant: attach var under term (deterministic direction)
        if kx == 'var' and ky != 'var':
            if occurs_check_uf(dag, rx, ry, uf):
                raise UnifyError(f"Occurs-check failed: {dag.sym(rx)} occurs in term")
            uf.attach_rep_to(rx, ry)  # var -> term
            continue
        if ky == 'var' and kx != 'var':
            if occurs_check_uf(dag, ry, rx, uf):
                raise UnifyError(f"Occurs-check failed: {dag.sym(ry)} occurs in term")
            uf.attach_rep_to(ry, rx)  # var -> term
            continue

        # Variable vs variable
        if kx == 'var' and ky == 'var':
            uf.union(rx, ry)
            continue

        # Both function nodes
        sx, sy = dag.sym(rx), dag.sym(ry)
        ax, ay = dag.arity(rx), dag.arity(ry)
        if sx != sy or ax != ay:
            raise UnifyError(f"Clash: {sx}/{ax} vs {sy}/{ay}")
        uf.union(rx, ry)
        cx = [uf.find(c) for c in dag.children(rx)]
        cy = [uf.find(c) for c in dag.children(ry)]
        # Enqueue aligned argument pairs (preserving duplicates)
        for i in range(ax):
            stack.append((cx[i], cy[i]))

    # Build minimal substitution
    substitution: Dict[str, Any] = {}
    vars_in_problem = [n for n in reachable if dag.is_var(n)]
    for v in vars_in_problem:
        rep = uf.find(v)
        if rep == v:
            continue
        py = _to_python_term(dag, rep, uf)
        substitution[dag.sym(v)] = py
    return substitution

# ---------------------------
# Helpers
# ---------------------------

def _collect_reachable_nodes(dag: TermDAG, roots: List[int]) -> List[int]:
    seen = set()
    stack = list(roots)
    while stack:
        n = stack.pop()
        if n in seen:
            continue
        seen.add(n)
        for c in dag.children(n):
            stack.append(c)
    return list(seen)

def _to_python_term(dag: TermDAG, n: int, uf: Optional[UnionFind] = None) -> Any:
    rep = uf.find(n) if uf else n
    if dag.is_var(rep):
        return Var(dag.sym(rep))
    sym = dag.sym(rep)
    ar = dag.arity(rep)
    if ar == 0:
        return fun(sym)
    args = [_to_python_term(dag, uf.find(c) if uf else c, uf) for c in dag.children(rep)]
    return fun(sym, *args)

# ---------------------------
# Test helpers
# ---------------------------

def unify_and_compare(t1: Any, t2: Any, expect_ok: bool = True):
    """Unify t1 ~ t2. If expect_ok, also check that applying the substitution
    makes both sides equal structurally."""
    dag = TermDAG()
    r1 = dag.add_from_python(t1)
    r2 = dag.add_from_python(t2)
    if expect_ok:
        subst = pw_unify(dag, r1, r2)
        # Structural equality after substitution on surface terms
        u1 = apply_subst(t1, subst)
        u2 = apply_subst(t2, subst)
        if u1 != u2:
            pretty_subst = {name: pp(val) for name, val in subst.items()}
            raise AssertionError(
                "Not equal after subst:\n"
                f"  {pp(u1)}\n"
                f"  {pp(u2)}\n"
                f"subst={pretty_subst}"
            )
        return subst
    else:
        try:
            _ = pw_unify(dag, r1, r2)
        except UnifyError:
            return None
        raise AssertionError("Expected failure, but unification succeeded.")

def assert_subst_has(subst: Dict[str, Any], **bindings):
    """Assert that subst contains each binding var=term (surface Python term)."""
    for v, term in bindings.items():
        got = subst.get(v, None)
        assert got is not None, f"Missing binding for {v}"
        assert got == term, f"For {v}: expected {pp(term)}, got {pp(got)}"

# ---------------------------
# 10 runnable tests
# ---------------------------

def test_01_basic_cross():
    # f(g(a), X)  ~~  f(Y, g(b))  => X=g(b), Y=g(a)
    X, Y = Var('X'), Var('Y')
    a, b = fun('a'), fun('b')
    t1 = fun('f', fun('g', a), X)
    t2 = fun('f', Y, fun('g', b))
    s = unify_and_compare(t1, t2)
    assert_subst_has(s, X=fun('g', b), Y=fun('g', a))

def test_02_var_var_chain():
    # h(X, Y) ~~ h(Y, a)  => X=a, Y=a
    X, Y = Var('X'), Var('Y')
    a = fun('a')
    t1 = fun('h', X, Y)
    t2 = fun('h', Y, a)
    s = unify_and_compare(t1, t2)
    assert_subst_has(s, X=a, Y=a)

def test_03_occurs_check():
    # X ~~ f(X)  => fail
    X = Var('X')
    t1 = X
    t2 = fun('f', X)
    unify_and_compare(t1, t2, expect_ok=False)

def test_04_identical_terms_empty_subst():
    # f(a,b) ~~ f(a,b)  => {}
    a, b = fun('a'), fun('b')
    t = fun('f', a, b)
    s = unify_and_compare(t, t)
    assert s == {}, f"Expected empty substitution, got {s}"

def test_05_symbol_clash():
    # f(a) ~~ g(a)  => fail
    a = fun('a')
    t1 = fun('f', a)
    t2 = fun('g', a)
    unify_and_compare(t1, t2, expect_ok=False)

def test_06_arity_mismatch():
    # f(a,b) ~~ f(a)  => fail
    a, b = fun('a'), fun('b')
    t1 = fun('f', a, b)
    t2 = fun('f', a)
    unify_and_compare(t1, t2, expect_ok=False)

def test_07_simple_swap():
    # f(X, a) ~~ f(b, Y)  => X=b, Y=a
    X, Y = Var('X'), Var('Y')
    a, b = fun('a'), fun('b')
    t1 = fun('f', X, a)
    t2 = fun('f', b, Y)
    s = unify_and_compare(t1, t2)
    assert_subst_has(s, X=b, Y=a)

def test_08_doubleton_mismatch():
    # f(X, X) ~~ f(a, b)  => fail unless a==b; here a!=b, so fail
    X = Var('X')
    a, b = fun('a'), fun('b')
    t1 = fun('f', X, X)
    t2 = fun('f', a, b)
    unify_and_compare(t1, t2, expect_ok=False)

def test_09_shared_subdag_linear():
    # Shared subterm s=g(h(a))
    # f(s, s, X) ~~ f(Y, g(h(a)), g(h(a)))  => X=g(h(a)), Y=g(h(a))
    X, Y = Var('X'), Var('Y')
    a = fun('a')
    s = fun('g', fun('h', a))
    t1 = fun('f', s, s, X)
    t2 = fun('f', Y, fun('g', fun('h', a)), fun('g', fun('h', a)))
    subst = unify_and_compare(t1, t2)
    expect = fun('g', fun('h', a))
    assert_subst_has(subst, X=expect, Y=expect)

def test_10_nested_deep():
    # k(X, f(Y, Z), Z) ~~ k(g(a), f(g(a), b), b) => X=g(a), Y=g(a), Z=b
    X, Y, Z = Var('X'), Var('Y'), Var('Z')
    a, b = fun('a'), fun('b')
    t1 = fun('k', X, fun('f', Y, Z), Z)
    t2 = fun('k', fun('g', a), fun('f', fun('g', a), b), b)
    subst = unify_and_compare(t1, t2)
    assert_subst_has(subst, X=fun('g', a), Y=fun('g', a), Z=b)

# ---------------------------
# Test runner
# ---------------------------

def _run_all_tests():
    tests = [
        test_01_basic_cross,
        test_02_var_var_chain,
        test_03_occurs_check,
        test_04_identical_terms_empty_subst,
        test_05_symbol_clash,
        test_06_arity_mismatch,
        test_07_simple_swap,
        test_08_doubleton_mismatch,
        test_09_shared_subdag_linear,
        test_10_nested_deep,
    ]
    total = len(tests)
    passed = 0
    for i, t in enumerate(tests, 1):
        name = t.__name__
        try:
            t()
            print(f"[{i:02d}/{total}] PASS - {name}")
            passed += 1
        except AssertionError as e:
            print(f"[{i:02d}/{total}] FAIL - {name}: {e}")
            traceback.print_exc(limit=1)
        except Exception as e:
            print(f"[{i:02d}/{total}] ERROR - {name}: {e}")
            traceback.print_exc(limit=1)
    print(f"\n{passed}/{total} tests passed.")
    if passed != total:
        sys.exit(1)

if __name__ == "__main__":
    _run_all_tests()

import process from 'node:process';
import { parse } from '@pgsql/parser/v17';
import { deparse } from 'pgsql-parser';
import type {
  ColumnDef,
  Constraint,
  CreateStmt,
  Node,
  RangeVar,
  RawStmt,
  TypeName,
} from '@pgsql/types';

const qname = (rangeVar: RangeVar): string =>
  (rangeVar.schemaname ? `${rangeVar.schemaname}.` : '') + rangeVar.relname;

const sval = (n: Node): string | undefined =>
  'String' in n ? n.String?.sval : undefined;

const identList = (xs: Node[]): string[] =>
  Array.isArray(xs) ? (xs.map(sval).filter(Boolean) as string[]) : [];

const strip = <T extends Record<string, any>, K extends keyof T>(o: T, ...ks: K[]) => {
  const r: any = { ...o };
  for (const k of ks) delete r[k];
  return r as Omit<T, K>;
};

(async () => {
  let input = '';
  for await (const chunk of process.stdin) input += chunk.toString();

  // psql meta-commands pgsql parser won't accept
  input = input.replaceAll(/\\(un)?restrict [a-zA-Z0-9]+/g, '');

  const ast = await parse(input);
  const stmts: RawStmt[] = ast.stmts ?? [];

  const typeNodes: Node[] = [];
  const creates: CreateStmt[] = [];
  const extraByTable = new Map<string, Constraint[]>();
  const identityByTableCol = new Map<string, Map<string, Constraint[]>>();

  // 1) collect: types/domains + CREATE TABLE + ALTER TABLE ADD CONSTRAINT
  for (const raw of stmts) {
    const stmt = raw.stmt;
    if (!stmt) continue;

    if (
      'CreateDomainStmt' in stmt ||
      'CompositeTypeStmt' in stmt ||
      'CreateEnumStmt' in stmt ||
      'CreateRangeStmt' in stmt
    ) {
      typeNodes.push(stmt);
      continue;
    }

    if ('CreateStmt' in stmt) {
      creates.push(stmt.CreateStmt);
      continue;
    }

    if ('AlterTableStmt' in stmt) {
      const alt = stmt.AlterTableStmt;
      const t = qname(alt.relation!);

      for (const w of alt.cmds ?? []) {
        if (!('AlterTableCmd' in w)) continue;
        const cmd = w.AlterTableCmd;

        const subtype = String(cmd.subtype ?? '');
        if (subtype.includes('UsingIndex')) continue;

        if (subtype === 'AT_AddConstraint' || subtype === 'AT_AddConstraintRecurse') {
          // ALTER TABLE ... ADD CONSTRAINT ...
          if (!cmd.def || !('Constraint' in cmd.def)) continue;
          const cons: Constraint = cmd.def.Constraint;
          const arr = extraByTable.get(t);
          if (arr) arr.push(cons);
          else extraByTable.set(t, [cons]);
        } else if (subtype === 'AT_AddIdentity') {
          // ALTER TABLE ... ALTER COLUMN <col> ADD GENERATED ... AS IDENTITY ...
          if (!cmd.name) continue;
          if (!cmd.def || !('Constraint' in cmd.def)) continue;
          const cons: Constraint = cmd.def.Constraint;
          if (String(cons.contype ?? '') === 'CONSTR_IDENTITY') {
            let byCol = identityByTableCol.get(t);
            if (!byCol) {
              byCol = new Map();
              identityByTableCol.set(t, byCol);
            }
            // minimal identity: GENERATED {ALWAYS|BY DEFAULT} AS IDENTITY
            const minimal: Constraint = {
              contype: cons.contype,
              generated_when: cons.generated_when,
            };
            const list = byCol.get(cmd.name);
            if (list) list.push(minimal);
            else byCol.set(cmd.name, [minimal]);
          }
        }
      }
    }
  }

  // 2) compact: merge single-col PK/UNIQUE/FK into ColumnDef constraints
  for (const create of creates) {
    const t = qname(create.relation!);

    const cols: ColumnDef[] = [];
    const otherElts: Node[] = [];
    const tableCons: Constraint[] = [];
    const colByName = new Map<string, ColumnDef>();

    for (const e of create.tableElts ?? []) {
      if ('ColumnDef' in e) {
        cols.push(e.ColumnDef);
        colByName.set(String(e.ColumnDef.colname), e.ColumnDef);
      } else if ('Constraint' in e) {
        tableCons.push(e.Constraint);
      } else {
        otherElts.push(e);
      }
    }

    // Fold ALTER COLUMN ... ADD GENERATED ... AS IDENTITY into ColumnDef.constraints
    for (const [colName, consList] of identityByTableCol.get(t) ?? []) {
      const col = colByName.get(colName);
      if (!col) continue;
      col.constraints ??= [];
      // remove any existing identity constraint; keep the last applied one
      col.constraints = col.constraints.filter((w) => {
        if (!('Constraint' in w)) return true;
        return String(w.Constraint.contype ?? '') !== 'CONSTR_IDENTITY';
      });
      const last = consList.at(-1)!;
      col.constraints.push({ Constraint: last });
    }

    const allCons = [...tableCons, ...(extraByTable.get(t) ?? [])];
    const remaining: Constraint[] = [];

    for (const c0 of allCons) {
      const c = strip(c0, 'conname');
      const contype = String(c.contype ?? '');

      if (contype === 'CONSTR_PRIMARY' || contype === 'CONSTR_UNIQUE') {
        const keys = identList(c.keys || []);
        if (keys.length === 1) {
          const col = colByName.get(keys[0]);
          if (col) {
            col.constraints ??= [];
            col.constraints.push({ Constraint: strip(c, 'keys') });
            continue;
          }
        }
        remaining.push(c);
        continue;
      }

      if (contype === 'CONSTR_FOREIGN') {
        const fk = identList(c.fk_attrs || []);
        if (fk.length === 1) {
          const col = colByName.get(fk[0]);
          if (col) {
            col.constraints ??= [];
            col.constraints.push({ Constraint: strip(c, 'fk_attrs') });
            continue;
          }
        }
        remaining.push(c);
        continue;
      }

      remaining.push(c);
    }

    create.tableElts = [
      ...cols.map((x) => ({ ColumnDef: x })),
      ...otherElts,
      ...remaining.map((x) => ({ Constraint: x })),
    ];
  }

  // 3) topo-sort tables by FK deps
  const createByName = new Map<string, CreateStmt>();
  const names: string[] = [];
  for (const c of creates) {
    const t = qname(c.relation!);
    names.push(t);
    createByName.set(t, c);
  }

  const schemaOf = (qn: string) => (qn.includes('.') ? qn.split('.')[0] : '');
  const localOf = (qn: string) =>
    qn.includes('.') ? qn.split('.').slice(1).join('.') : qn;

  const pos = new Map(names.map((n, i) => [n, i] as const));
  const schemaRank = new Map<string, number>();
  for (const n of names) {
    const s = schemaOf(n);
    if (!schemaRank.has(s)) schemaRank.set(s, schemaRank.size);
  }

  const cmp = (a: string, b: string) => {
    const sa = schemaRank.get(schemaOf(a)) ?? 1e9;
    const sb = schemaRank.get(schemaOf(b)) ?? 1e9;
    if (sa !== sb) return sa - sb;
    const la = localOf(a);
    const lb = localOf(b);
    if (la !== lb) return la.localeCompare(lb);
    return (pos.get(a) ?? 0) - (pos.get(b) ?? 0);
  };

  const fkDepsOf = (create: CreateStmt): Set<string> => {
    const deps = new Set<string>();
    const add = (cons: Constraint | undefined) => {
      if (!cons) return;
      if (String(cons.contype ?? '') !== 'CONSTR_FOREIGN') return;
      if (!cons.pktable) return;
      deps.add(qname(cons.pktable));
    };

    for (const e of create.tableElts ?? []) {
      if ('ColumnDef' in e) {
        for (const w of e.ColumnDef.constraints ?? [])
          if ('Constraint' in w) add(w.Constraint);
      } else if ('Constraint' in e) {
        add(e.Constraint);
      }
    }
    return deps;
  };

  const indeg = new Map<string, number>(names.map((n) => [n, 0]));
  const adj = new Map<string, Set<string>>(names.map((n) => [n, new Set()]));

  for (const t of names) {
    const deps = fkDepsOf(createByName.get(t)!);
    for (const d of deps) {
      if (d === t) continue;
      if (!createByName.has(d)) continue;
      if (!adj.get(d)!.has(t)) {
        adj.get(d)!.add(t);
        indeg.set(t, (indeg.get(t) ?? 0) + 1);
      }
    }
  }

  const pushSorted = (q: string[], x: string) => {
    let i = 0;
    while (i < q.length && cmp(q[i], x) <= 0) i++;
    q.splice(i, 0, x);
  };

  const queue: string[] = [];
  for (const n of names) if ((indeg.get(n) ?? 0) === 0) pushSorted(queue, n);

  const outNames: string[] = [];
  const seen = new Set<string>();

  while (queue.length) {
    const n = queue.shift()!;
    if (seen.has(n)) continue;
    seen.add(n);
    outNames.push(n);

    for (const m of adj.get(n) ?? []) {
      const k = (indeg.get(m) ?? 0) - 1;
      indeg.set(m, k);
      if (k === 0) pushSorted(queue, m);
    }
  }

  for (const n of names) if (!seen.has(n)) outNames.push(n);
  const sortedCreates = outNames.map((n) => createByName.get(n)!);

  // 4) interleave types (just before first use)
  const typeByFull = new Map<string, Node>();
  const shortToFull = new Map<string, string | null>();
  const unkeyedTypes: Node[] = [];

  const fullTypeNameOf = (stmt: Node): string | undefined => {
    if ('CreateEnumStmt' in stmt)
      return identList(stmt.CreateEnumStmt.typeName || []).join('.');
    if ('CreateDomainStmt' in stmt)
      return identList(stmt.CreateDomainStmt.domainname || []).join('.');
    if ('CreateRangeStmt' in stmt)
      return identList(stmt.CreateRangeStmt.typeName || []).join('.');
    if ('CompositeTypeStmt' in stmt) return qname(stmt.CompositeTypeStmt.typevar!);
    return undefined;
  };

  for (const s of typeNodes) {
    const full = fullTypeNameOf(s);
    if (!full) {
      unkeyedTypes.push(s);
      continue;
    }
    typeByFull.set(full, s);

    const short = full.split('.').at(-1)!;
    const prev = shortToFull.get(short);
    if (prev === undefined) shortToFull.set(short, full);
    else if (prev !== full) shortToFull.set(short, null);
  }

  const resolveTypeRef = (typeNameNode: TypeName): string | undefined => {
    const names = identList(typeNameNode.names || []);
    if (!names.length) return undefined;

    const full = names.join('.');
    if (typeByFull.has(full)) return full;

    const short = names.at(-1)!;
    const mapped = shortToFull.get(short);
    return mapped && typeByFull.has(mapped) ? mapped : undefined;
  };

  const typeDeps = new Map<string, Set<string>>();
  for (const [full, stmt] of typeByFull) {
    const deps = new Set<string>();

    if ('CreateDomainStmt' in stmt) {
      const base = resolveTypeRef(stmt.CreateDomainStmt.typeName!);
      if (base) deps.add(base);
    } else if ('CompositeTypeStmt' in stmt) {
      for (const n of stmt.CompositeTypeStmt.coldeflist ?? []) {
        if (!('ColumnDef' in n) || !n.ColumnDef.typeName) continue;
        const k = resolveTypeRef(n.ColumnDef.typeName);
        if (k) deps.add(k);
      }
    } else if ('CreateRangeStmt' in stmt) {
      for (const p of stmt.CreateRangeStmt.params ?? []) {
        if (!('DefElem' in p)) continue;
        const de = p.DefElem;
        if (!de?.arg || de.defname !== 'subtype') continue;
        const k = 'TypeName' in de.arg ? resolveTypeRef(de.arg.TypeName) : undefined;
        if (k) deps.add(k);
      }
    }

    typeDeps.set(full, deps);
  }

  const usedTypesInTable = (create: CreateStmt): string[] => {
    const used = new Set<string>();
    for (const e of create.tableElts ?? []) {
      if (!('ColumnDef' in e)) continue;
      const k = resolveTypeRef(e.ColumnDef.typeName!);
      if (k) used.add(k);
    }
    return [...used];
  };

  const emitted = new Set<string>();
  const visiting = new Set<string>();
  const outNodes: Node[] = [];

  const emitType = (full: string) => {
    if (emitted.has(full) || visiting.has(full)) return;
    const stmt = typeByFull.get(full);
    if (!stmt) return;

    visiting.add(full);
    for (const d of typeDeps.get(full) ?? []) emitType(d);
    visiting.delete(full);

    if (!emitted.has(full)) {
      outNodes.push(stmt);
      emitted.add(full);
    }
  };

  for (const create of sortedCreates) {
    for (const tname of usedTypesInTable(create)) emitType(tname);
    outNodes.push({ CreateStmt: create });
  }

  for (const t of [...typeByFull.keys()]) emitType(t);
  outNodes.push(...unkeyedTypes);

  // stmt_len: 1, so that deparse emits semicolons between statements
  const outSql = await deparse({
    version: ast.version,
    stmts: outNodes.map((stmt: Node): RawStmt => ({ stmt_len: 1, stmt })),
  });

  process.stdout.write(
    outSql
      .replaceAll(/[ \t]+$/gm, '')
      .replaceAll(/\n{3,}/g, '\n\n')
      .replaceAll(/\n?$/g, '\n'),
  );
})();

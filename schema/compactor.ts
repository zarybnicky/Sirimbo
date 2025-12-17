import process from 'node:process';
import { deparse } from 'pgsql-parser';
import type {
  ColumnDef,
  Constraint,
  CreateStmt,
  Node,
  RangeVar,
  RawStmt,
} from '@pgsql/types';

const qname = (rangeVar: RangeVar): string =>
  (rangeVar?.schemaname ? `${rangeVar.schemaname}.` : '') + rangeVar.relname;

const identList = (xs: unknown): string[] =>
  Array.isArray(xs)
    ? xs.map((n: any) => n?.String?.sval ?? n?.String?.str ?? n?.String?.val ?? String(n))
    : [];

const clone = <T>(x: T): T => JSON.parse(JSON.stringify(x));

(async () => {
  let input = '';
  for await (const chunk of process.stdin) input += chunk.toString();
  input = input.replace(/\\(un)?restrict [a-zA-Z0-9]+/g, '');

  const { parse } = await import('@pgsql/parser/v17');
  const ast = await parse(input);
  const stmts: RawStmt[] = ast.stmts ?? [];

  const keep: RawStmt[] = [];
  const creates: CreateStmt[] = [];
  const extraByTable = new Map<string, Constraint[]>();

  // 1) collect type/domain + create table + ALTER ADD CONSTRAINT
  for (const raw of stmts) {
    const stmt = raw.stmt!;

    if (
      'CreateDomainStmt' in stmt ||
      'CompositeTypeStmt' in stmt ||
      'CreateEnumStmt' in stmt ||
      'CreateRangeStmt' in stmt
    ) {
      keep.push(raw);
      continue;
    }

    if ('CreateStmt' in stmt) {
      creates.push(stmt.CreateStmt);
      continue;
    }

    if ('AlterTableStmt' in stmt) {
      const alt = stmt.AlterTableStmt;
      const t = qname(alt.relation!);

      for (const wrap of alt.cmds ?? []) {
        if (!('AlterTableCmd' in wrap)) continue;
        const cmd = wrap.AlterTableCmd;
        const subtype = String(cmd.subtype ?? '');

        if (subtype.includes('UsingIndex')) continue;
        if (subtype !== 'AT_AddConstraint' && subtype !== 'AT_AddConstraintRecurse')
          continue;

        if (cmd.def && 'Constraint' in cmd.def) {
          const cons = cmd.def.Constraint;
          const arr = extraByTable.get(t) ?? [];
          arr.push(cons);
          extraByTable.set(t, arr);
        }
      }
    }

    // ignore everything else (indexes, grants, comments, extensions, etc.)
  }

  // 2) compact each CREATE TABLE by merging single-col PK/UNIQUE/FK into ColumnDef
  for (const create of creates) {
    const t = qname(create.relation!);

    const cols: ColumnDef[] = [];
    const other: Node[] = [];
    const tableCons: Constraint[] = [];

    const colMap = new Map<string, ColumnDef>();

    for (const e of create.tableElts ?? []) {
      if ('ColumnDef' in e) {
        cols.push(e.ColumnDef);
        colMap.set(String(e.ColumnDef.colname), e.ColumnDef);
      } else if ('Constraint' in e) {
        tableCons.push(e.Constraint);
      } else {
        other.push(e);
      }
    }

    const allCons = [...tableCons, ...(extraByTable.get(t) ?? [])];
    const remaining: Constraint[] = [];

    for (const c0 of allCons) {
      const c = clone(c0);
      delete c.conname;

      const contype = String(c.contype ?? '');

      if (contype === 'CONSTR_PRIMARY' || contype === 'CONSTR_UNIQUE') {
        const keys = identList(c.keys);
        if (keys.length === 1 && colMap.has(keys[0])) {
          const col = colMap.get(keys[0])!;
          col.constraints ??= [];
          const colC = clone(c);
          delete colC.keys;
          col.constraints.push({ Constraint: colC });
        } else {
          remaining.push(c);
        }
        continue;
      }

      if (contype === 'CONSTR_FOREIGN') {
        const fk = identList(c.fk_attrs);
        if (fk.length === 1 && colMap.has(fk[0])) {
          const col = colMap.get(fk[0])!;
          col.constraints ??= [];
          const colC = clone(c);
          delete colC.fk_attrs;
          col.constraints.push({ Constraint: colC });
        } else {
          remaining.push(c);
        }
        continue;
      }

      remaining.push(c); // CHECK / others stay table-level
    }

    create.tableElts = [
      ...cols.map((x) => ({ ColumnDef: x })),
      ...other,
      ...remaining.map((c) => ({ Constraint: c })),
    ];
  }
  // 3) topo-sort CREATE TABLEs by FK dependencies (referenced table first)
  const createByName = new Map<string, CreateStmt>();
  const names: string[] = [];
  for (const create of creates) {
    const t = qname(create.relation!);
    names.push(t);
    createByName.set(t, create);
  }

  const pos = new Map(names.map((n, i) => [n, i] as const));
  const indeg = new Map<string, number>(names.map((n) => [n, 0]));
  const adj = new Map<string, Set<string>>(names.map((n) => [n, new Set()]));

  for (const t of names) {
    const create = createByName.get(t)!;
    const deps = new Set<string>();

    for (const e of create.tableElts ?? []) {
      // column constraints
      if ('ColumnDef' in e) {
        const c = e.ColumnDef;
        for (const w of c.constraints ?? []) {
          if (!('Constraint' in w)) continue;
          const cons = w.Constraint;
          if (String(cons.contype ?? '') !== 'CONSTR_FOREIGN') continue;
          if (!cons.pktable) continue;
          deps.add(qname(cons.pktable));
        }
        continue;
      }
      // table constraints
      if ('Constraint' in e) {
        const cons = e.Constraint;
        if (String(cons.contype ?? '') !== 'CONSTR_FOREIGN') continue;
        if (!cons.pktable) continue;
        deps.add(qname(cons.pktable));
      }
    }

    for (const d of deps) {
      if (d === t) continue;
      if (!createByName.has(d)) continue; // ignore external refs
      if (!adj.get(d)!.has(t)) {
        adj.get(d)!.add(t);
        indeg.set(t, (indeg.get(t) ?? 0) + 1);
      }
    }
  }

  const queue = names.filter((n) => (indeg.get(n) ?? 0) === 0);
  const outNames: string[] = [];
  const seen = new Set<string>();

  queue.sort((a, b) => pos.get(a)! - pos.get(b)!);

  while (queue.length) {
    const n = queue.shift()!;
    if (seen.has(n)) continue;
    seen.add(n);
    outNames.push(n);

    for (const m of adj.get(n) ?? []) {
      const k = (indeg.get(m) ?? 0) - 1;
      indeg.set(m, k);
      if (k === 0) {
        queue.push(m);
        queue.sort((a, b) => pos.get(a)! - pos.get(b)!);
      }
    }
  }

  // cycles / leftovers: keep original relative order
  for (const n of names) if (!seen.has(n)) outNames.push(n);

  const sortedCreates = outNames.map((n) => createByName.get(n)!);

  // 4) interleave types into the topo-ordered table stream (emit just before first use)
  // Build: type name -> RawStmt (and stable order + short-name resolution)
  const typeByFull = new Map<string, RawStmt>();
  const shortToFull = new Map<string, string | null>(); // null = ambiguous
  const typePos = new Map<string, number>();
  const unkeyedTypes: RawStmt[] = [];

  const fullTypeNameOf = (raw: RawStmt): string | undefined => {
    const stmt = raw.stmt!;
    if ('CreateEnumStmt' in stmt)
      return identList(stmt.CreateEnumStmt.typeName).join('.');
    if ('CreateDomainStmt' in stmt)
      return identList(stmt.CreateDomainStmt.domainname).join('.');
    if ('CreateRangeStmt' in stmt)
      return identList(stmt.CreateRangeStmt.typeName).join('.');
    if ('CompositeTypeStmt' in stmt) return qname(stmt.CompositeTypeStmt.typevar);
    return undefined;
  };

  // Register types from `keep` (your collected enum/domain/composite/range statements)
  for (const raw of keep) {
    const full = fullTypeNameOf(raw);
    if (!full) {
      unkeyedTypes.push(raw);
      continue;
    }
    typeByFull.set(full, raw);
    typePos.set(full, typePos.size);

    const short = full.split('.').at(-1)!;
    const prev = shortToFull.get(short);
    if (prev === undefined) shortToFull.set(short, full);
    else if (prev !== full) shortToFull.set(short, null);
  }

  const resolveTypeRef = (typeNameNode: any): string | undefined => {
    const tn =
      typeNameNode && typeof typeNameNode === 'object' && 'TypeName' in typeNameNode
        ? typeNameNode.TypeName
        : typeNameNode;

    const names = identList(tn?.names);
    if (!names.length) return undefined;

    const full = names.join('.');
    if (typeByFull.has(full)) return full;

    const short = names.at(-1)!;
    const mapped = shortToFull.get(short);
    if (mapped && typeByFull.has(mapped)) return mapped;

    return undefined;
  };

  // Type deps: domain -> base type, composite -> attr types, range -> subtype
  const typeDeps = new Map<string, Set<string>>();
  for (const [full, raw] of typeByFull) {
    const deps = new Set<string>();
    const stmt = raw.stmt!;

    if ('CreateDomainStmt' in stmt) {
      const base = resolveTypeRef((stmt as any).CreateDomainStmt.typeName);
      if (base) deps.add(base);
    } else if ('CompositeTypeStmt' in stmt) {
      for (const n of (stmt as any).CompositeTypeStmt.coldeflist ?? []) {
        if ('ColumnDef' in n) {
          const k = resolveTypeRef((n as any).ColumnDef.typeName);
          if (k) deps.add(k);
        }
      }
    } else if ('CreateRangeStmt' in stmt) {
      // best-effort: find DefElem(defname='subtype') with arg.TypeName
      for (const p of (stmt as any).CreateRangeStmt.params ?? []) {
        const de = p && 'DefElem' in p ? (p as any).DefElem : undefined;
        if (!de || de.defname !== 'subtype') continue;
        const k = resolveTypeRef(de.arg);
        if (k) deps.add(k);
      }
    }

    typeDeps.set(full, deps);
  }

  const usedTypesInTable = (create: CreateStmt): string[] => {
    const used = new Set<string>();
    for (const e of create.tableElts ?? []) {
      if ('ColumnDef' in e) {
        const k = resolveTypeRef(e.ColumnDef.typeName as any);
        if (k) used.add(k);
      }
    }
    // stable order (as in dump)
    return [...used].sort((a, b) => typePos.get(a)! - typePos.get(b)!);
  };

  const emitted = new Set<string>();
  const visiting = new Set<string>();
  const outStmts: RawStmt[] = [];

  const emitType = (full: string) => {
    if (emitted.has(full)) return;
    if (visiting.has(full)) return; // break cycles
    const raw = typeByFull.get(full);
    if (!raw) return;

    visiting.add(full);
    for (const d of typeDeps.get(full) ?? []) emitType(d);
    visiting.delete(full);

    if (!emitted.has(full)) {
      outStmts.push(raw);
      emitted.add(full);
    }
  };

  // Interleave: types (first use) + table
  for (const create of sortedCreates) {
    for (const tname of usedTypesInTable(create)) emitType(tname);
    outStmts.push({ stmt_len: 1, stmt: { CreateStmt: create } });
  }

  // Unused types (and any unkeyed ones) at the end
  const allTypes = [...typeByFull.keys()].sort(
    (a, b) => typePos.get(a)! - typePos.get(b)!,
  );
  for (const t of allTypes) emitType(t);
  outStmts.push(...unkeyedTypes);

  const outSql = await deparse({
    version: ast.version,
    stmts: outStmts,
  });

  process.stdout.write(
    outSql
      .replace(/[ \t]+$/gm, '')
      .replace(/\n{3,}/g, '\n\n')
      .replace(/\n?$/, '\n'),
  );
})();

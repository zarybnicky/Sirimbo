import process from 'node:process';
import { deparse } from 'pgsql-parser';
import type { Node, RangeVar, RawStmt, Constraint, ColumnDef } from '@pgsql/types';

const qname = (rangeVar: RangeVar): string =>
  (rangeVar?.schemaname ? `${rangeVar.schemaname}.` : '') + rangeVar.relname;

const identList = (xs: unknown): string[] =>
  Array.isArray(xs)
    ? xs.map((n: any) => n?.String?.sval ?? n?.String?.str ?? n?.String?.val ?? String(n))
    : [];

const clone = <T>(x: T): T => JSON.parse(JSON.stringify(x));

(async () => {
  let input = "";
  for await (const chunk of process.stdin)
    input += chunk.toString();
  input = input.replace(/\\(un)?restrict [a-zA-Z0-9]+/g, '');

  const { parse } = await import('@pgsql/parser/v17');
  const ast = await parse(input);
  const stmts = ast.stmts ?? [];

  const keep: RawStmt[] = [];
  const creates: RawStmt[] = [];
  const extraByTable = new Map<string, Constraint[]>();

  // 1) collect type/domain + create table + ALTER ADD CONSTRAINT
  for (const raw of stmts) {
    const stmt = raw.stmt!;
    const tag = Object.keys(stmt)[0];

    if (
      tag === 'CreateDomainStmt' ||
      tag === 'CompositeTypeStmt' ||
      tag === 'CreateEnumStmt' ||
      tag === 'CreateRangeStmt'
    ) {
      keep.push(raw);
      continue;
    }

    if (tag === 'CreateStmt') {
      creates.push(raw);
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
        if (subtype !== 'AT_AddConstraint' && subtype !== 'AT_AddConstraintRecurse') continue;

        if (cmd.def && 'Constraint' in cmd.def) {
          const cons = cmd.def?.Constraint;
          const arr = extraByTable.get(t) ?? [];
          arr.push(cons);
          extraByTable.set(t, arr);
        }
      }
    }

    // ignore everything else (indexes, grants, comments, extensions, etc.)
  }

  // 2) compact each CREATE TABLE by merging single-col PK/UNIQUE/FK into ColumnDef
  for (const raw of creates) {
    const stmt = raw.stmt!;
    if (!('CreateStmt' in stmt)) continue;
    const create = stmt.CreateStmt;
    const t = qname(create.relation!);

    const elts: Node[] = create.tableElts ?? [];
    const cols: ColumnDef[] = [];
    const other: Node[] = [];
    const tableCons: Constraint[] = [];

    const colMap = new Map<string, ColumnDef>();

    for (const e of elts) {
      if ('ColumnDef' in e) {
        const { ColumnDef: c } = e
        cols.push(c);
        colMap.set(String(c.colname), c);
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
      ...cols.map(x => ({ ColumnDef: x })),
      ...other,
      ...remaining.map((c) => ({ Constraint: c })),
    ];
  }

  const outSql = await deparse({
    version: ast.version,
    stmts: [...keep, ...creates],
  });

  process.stdout.write(
    outSql
      .replace(/[ \t]+$/gm, '')
      .replace(/\n{3,}/g, '\n\n')
      .replace(/\n?$/, '\n')
  );
})();

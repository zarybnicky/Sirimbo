export function formatException(e: any) {
  if (!(e instanceof Error)) return String(e);

  const base = e.stack || e.message;

  // pg errors have a `code` (SQLSTATE) and other diagnostic fields
  const pgFields = ['code', 'detail', 'hint', 'where', 'schema', 'table',
                    'column', 'constraint', 'routine', 'severity', 'position',
                    'internalQuery'];

  const extras: Record<string, string> = {};
  for (const f of pgFields) {
    if (f in e && (e as any)[f] !== undefined) extras[f] = (e as any)[f];
  }

  if (Object.keys(extras).length === 0) return base;
  return `${base}\n${JSON.stringify(extras, null, 2)}`;
}

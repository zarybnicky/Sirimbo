import { mapExchange } from "urql";

const reconstructForTracing = (target: unknown, name?: string): any => {
  let reconstructed: any = undefined;
  let traceChildren: any = undefined;
  if (Array.isArray(target)) {
    traceChildren = [];
    reconstructed = [];
    target.forEach((x, i) => {
      const [t1, x1] = reconstructForTracing(x, i.toString());
      traceChildren.push(t1);
      reconstructed.push(x1);
    });
  } else if (typeof target === 'object' && target) {
    traceChildren = {};
    reconstructed = {};
    for (const [k, v] of Object.entries(target)) {
      const [t1, x1] = reconstructForTracing(v, k);
      traceChildren[k] = t1;
      reconstructed[k] = x1;
    }
  }

  const trace: { name?: string; visited: boolean; children?: any; } = { name, visited: false };
  if (traceChildren)
    trace.children = traceChildren;
  const proxy = !reconstructed ? target : new Proxy(reconstructed, {
    get(target, p) {
      if (p in trace.children)
        if (!(p === 'length' && Array.isArray(trace.children)))
          trace.children[p].visited = true;
      return target[p];
    }
  });
  if (!name) {
    return [traceChildren, proxy];
  }
  return [trace, proxy];
};

(globalThis as any).traceRegistry = [];
(globalThis as any).reportTrace = () => {
  for (const { query, variables, trace } of (globalThis as any).traceRegistry) {
    let emittedHeader = false;
    const reportUnused = (obj: any, path: string[]) => {
      let emptyChildren = true;
      for (const _key in obj.children ?? {})
        emptyChildren = false;

      if (['id', '__typename'].includes(obj.name) && emptyChildren) return;

      const fieldPath = path.concat(obj.name);
      if (!obj.visited) {
        if (!emittedHeader) {
          console.log(`Unused parts of ${query} (${JSON.stringify(variables)})`);
          emittedHeader = true;
        }
        console.log(`Unused field ${fieldPath.join('.')}${emptyChildren ? '' : '.*'}`);
      } else {
        for (const key in obj.children ?? {}) {
          reportUnused(obj.children[key], fieldPath);
        }
      }
    };

    for (const key in trace ?? {})
      reportUnused(trace[key], []);
  }
};

export const tracingExchange = mapExchange({
  onResult({ data, ...rest }) {
    if (rest.operation.kind !== 'query') return { data, ...rest};
    const [trace, proxy] = reconstructForTracing(data);
    (globalThis as any).traceRegistry.push({
      query: rest.operation.query.loc?.source.body.split('\nfragment')[0],
      variables: rest.operation.variables,
      trace,
    });
    return { data: proxy, ...rest };
  },
});

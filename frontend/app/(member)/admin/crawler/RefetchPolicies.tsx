import { scaleSymlog } from 'd3';
import { Fragment } from 'react';
import Link from 'next/link';
import type { IGetRefetchPoliciesResult } from './crawler.queries';
import { RefetchPolicyEditor } from './RefetchPolicyEditor';

function intervalLabel(interval: string | null | undefined) {
  if (!interval) return 'None';
  return interval
    .replaceAll(/\bmons?\b/g, (unit) => (unit === 'mon' ? 'month' : 'months'))
    .replaceAll(/(\d+):(\d+):(\d+)/g, (_, hours, minutes, seconds) =>
      [
        Number(hours) ? `${Number(hours)} h` : '',
        Number(minutes) ? `${Number(minutes)} min` : '',
        Number(seconds) ? `${Number(seconds)} s` : '',
      ]
        .filter(Boolean)
        .join(' '),
    );
}

export function RefetchPolicies({ policies }: { policies: IGetRefetchPoliciesResult[] }) {
  const rows = Map.groupBy(policies, (policy) => `${policy.federation}:${policy.kind}`);
  const groups = Map.groupBy([...rows], ([, xs]) =>
    xs.some((x) => x.offsetDays !== null),
  );
  const extent =
    Math.max(365, ...policies.map((policy) => Math.abs(policy.offsetDays ?? 0))) * 4;
  const x = scaleSymlog().domain([-extent, extent]).range([0, 100]);
  const ticks = [-365, -30, -7, -1, 0, 1, 7, 30, 365];
  const referenceDay = { left: `${x(0)}%`, width: `${x(1) - x(0)}%` };

  return (
    <section className="mt-8">
      <h2 className="text-lg font-bold">Refetch policies</h2>
      <h3 className="mt-4 font-semibold">Date-based policies</h3>
      <div className="overflow-x-auto">
        <div className="grid min-w-250 grid-cols-[240px_80px_minmax(0,1fr)] text-xs">
          <div className="py-2 font-semibold">Kind</div>
          <div className="py-2 font-semibold">Default</div>
          <div className="relative h-16 text-neutral-11">
            <span
              className="absolute inset-y-0 border-x border-accent-6 bg-accent-3/50"
              style={referenceDay}
            />
            <div className="absolute top-1 text-center" style={referenceDay}>
              <span className="absolute left-1/2 -translate-x-1/2 whitespace-nowrap">
                Reference day
                <br />
                00:00–24:00 UTC
              </span>
            </div>
            {ticks.map((tick) => (
              <span
                key={tick}
                className="absolute bottom-2 -translate-x-1/2 whitespace-nowrap"
                style={{ left: `${x(tick)}%` }}
              >
                {tick === 0 ? '0' : `${tick > 0 ? '+' : '−'}${Math.abs(tick)}d`}
              </span>
            ))}
          </div>
          {groups.get(true)?.map(([kind, rules]) => {
            const fallback = rules.find((rule) => rule.offsetDays === null);
            const boundaries = rules.filter((rule) => rule.offsetDays !== null);
            const segments = [
              { rule: fallback, start: -extent },
              ...boundaries.map((rule) => ({ rule, start: rule.offsetDays! })),
            ];
            return (
              <Fragment key={kind}>
                <Link
                  href={`/admin/crawler/${kind}`}
                  prefetch={false}
                  className="flex items-center text-accent-11 hover:underline"
                >
                  {kind}
                </Link>
                <div
                  className="flex items-center text-neutral-11"
                  title={fallback?.fetchInterval ?? 'No automatic refetch'}
                >
                  <RefetchPolicyEditor
                    rule={fallback}
                    field="fetchInterval"
                    className="underline decoration-dotted underline-offset-2 hover:text-accent-11"
                  >
                    {intervalLabel(fallback?.fetchInterval)}
                  </RefetchPolicyEditor>
                </div>
                <div className="relative h-12">
                  <span
                    className="absolute inset-y-0 border-x border-accent-6 bg-accent-3/50"
                    style={referenceDay}
                  />
                  {segments.map(({ rule, start }, index) => (
                    <div
                      key={start}
                      className="absolute inset-y-0 flex items-center justify-center"
                      style={{
                        left: `${x(start)}%`,
                        width: `${x(segments[index + 1]?.start ?? extent) - x(start)}%`,
                      }}
                    >
                      <span className="absolute inset-x-0 top-1/2 border-t-2 border-accent-7" />
                      {index > 0 && (
                        <RefetchPolicyEditor
                          rule={rule}
                          field="offset"
                          className="absolute top-1/2 left-0 z-10 flex size-5 -translate-1/2 items-center justify-center rounded-full hover:bg-accent-4"
                        >
                          <span className="size-2 rounded-full bg-accent-9" />
                        </RefetchPolicyEditor>
                      )}
                      <RefetchPolicyEditor
                        rule={rule}
                        field="fetchInterval"
                        className="relative bg-neutral-1 px-1 whitespace-nowrap text-neutral-12 underline decoration-dotted underline-offset-2 hover:text-accent-11"
                      >
                        {intervalLabel(rule?.fetchInterval)}
                      </RefetchPolicyEditor>
                    </div>
                  ))}
                </div>
              </Fragment>
            );
          })}
        </div>
      </div>
      <h3 className="mt-6 mb-3 font-semibold">Default-only policies</h3>
      <div className="grid grid-cols-[minmax(0,240px)_1fr] gap-y-3 text-xs">
        {groups.get(false)?.map(([kind, rules]) => (
          <Fragment key={kind}>
            <Link
              href={`/admin/crawler/${kind}`}
              prefetch={false}
              className="pr-3 text-accent-11 hover:underline"
            >
              {kind}
            </Link>
            <span className="text-neutral-11">
              <RefetchPolicyEditor
                rule={rules[0]}
                field="fetchInterval"
                className="underline decoration-dotted underline-offset-2 hover:text-accent-11"
              >
                {intervalLabel(rules[0]?.fetchInterval)}
              </RefetchPolicyEditor>
            </span>
          </Fragment>
        ))}
      </div>
    </section>
  );
}

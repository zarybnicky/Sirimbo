import { scaleSymlog } from 'd3';
import { Fragment } from 'react';
import Link from 'next/link';
import type { IGetRefetchPoliciesResult } from './crawler.queries';
import { RefetchPolicyEditor } from './RefetchPolicyEditor';
import { numberFormatter } from '@/ui/format';
import { Plus } from 'lucide-react';

function intervalLabel(interval: string | null | undefined) {
  if (!interval) return 'None';
  return interval
    .replaceAll(
      /\b(\d+) mons?\b/g,
      (_, amount) => `${amount} ${Number(amount) === 1 ? 'month' : 'months'}`,
    )
    .replaceAll(/\b1 (days|years)\b/g, (_, unit) => `1 ${unit.slice(0, -1)}`)
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
  const extent =
    Math.max(365, ...policies.map((policy) => Math.abs(policy.offsetDays ?? 0))) * 4;
  const x = scaleSymlog().domain([-extent, extent]).range([0, 100]);
  const ticks = [-365, -30, -7, -1, 0, 1, 7, 30, 365];
  const referenceDay = { left: `${x(0)}%`, width: `${x(1) - x(0)}%` };

  return (
    <section className="mt-8">
      <h2 className="text-lg font-bold">Refetch policies</h2>
      <div className="overflow-x-auto">
        <div className="grid min-w-250 grid-cols-[240px_minmax(0,1fr)] text-xs">
          <div className="py-2 font-semibold">Kind</div>
          <div className="relative mr-4 h-16 text-neutral-11">
            <span
              className="absolute inset-y-0 border-r border-l-[3px] border-accent-6 bg-accent-3/50"
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
          {[...rows].map(([kind, rules]) => {
            const ordered = rules.toSorted(
              (a, b) =>
                Math.abs(b.offsetDays ?? Infinity) - Math.abs(a.offsetDays ?? Infinity),
            );
            const before = ordered.filter(
              (rule) => rule.offsetDays !== null && rule.offsetDays < 0,
            );
            const after = ordered.filter(
              (rule) => rule.offsetDays !== null && rule.offsetDays >= 0,
            );
            const fallback = ordered.find((rule) => rule.offsetDays === null);
            const levels = Math.max(before.length, after.length);
            const stacked = [before, after].flatMap((side) =>
              side.map((rule, index) => ({ rule, level: levels - index - 1 })),
            );
            if (fallback) stacked.push({ rule: fallback, level: levels });
            return (
              <Fragment key={kind}>
                <div className="flex flex-col items-start justify-center pb-1">
                  <div className="flex items-center gap-1">
                    <RefetchPolicyEditor
                      rule={rules[0]}
                      create
                      className="flex size-4 items-center justify-center rounded-sm text-neutral-11 hover:text-accent-11"
                    >
                      <Plus className="size-3" />
                    </RefetchPolicyEditor>
                    <Link
                      href={`/admin/crawler/${kind}`}
                      prefetch={false}
                      className="text-accent-11 hover:underline"
                    >
                      {kind}
                    </Link>
                  </div>
                  {rules[0]!.frontierCount !== 1 && (
                    <div className="pl-5 tabular-nums text-neutral-11">
                      {numberFormatter.format(rules[0]!.frontierCount)}
                    </div>
                  )}
                </div>
                <div
                  className="relative mr-4 self-center"
                  style={{ height: (levels + (fallback ? 1 : 0)) * 20 + 4 }}
                >
                  <span
                    className="absolute top-0 bottom-1 border-r border-l-[3px] border-accent-6 bg-accent-3/50"
                    style={referenceDay}
                  />
                  {stacked.map(({ rule, level }) => {
                    const end = x(rule.offsetDays ?? extent);
                    return (
                      <div
                        key={rule.offsetLabel ?? 'default'}
                        className="pointer-events-none absolute inset-x-0 h-5"
                        style={{ top: level * 20 }}
                      >
                        <span
                          className="absolute top-1/2 border-t-2 border-accent-7"
                          style={{
                            left: `${rule.offsetDays === null ? 0 : Math.min(x(0), end)}%`,
                            width: `${rule.offsetDays === null ? 100 : Math.abs(end - x(0))}%`,
                          }}
                        />
                        {rule.offsetDays === null ? (
                          <>
                            <span className="absolute top-1/2 left-0 -translate-y-1/2 bg-neutral-1 pr-1 text-neutral-11">
                              −∞
                            </span>
                            <RefetchPolicyEditor
                              rule={rule}
                              className="pointer-events-auto absolute top-1/2 left-1/2 -translate-1/2 bg-neutral-1 px-1"
                            >
                              {intervalLabel(rule.fetchInterval)}
                            </RefetchPolicyEditor>
                            <span className="absolute top-1/2 right-0 -translate-y-1/2 bg-neutral-1 pl-1 text-neutral-11">
                              +∞
                            </span>
                          </>
                        ) : (
                          <span
                            className="pointer-events-auto absolute top-1/2"
                            style={{ left: `${end}%` }}
                          >
                            <span className="absolute size-2 -translate-1/2 rounded-full bg-accent-9" />
                            <RefetchPolicyEditor
                              rule={rule}
                              className={`absolute -translate-y-1/2 ${rule.offsetDays < 0 ? 'right-full mr-3' : 'left-full ml-3'}`}
                            >
                              {intervalLabel(rule.fetchInterval)}
                            </RefetchPolicyEditor>
                          </span>
                        )}
                      </div>
                    );
                  })}
                </div>
              </Fragment>
            );
          })}
        </div>
      </div>
    </section>
  );
}

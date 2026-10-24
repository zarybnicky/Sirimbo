'use client';

import { DotSeparated } from '@/ui/DotSeparated';

import { DefinitionList } from '@/ui/DefinitionList';
import React from 'react';
import Link from 'next/link';
import { useQuery } from 'urql';
import { PageHeader } from '@/ui/PageHeader';
import { computeRange, type PeriodPreset, PeriodSelector } from '@/ui/PeriodSelector';
import { ScoreboardDocument } from '@/graphql/Scoreboard';
import { Combobox } from '@/ui/fields/Combobox';
import { fullDateFormatter, numberFormatter } from '@/ui/format';

export function Scoreboard() {
  const [cohortId, setCohortId] = React.useState<string | null | undefined>(null);
  const [preset, setPreset] = React.useState<PeriodPreset>('schoolyear');
  const [date, setDate] = React.useState(() => new Date());
  const [range, setRange] = React.useState<{
    since: Date | null;
    until: Date | null;
  }>({ since: null, until: null });

  const period = React.useMemo(
    () => computeRange(preset, date, range.since, range.until),
    [preset, date, range],
  );

  const [{ data, fetching, error }] = useQuery({
    query: ScoreboardDocument,
    variables: {
      cohortId: cohortId,
      since: period.since,
      until: period.until,
    },
    pause: !period.since || !period.until,
  });

  const cohorts = data?.cohortsList ?? [];
  const scoreboard = data?.scoreboardEntriesList ?? [];
  const activeCohort = cohortId
    ? (cohorts.find((item) => item.id === cohortId) ?? null)
    : null;

  return (
    <>
      <PageHeader title="Žebříček aktivity" />

      <div className="space-y-4">
        <section>
          <p>Skóre se skládá z:</p>
          <DefinitionList
            items={{
              'Individuální lekce': '1b, max. 4b/týden',
              'Pohybovka, performance': '1b',
              'Vedená hodina': '1b',
              Practice: '2b',
              'Jednodenní klubová akce': '3b',
              'Vícedenní klubová akce': '5b',
              'Účast na soutěži': <s>2b</s>,
              'Sportovní aktivita': <s>1b</s>,
            }}
          />
        </section>

        <section className="not-prose rounded-lg border border-neutral-6 p-4 space-y-4">
          <div className="flex gap-4 flex-wrap">
            <Combobox
              label="Skupina"
              value={cohortId}
              onChange={setCohortId}
              options={[
                { id: '', label: 'Všechny skupiny' },
                ...cohorts.map((cohort) => ({
                  id: cohort.id,
                  label: cohort.name,
                })),
              ]}
              placeholder="Všechny skupiny"
            />

            <PeriodSelector
              preset={preset}
              onPresetChange={setPreset}
              date={date}
              onDateChange={setDate}
              since={range.since}
              onSinceChange={(since) => setRange((x) => ({ ...x, since }))}
              until={range.until}
              onUntilChange={(until) => setRange((x) => ({ ...x, until }))}
            />
          </div>

          {period.displaySince && period.displayUntil && (
            <DotSeparated className="text-sm text-neutral-10">
              <span>
                Zobrazené období:{' '}
                <span className="font-medium text-neutral-12">
                  {fullDateFormatter.formatRange(
                    period.displaySince,
                    period.displayUntil,
                  )}
                </span>
              </span>
              {activeCohort ? `Skupina ${activeCohort.name}` : 'Všechny skupiny'}
            </DotSeparated>
          )}
        </section>

        <section>
          {error ? (
            <p className="not-prose rounded-md border border-accent-7 bg-accent-3 p-3 text-sm text-accent-11">
              Nepodařilo se načíst žebříček. Zkuste to prosím znovu.
            </p>
          ) : fetching ? (
            <p>Načítám výsledky…</p>
          ) : scoreboard.length === 0 ? (
            <p>Pro vybrané období nejsou žádné body.</p>
          ) : null}

          <div className="overflow-x-auto">
            <table className="min-w-full text-sm">
              <thead>
                <tr className="border-b border-neutral-6 text-left">
                  <th className="py-2 pr-2" />
                  <th className="py-2 pr-4">Člen</th>
                  <th className="p-2 text-center">Lekce</th>
                  <th className="p-2 text-center">Společné</th>
                  <th className="p-2 text-center">Akce</th>
                  <th className="py-2 pl-2 text-center">Celkem</th>
                </tr>
              </thead>
              <tbody>
                {scoreboard.map((entry) => {
                  const key = `${entry.personId ?? 'unknown'}-${entry.cohortId ?? 'all'}`;
                  const rank = entry.ranking ? Number(entry.ranking) : undefined;

                  const highlightClass =
                    rank === 1
                      ? 'bg-[rgb(254,240,138)] text-black'
                      : rank === 2
                        ? 'bg-[rgb(186,230,253)] text-black'
                        : rank === 3
                          ? 'bg-[rgb(254,215,170)] text-black'
                          : '';

                  return (
                    <tr key={key} className={highlightClass}>
                      <td className="py-1 pr-2 font-bold">
                        {entry.ranking
                          ? `${numberFormatter.format(BigInt(entry.ranking))}.`
                          : '–'}
                      </td>
                      <td className="py-1 pr-4">
                        {entry.person ? (
                          <Link
                            href={`/clenove/${entry.person.id}`}
                            className="hover:underline text-inherit"
                          >
                            {entry.person.name}
                          </Link>
                        ) : (
                          '-'
                        )}
                      </td>
                      <td className="py-1 px-2 text-center">
                        {numberFormatter.format(BigInt(entry.lessonTotalScore ?? '0'))}
                      </td>
                      <td className="py-1 px-2 text-center">
                        {numberFormatter.format(BigInt(entry.groupTotalScore ?? '0'))}
                      </td>
                      <td className="py-1 px-2 text-center">
                        {numberFormatter.format(BigInt(entry.eventTotalScore ?? '0'))}
                      </td>
                      <td className="py-1 pl-2 text-center font-bold">
                        {numberFormatter.format(BigInt(entry.totalScore ?? '0'))}
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </section>
      </div>
    </>
  );
}

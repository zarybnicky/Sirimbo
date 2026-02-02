import React from 'react';
import Link from 'next/link';
import { useQuery } from 'urql';
import { Layout } from '@/ui/Layout';
import { TitleBar } from '@/ui/TitleBar';
import { ScoreboardPeriodSelector } from '@/scoreboard/ScoreboardPeriodSelector';
import { computeRange, PeriodPreset } from '@/scoreboard/periods';
import { ScoreboardDocument } from '@/graphql/Scoreboard';
import { Combobox } from '@/ui/fields/Combobox';
import { fullDateFormatter } from '@/ui/format';

export default function ScoreboardPage() {
  const [selectedCohortId, setSelectedCohortId] = React.useState<
    string | null | undefined
  >(null);
  const [preset, setPreset] = React.useState<PeriodPreset>('schoolyear');
  const [referenceDate, setReferenceDate] = React.useState(() => new Date());
  const [customRange, setCustomRange] = React.useState<{
    since: Date | null;
    until: Date | null;
  }>({ since: null, until: null });

  const period = React.useMemo(
    () => computeRange(preset, referenceDate, customRange.since, customRange.until),
    [preset, referenceDate, customRange],
  );

  const [{ data, fetching, error }] = useQuery({
    query: ScoreboardDocument,
    variables: {
      cohortId: selectedCohortId,
      since: period.since,
      until: period.until,
    },
    pause: !period.since || !period.until,
  });

  const cohorts = data?.cohortsList ?? [];
  const scoreboard = data?.scoreboardEntriesList ?? [];
  const activeCohort = selectedCohortId
    ? (cohorts.find((item) => item.id === selectedCohortId) ?? null)
    : null;

  return (
    <Layout requireMember>
      <TitleBar title="Žebříček aktivity" />

      <div className="prose prose-accent space-y-8">
        <section>
          <p>Skóre se skládá z:</p>
          <dl className="not-prose text-sm mt-2 grid gap-y-1">
            <div>
              <dt>Individuální lekce</dt>
              <dd>1b, max. 4b za týden</dd>
            </div>
            <div>
              <dt>Pohybovka, performance</dt>
              <dd>1b</dd>
            </div>
            <div>
              <dt>Vedená hodina</dt>
              <dd>1b</dd>
            </div>
            <div>
              <dt>Practice</dt>
              <dd>2b</dd>
            </div>
            <div>
              <dt>Jednodenní klubová akce</dt>
              <dd>3b</dd>
            </div>
            <div>
              <dt>Vícedenní klubová akce</dt>
              <dd>5b</dd>
            </div>
            <div>
              <dt>Účast na soutěži</dt>
              <dd>
                <s>2b</s>
              </dd>
            </div>
            <div>
              <dt>Sportovní aktivita</dt>
              <dd>
                <s>1b</s>
              </dd>
            </div>
          </dl>
        </section>

        <section className="not-prose rounded-lg border border-neutral-6 p-4 space-y-4">
          <div className="flex gap-4 flex-wrap">
            <Combobox
              label="Skupina"
              value={selectedCohortId}
              onChange={setSelectedCohortId}
              options={[
                { id: '', label: 'Všechny skupiny' },
                ...cohorts.map((cohort) => ({
                  id: cohort.id,
                  label: cohort.name,
                })),
              ]}
              placeholder="Všechny skupiny"
            />

            <ScoreboardPeriodSelector
              preset={preset}
              onPresetChange={setPreset}
              referenceDate={referenceDate}
              onReferenceDateChange={setReferenceDate}
              customSince={customRange.since}
              onCustomSinceChange={(value) =>
                setCustomRange((prev) => ({ ...prev, since: value }))
              }
              customUntil={customRange.until}
              onCustomUntilChange={(value) =>
                setCustomRange((prev) => ({ ...prev, until: value }))
              }
              showCustomRangeWarning={
                preset === 'custom' && (!period.since || !period.until)
              }
            />
          </div>

          {period.displaySince && period.displayUntil ? (
            <p className="text-sm text-neutral-10">
              {'Zobrazené období: '}
              <span className="font-medium text-neutral-12">
                {fullDateFormatter.formatRange(period.displaySince, period.displayUntil)}
              </span>
              {activeCohort ? ` · Skupina ${activeCohort.name}` : ' · Všechny skupiny'}
            </p>
          ) : null}
        </section>

        <section>
          {error ? (
            <p className="not-prose rounded-md border border-accent-7 bg-accent-3 p-3 text-sm text-accent-11">
              Nepodařilo se načíst žebříček. Zkuste to prosím znovu.
            </p>
          ) : null}

          {fetching && !error ? <p>Načítáme aktuální výsledky…</p> : null}

          {!fetching && !error && scoreboard.length === 0 ? (
            <p>Pro vybrané období zatím nemáme žádné body.</p>
          ) : null}

          {scoreboard.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="min-w-full text-sm">
                <thead>
                  <tr className="border-b border-neutral-6 text-left">
                    <th className="py-2 pr-2" />
                    <th className="py-2 pr-4">Člen</th>
                    <th className="p-2 text-center">Lekce</th>
                    <th className="p-2 text-center">Skupiny</th>
                    <th className="p-2 text-center">Akce</th>
                    <th className="p-2 text-center">Manuální</th>
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
                          {entry.ranking ? `${entry.ranking}.` : '–'}
                        </td>
                        <td className="py-1 pr-4">
                          {entry.person ? (
                            <Link
                              href={{
                                pathname: '/clenove/[id]',
                                query: { id: entry.person.id },
                              }}
                              className="hover:underline text-inherit"
                            >
                              {entry.person.name}
                            </Link>
                          ) : (
                            '—'
                          )}
                        </td>
                        <td className="py-1 px-2 text-center">
                          {entry.lessonTotalScore ?? '0'}
                        </td>
                        <td className="py-1 px-2 text-center">
                          {entry.groupTotalScore ?? '0'}
                        </td>
                        <td className="py-1 px-2 text-center">
                          {entry.eventTotalScore ?? '0'}
                        </td>
                        <td className="py-1 px-2 text-center">
                          {entry.manualTotalScore ?? '0'}
                        </td>
                        <td className="py-1 pl-2 text-center font-bold">
                          {entry.totalScore ?? '0'}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          ) : null}
        </section>
      </div>
    </Layout>
  );
}

'use client';

import { DefinitionList } from '@/ui/DefinitionList';
import React from 'react';
import Link from 'next/link';
import { useQuery } from 'urql';
import { PageHeader } from '@/ui/PageHeader';
import { getPeriodRange, type PeriodUnit } from '@/lib/periods';
import { ScoreboardDocument } from '@/graphql/Scoreboard';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import {
  capitalize,
  fullDateFormatter,
  monthFormatter,
  numberFormatter,
} from '@/ui/format';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from '@/ui/style';
import { Table } from '@/ui/Table';
import { ChevronLeft, ChevronRight, InfoIcon } from 'lucide-react';

const periodOptions: { value: PeriodUnit; label: string }[] = [
  { value: 'schoolyear', label: 'Školní rok' },
  { value: 'semester', label: 'Pololetí' },
  { value: 'quarter', label: 'Čtvrtletí' },
  { value: 'month', label: 'Měsíc' },
];

export function Scoreboard() {
  const [cohortId, setCohortId] = React.useState('');
  const [unit, setUnit] = React.useState<PeriodUnit>('schoolyear');
  const [date, setDate] = React.useState(() => new Date());
  const period = getPeriodRange(unit, date);

  const [{ data, fetching, error }] = useQuery({
    query: ScoreboardDocument,
    variables: {
      cohortId: cohortId || null,
      since: period.since.toISOString().slice(0, 10),
      until: period.until.toISOString().slice(0, 10),
    },
  });

  const [cohorts, setCohorts] = React.useState(data?.cohortsList ?? []);
  if (data?.cohortsList && data.cohortsList !== cohorts) setCohorts(data.cohortsList);
  const scoreboard = data?.scoreboardEntriesList ?? [];

  return (
    <>
      <PageHeader title="Žebříček aktivity">
        <Popover>
          <PopoverTrigger
            aria-label="Jak se počítá skóre"
            title="Jak se počítá skóre"
            className={buttonCls({ variant: 'outline', size: 'icon' })}
          >
            <InfoIcon aria-hidden />
          </PopoverTrigger>
          <PopoverContent
            align="end"
            side="bottom"
            sideOffset={6}
            className="max-w-[calc(100vw-2rem)]"
          >
            <p className="mb-2 pr-6 text-sm font-semibold">Skóre se skládá z:</p>
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
          </PopoverContent>
        </Popover>
      </PageHeader>

      <div className="min-w-0">
        <div className="mb-3 grid w-full justify-items-start gap-2">
          <FacetRadioGroup
            inline
            name="scoreboard-cohort"
            label="Skupina"
            value={cohortId}
            defaultValue=""
            onChange={setCohortId}
            options={[
              { value: '', label: 'Všechny skupiny' },
              ...cohorts.map((cohort) => ({ value: cohort.id, label: cohort.name })),
            ]}
          />
          <FacetRadioGroup
            inline
            name="scoreboard-period"
            label="Období"
            value={unit}
            options={periodOptions}
            onChange={setUnit}
          />

          <div className="flex w-full flex-wrap items-center gap-2 sm:pl-20">
            <button
              type="button"
              aria-label="Předchozí období"
              className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
              onClick={() => setDate(new Date(period.since.getTime() - 86_400_000))}
            >
              <ChevronLeft aria-hidden />
            </button>
            <span className="text-xs font-medium tabular-nums text-neutral-12">
              {unit === 'schoolyear'
                ? `${period.since.getUTCFullYear()}/${period.until.getUTCFullYear()}`
                : unit === 'month'
                  ? capitalize(monthFormatter.format(period.since))
                  : fullDateFormatter.formatRange(
                      period.since,
                      new Date(period.until.getTime() - 86_400_000),
                    )}
            </span>
            <button
              type="button"
              aria-label="Další období"
              className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
              onClick={() => setDate(period.until)}
            >
              <ChevronRight aria-hidden />
            </button>
            <button
              type="button"
              className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
              onClick={() => setDate(new Date())}
            >
              Dnes
            </button>

            {(cohortId || unit !== 'schoolyear') && (
              <button
                type="button"
                className="px-2 py-1.5 text-xs text-accent-11 underline hover:text-accent-12"
                onClick={() => {
                  setCohortId('');
                  setUnit('schoolyear');
                  setDate(new Date());
                }}
              >
                Zrušit filtry
              </button>
            )}
          </div>
        </div>

        <section>
          <Table
            busy={fetching}
            error={error}
            columns={[
              <span key="rank" className="sr-only">
                Pořadí
              </span>,
              'Člen',
              <span key="lessons" className="block text-center">
                Lekce
              </span>,
              <span key="groups" className="block text-center">
                Společné
              </span>,
              <span key="events" className="block text-center">
                Akce
              </span>,
              <span key="total" className="block text-center">
                Celkem
              </span>,
            ]}
            empty="Pro vybrané období nejsou žádné body."
          >
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
                  <td className="font-bold">
                    {entry.ranking
                      ? `${numberFormatter.format(BigInt(entry.ranking))}.`
                      : '–'}
                  </td>
                  <td>
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
                  <td className="text-center">
                    {numberFormatter.format(BigInt(entry.lessonTotalScore ?? '0'))}
                  </td>
                  <td className="text-center">
                    {numberFormatter.format(BigInt(entry.groupTotalScore ?? '0'))}
                  </td>
                  <td className="text-center">
                    {numberFormatter.format(BigInt(entry.eventTotalScore ?? '0'))}
                  </td>
                  <td className="text-center font-bold">
                    {numberFormatter.format(BigInt(entry.totalScore ?? '0'))}
                  </td>
                </tr>
              );
            })}
          </Table>
        </section>
      </div>
    </>
  );
}

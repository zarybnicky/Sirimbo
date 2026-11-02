'use client';

import { DefinitionList } from '@/ui/DefinitionList';
import React from 'react';
import Link from 'next/link';
import { useQuery } from 'urql';
import { PageHeader } from '@/ui/PageHeader';
import {
  getPeriodRange,
  periodOptions,
  PeriodPicker,
  type PeriodUnit,
} from '@/ui/PeriodPicker';
import { ScoreboardDocument } from '@/graphql/Scoreboard';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { numberFormatter } from '@/ui/format';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from '@/ui/style';
import { Table } from '@/ui/Table';
import { InfoIcon } from 'lucide-react';
import {
  parseAsIsoDateTime,
  parseAsString,
  parseAsStringEnum,
  useQueryStates,
} from 'nuqs';

export function Scoreboard() {
  const [filters, setFilters] = useQueryStates(
    {
      cohortId: parseAsString.withDefault(''),
      unit: parseAsStringEnum<PeriodUnit>(periodOptions.map((x) => x.value)).withDefault('schoolyear'),
      date: parseAsIsoDateTime,
    },
    { shallow: true, scroll: false },
  );
  const period = { unit: filters.unit, date: filters.date ?? new Date() };
  const range = getPeriodRange(period.unit, period.date);

  const [{ data, fetching, error }] = useQuery({
    query: ScoreboardDocument,
    variables: {
      cohortId: filters.cohortId || null,
      since: range.since.toISOString().slice(0, 10),
      until: range.until.toISOString().slice(0, 10),
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
        <div className="mb-3 grid w-full grid-cols-1 gap-x-3 gap-y-2 sm:grid-cols-[max-content_minmax(0,1fr)]">
          <FacetRadioGroup
            inline
            name="scoreboard-cohort"
            label="Skupina"
            value={filters.cohortId}
            defaultValue=""
            onChange={(cohortId) => void setFilters({ cohortId })}
            options={[
              { value: '', label: 'Všechny skupiny' },
              ...cohorts.map((cohort) => ({ value: cohort.id, label: cohort.name })),
            ]}
          />
          <PeriodPicker
            name="scoreboard-period"
            value={period}
            onChange={({ unit, date }) => void setFilters({ unit, date })}
          />
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
              const rank = entry.ranking ? Number(entry.ranking) : undefined;
              return (
                <tr
                  key={`${entry.personId ?? 'unknown'}-${entry.cohortId ?? 'all'}`}
                  className={
                    rank === 1
                      ? 'bg-[rgb(254,240,138)] text-black'
                      : rank === 2
                        ? 'bg-[rgb(186,230,253)] text-black'
                        : rank === 3
                          ? 'bg-[rgb(254,215,170)] text-black'
                          : ''
                  }
                >
                  <td className="font-bold">
                    {rank ? `${numberFormatter.format(rank)}.` : '–'}
                  </td>
                  <td>
                    {entry.person ? (
                      <Link
                        href={`/clenove/${entry.person.id}`}
                        className="font-medium text-inherit underline"
                      >
                        {entry.person.name}
                      </Link>
                    ) : (
                      '?'
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

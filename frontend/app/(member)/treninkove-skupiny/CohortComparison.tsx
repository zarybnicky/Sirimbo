import { useState } from 'react';
import type { CohortListQuery, PersonHistogramFragment } from '@/graphql/Cohorts';
import {
  ageGroupNames,
  formatAgeGroup,
  formatClass,
  getBestCstsProgress,
} from '@/ui/csts';
import { Histogram } from '@/ui/Histogram';
import { Dialog, DialogContent, DialogTitle } from '@/ui/dialog';
import { Table } from '@/ui/Table';
import { compareText } from '@/lib/compare';
import { EntityLink } from '@/ui/EntityLink';

export type HistogramSelection = {
  title: string;
  people: readonly PersonHistogramFragment[];
};

export function CohortComparison({ data }: { data: CohortListQuery | undefined }) {
  const [selection, setSelection] = useState<HistogramSelection | null>(null);

  const rows = data
    ? [
        { id: null, name: 'Celý klub', color: null, people: data.people?.nodes ?? [] },
        {
          id: null,
          name: 'Bez skupiny',
          color: null,
          people: (data.people?.nodes ?? []).filter((x) => !x.cohortIds?.length),
        },
        ...(data.cohortsList ?? []).map((cohort) => ({
          id: cohort.id,
          name: cohort.name,
          color: cohort.colorRgb,
          people: (cohort.cohortMembershipsList ?? []).flatMap(({ person }) =>
            person ? [person] : [],
          ),
        })),
      ]
    : [];
  const ageBuckets = getAgeCounts(rows.flatMap((row) => row.people)).map(
    ([code]) => code,
  );

  return (
    <>
      <Table columns={['Skupina', 'Osob', 'Věk', 'STT', 'LAT']}>
        {rows.map((row) => (
          <tr key={row.id ?? row.name}>
            <th scope="row">
              {row.id ? (
                <EntityLink
                  type="cohort"
                  entity={row}
                  className="block border-l-4 pl-2 underline"
                  style={{ borderColor: row.color ?? undefined }}
                />
              ) : (
                row.name
              )}
            </th>
            <td>{row.people.length}</td>
            <td>
              <AgeHistogram
                people={row.people}
                buckets={ageBuckets}
                onSelect={(label, people) =>
                  setSelection({
                    title: `${row.name} · ${label}`,
                    people,
                  })
                }
              />
            </td>
            {(['Standard', 'Latin'] as const).map((discipline) => (
              <td key={discipline}>
                <ClassHistogram
                  people={row.people}
                  discipline={discipline}
                  onSelect={(label, people) =>
                    setSelection({
                      title: `${row.name} · ${label}`,
                      people,
                    })
                  }
                />
              </td>
            ))}
          </tr>
        ))}
      </Table>

      <HistogramPeopleDialog selection={selection} onClose={() => setSelection(null)} />
    </>
  );
}

export function HistogramPeopleDialog({
  selection,
  onClose,
}: {
  selection: HistogramSelection | null;
  onClose: () => void;
}) {
  return (
    <Dialog open={selection !== null} onOpenChange={(open) => { if (!open) onClose(); }}>
      <DialogContent aria-describedby={undefined}>
        <DialogTitle>{selection?.title}</DialogTitle>
        <p className="text-sm text-neutral-11">Osob: {selection?.people.length ?? 0}</p>
        <Table columns={['Jméno', 'Věk', 'STT', 'LAT']}>
          {selection?.people
            .toSorted((a, b) => compareText(a.name, b.name))
            .map((person) => (
              <tr key={person.id}>
                <th scope="row"><EntityLink type="person" entity={person} /></th>
                <td>{formatAgeGroup(person.ageGroup) ?? '?'}</td>
                <td>{personClass(person, 'Standard')}</td>
                <td>{personClass(person, 'Latin')}</td>
              </tr>
            ))}
        </Table>
      </DialogContent>
    </Dialog>
  );
}

const shortAgeGroups: Record<string, string> = {
  Under_8: 'U8',
  Juvenile: 'D',
  Juvenile_I: 'D1',
  Juvenile_II: 'D2',
  Junior: 'J',
  Junior_I: 'J1',
  Junior_II: 'J2',
  Youth: 'Ml',
  Under_21: 'U21',
  Adult: 'Do',
  Senior: 'S',
  Senior_I: 'S1',
  Senior_II: 'S2',
  Senior_III: 'S3',
  Senior_IV: 'S4',
  Senior_V: 'S5',
};

const collapsedAgeGroups: Record<string, string> = {
  Senior_Ia: 'Adult',
  Senior_Ib: 'Senior_I',
  Senior_IIa: 'Senior_I',
  Senior_IIb: 'Senior_II',
  Senior_IIIa: 'Senior_II',
  Senior_IIIb: 'Senior_III',
  Senior_IVa: 'Senior_III',
  Senior_IVb: 'Senior_IV',
};

const ageGroupOrder = new Map(
  Object.keys(ageGroupNames).map((code, index) => [code, index]),
);

function personAgeGroup(person: PersonHistogramFragment) {
  return collapsedAgeGroups[person.ageGroup ?? ''] ?? person.ageGroup ?? '?';
}

function personClass(person: PersonHistogramFragment, discipline: 'Standard' | 'Latin') {
  return (
    (person.cstsId &&
      formatClass(
        getBestCstsProgress(person.federationCategoryProgresses.nodes, discipline)
          ?.category?.class,
      )) ||
    '?'
  );
}

function getAgeCounts(
  people: readonly PersonHistogramFragment[],
  ageBuckets?: readonly string[],
) {
  const ages = people.map(personAgeGroup);
  const buckets =
    ageBuckets ??
    [
      ...new Set([
        '?',
        ...Object.keys(shortAgeGroups).filter(
          (code) => !['Juvenile', 'Junior', 'Senior'].includes(code),
        ),
        ...ages,
      ]),
    ]
      .filter(
        (code) =>
          ages.includes(code) ||
          (!['Under_8', 'Juvenile_I', 'Juvenile_II'].includes(code) &&
            !code.startsWith('Senior_')),
      )
      .toSorted(
        (a, b) =>
          Number(b === '?') - Number(a === '?') ||
          (ageGroupOrder.get(a) ?? Infinity) - (ageGroupOrder.get(b) ?? Infinity) ||
          a.localeCompare(b),
      );
  return buckets.map((code): [string, number] => [
    code,
    ages.filter((age) => age === code).length,
  ]);
}

export function AgeHistogram({
  label,
  people,
  buckets,
  onSelect,
}: {
  label?: string;
  people: readonly PersonHistogramFragment[];
  buckets?: readonly string[];
  onSelect?: (label: string, people: readonly PersonHistogramFragment[]) => void;
}) {
  return (
    <Histogram
      label={label}
      data={getAgeCounts(people, buckets)}
      bucketWidth={21}
      formatLabel={(code) => shortAgeGroups[code] ?? code}
      formatTitle={(code) => formatAgeGroup(code) ?? '?'}
      onSelect={
        onSelect
          ? (code) =>
              onSelect(
                `Věk: ${formatAgeGroup(code) ?? '?'}`,
                people.filter((x) => personAgeGroup(x) === code),
              )
          : undefined
      }
    />
  );
}

export function ClassHistogram({
  label,
  people,
  discipline,
  onSelect,
}: {
  label?: string;
  people: readonly PersonHistogramFragment[];
  discipline: 'Standard' | 'Latin';
  onSelect?: (label: string, people: readonly PersonHistogramFragment[]) => void;
}) {
  const classes = people.map((person) => personClass(person, discipline));
  return (
    <Histogram
      label={label}
      data={['?', 'E', 'D', 'C', 'B', 'A', 'M'].map((code) => [
        code,
        classes.filter((value) => value === code).length,
      ])}
      bucketWidth={18}
      onSelect={
        onSelect
          ? (code) =>
              onSelect(
                `${discipline}: ${code}`,
                people.filter((x) => personClass(x, discipline) === code),
              )
          : undefined
      }
    />
  );
}

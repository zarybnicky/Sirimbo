import React from 'react';
import Link from 'next/link';
import { gql } from '@urql/core';
import { useQuery } from 'urql';
import { Layout } from '@/components/layout/Layout';
import { TitleBar } from '@/ui/TitleBar';

type ScoreboardEntry = {
  personId: string | null;
  cohortId: string | null;
  lessonTotalScore: string | null;
  groupTotalScore: string | null;
  eventTotalScore: string | null;
  manualTotalScore: string | null;
  totalScore: string | null;
  ranking: string | null;
  person: { id: string; name: string } | null;
  cohort: { id: string; name: string } | null;
};

type CohortSummary = {
  id: string;
  name: string;
  archivedAt: string | null;
};

type ScoreboardQueryResult = {
  cohortsList: CohortSummary[];
  scoreboardEntries: ScoreboardEntry[] | null;
};

type ScoreboardQueryVariables = {
  cohortId?: string | null;
  since?: string;
  until?: string;
};

type PeriodPreset = 'schoolYear' | 'semester' | 'quarter' | 'custom';

type PeriodRange = {
  since: string | null;
  until: string | null;
  displaySince: string | null;
  displayUntil: string | null;
};

const periodLabels: Record<PeriodPreset, string> = {
  schoolYear: 'Školní rok',
  semester: 'Pololetí',
  quarter: 'Čtvrtletí',
  custom: 'Vlastní interval',
};

const ScoreboardDocument = gql`
  query Scoreboard($cohortId: BigInt, $since: Date, $until: Date) {
    cohortsList(orderBy: [NAME_ASC]) {
      id
      name
      archivedAt
    }
    scoreboardEntries(cohortId: $cohortId, since: $since, until: $until) {
      personId
      cohortId
      lessonTotalScore
      groupTotalScore
      eventTotalScore
      manualTotalScore
      totalScore
      ranking
      person {
        id
        name
      }
      cohort {
        id
        name
      }
    }
  }
`;

const formatDate = (value: Date) => value.toISOString().slice(0, 10);

const parseDate = (value: string): Date | null => {
  const parsed = new Date(`${value}T00:00:00Z`);
  return Number.isNaN(parsed.getTime()) ? null : parsed;
};

const addMonths = (value: Date, months: number) => {
  return new Date(Date.UTC(value.getUTCFullYear(), value.getUTCMonth() + months, value.getUTCDate()));
};

const addDays = (value: Date, days: number) => {
  return new Date(Date.UTC(value.getUTCFullYear(), value.getUTCMonth(), value.getUTCDate() + days));
};

const startOfSchoolYear = (value: Date) => {
  const year = value.getUTCMonth() >= 8 ? value.getUTCFullYear() : value.getUTCFullYear() - 1;
  return new Date(Date.UTC(year, 8, 1));
};

const computeRange = (
  preset: PeriodPreset,
  referenceDate: string,
  customSince: string,
  customUntil: string,
): PeriodRange => {
  if (preset === 'custom') {
    if (!customSince || !customUntil) {
      return { since: null, until: null, displaySince: customSince || null, displayUntil: customUntil || null };
    }

    const sinceDate = parseDate(customSince);
    const untilInclusive = parseDate(customUntil);

    if (!sinceDate || !untilInclusive) {
      return { since: null, until: null, displaySince: null, displayUntil: null };
    }

    const untilExclusive = addDays(untilInclusive, 1);
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilExclusive),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(untilInclusive),
    };
  }

  const reference = parseDate(referenceDate);
  if (!reference) {
    return { since: null, until: null, displaySince: null, displayUntil: null };
  }

  const schoolYearStart = startOfSchoolYear(reference);

  if (preset === 'schoolYear') {
    const untilDate = addMonths(schoolYearStart, 12);
    return {
      since: formatDate(schoolYearStart),
      until: formatDate(untilDate),
      displaySince: formatDate(schoolYearStart),
      displayUntil: formatDate(addDays(untilDate, -1)),
    };
  }

  const monthsDiff =
    (reference.getUTCFullYear() - schoolYearStart.getUTCFullYear()) * 12 +
    (reference.getUTCMonth() - schoolYearStart.getUTCMonth());

  if (preset === 'semester') {
    const isSecondSemester = monthsDiff >= 5;
    const sinceDate = isSecondSemester ? addMonths(schoolYearStart, 5) : schoolYearStart;
    const untilDate = isSecondSemester ? addMonths(schoolYearStart, 12) : addMonths(schoolYearStart, 5);
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(addDays(untilDate, -1)),
    };
  }

  if (preset === 'quarter') {
    const quarterIndex = Math.max(0, Math.min(3, Math.floor(monthsDiff / 3)));
    const sinceDate = addMonths(schoolYearStart, quarterIndex * 3);
    const untilDate = addMonths(sinceDate, 3);
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(addDays(untilDate, -1)),
    };
  }

  return { since: null, until: null, displaySince: null, displayUntil: null };
};

export default function ScoreboardPage() {
  const [selectedCohortId, setSelectedCohortId] = React.useState<string | null>(null);
  const [preset, setPreset] = React.useState<PeriodPreset>('schoolYear');
  const [referenceDate, setReferenceDate] = React.useState(() => formatDate(new Date()));
  const [customRange, setCustomRange] = React.useState<{ since: string; until: string }>({ since: '', until: '' });

  const period = React.useMemo(
    () => computeRange(preset, referenceDate, customRange.since, customRange.until),
    [preset, referenceDate, customRange],
  );

  const variables = React.useMemo<ScoreboardQueryVariables>(
    () => ({
      cohortId: selectedCohortId,
      since: period.since ?? undefined,
      until: period.until ?? undefined,
    }),
    [selectedCohortId, period.since, period.until],
  );

  const shouldPause = !period.since || !period.until;

  const [{ data, fetching, error }] = useQuery<ScoreboardQueryResult, ScoreboardQueryVariables>({
    query: ScoreboardDocument,
    variables,
    pause: shouldPause,
  });

  const cohorts = data?.cohortsList ?? [];
  const scoreboard = data?.scoreboardEntries ?? [];
  const activeCohort = selectedCohortId ? cohorts.find((item) => item.id === selectedCohortId) ?? null : null;

  const periodSummary =
    period.displaySince && period.displayUntil
      ? `${period.displaySince} – ${period.displayUntil}`
      : undefined;

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

        <section className="not-prose rounded-lg border border-border bg-surface p-4 space-y-4">
          <div className="grid gap-4 md:grid-cols-2 xl:grid-cols-4">
            <label className="flex flex-col gap-1 text-sm font-medium">
              Skupina
              <select
                className="rounded-md border border-border bg-background px-3 py-2 text-base"
                value={selectedCohortId ?? ''}
                onChange={(event) => setSelectedCohortId(event.target.value ? event.target.value : null)}
              >
                <option value="">Všechny skupiny</option>
                {cohorts.map((cohort) => (
                  <option key={cohort.id} value={cohort.id}>
                    {cohort.name}
                    {cohort.archivedAt ? ' (archivováno)' : ''}
                  </option>
                ))}
              </select>
            </label>

            <label className="flex flex-col gap-1 text-sm font-medium">
              Období
              <select
                className="rounded-md border border-border bg-background px-3 py-2 text-base"
                value={preset}
                onChange={(event) => setPreset(event.target.value as PeriodPreset)}
              >
                {Object.entries(periodLabels).map(([value, label]) => (
                  <option key={value} value={value}>
                    {label}
                  </option>
                ))}
              </select>
            </label>

            {preset === 'custom' ? (
              <>
                <label className="flex flex-col gap-1 text-sm font-medium">
                  Od
                  <input
                    type="date"
                    className="rounded-md border border-border bg-background px-3 py-2 text-base"
                    value={customRange.since}
                    onChange={(event) => setCustomRange((prev) => ({ ...prev, since: event.target.value }))}
                  />
                </label>
                <label className="flex flex-col gap-1 text-sm font-medium">
                  Do
                  <input
                    type="date"
                    className="rounded-md border border-border bg-background px-3 py-2 text-base"
                    value={customRange.until}
                    onChange={(event) => setCustomRange((prev) => ({ ...prev, until: event.target.value }))}
                  />
                </label>
              </>
            ) : (
              <label className="flex flex-col gap-1 text-sm font-medium">
                Referenční datum
                <input
                  type="date"
                  className="rounded-md border border-border bg-background px-3 py-2 text-base"
                  value={referenceDate}
                  onChange={(event) => setReferenceDate(event.target.value)}
                />
              </label>
            )}
          </div>

          {preset === 'custom' && shouldPause ? (
            <p className="text-sm text-muted-foreground">Vyplňte prosím datum od i do, aby bylo možné žebříček spočítat.</p>
          ) : null}

          {periodSummary ? (
            <p className="text-sm text-muted-foreground">
              Zobrazené období: <span className="font-medium text-foreground">{periodSummary}</span>
              {activeCohort ? ` · Skupina ${activeCohort.name}` : ' · Všechny skupiny'}
            </p>
          ) : null}
        </section>

        <section>
          {error ? (
            <p className="not-prose rounded-md border border-destructive/40 bg-destructive/10 p-3 text-sm text-destructive">
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
                  <tr className="border-b border-border text-left">
                    <th className="py-2 pr-2" />
                    <th className="py-2 pr-4">Člen</th>
                    <th className="py-2 px-2 text-center">Lekce</th>
                    <th className="py-2 px-2 text-center">Skupiny</th>
                    <th className="py-2 px-2 text-center">Akce</th>
                    <th className="py-2 px-2 text-center">Manuální</th>
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
                        <td className="py-1 pr-2 font-bold">{entry.ranking ? `${entry.ranking}.` : '–'}</td>
                        <td className="py-1 pr-4">
                          {entry.person ? (
                            <Link
                              href={{
                                pathname: '/clenove/[id]',
                                query: { id: entry.person.id },
                              }}
                              className="hover:underline"
                            >
                              {entry.person.name}
                            </Link>
                          ) : (
                            '—'
                          )}
                        </td>
                        <td className="py-1 px-2 text-center">{entry.lessonTotalScore ?? '0'}</td>
                        <td className="py-1 px-2 text-center">{entry.groupTotalScore ?? '0'}</td>
                        <td className="py-1 px-2 text-center">{entry.eventTotalScore ?? '0'}</td>
                        <td className="py-1 px-2 text-center">{entry.manualTotalScore ?? '0'}</td>
                        <td className="py-1 pl-2 text-center font-bold">{entry.totalScore ?? '0'}</td>
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

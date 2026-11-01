'use client';

import Link from 'next/link';
import { addDays, format, startOfWeek } from 'date-fns';
import {
  parseAsInteger,
  parseAsIsoDate,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import { CompetitionResultsDocument } from '@/graphql/CompetitionParticipation';
import type { CompetitionResultCondition } from '@/graphql';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { compareText } from '@/lib/compare';
import { Pagination } from '@/ui/Pagination';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { WeekPicker } from '@/ui/WeekPicker';
import { compareCategories, compareDisciplines, formatCategoryName } from '@/ui/csts';
import { formatDateOnly } from '@/ui/format';
import { inputCls } from '@/ui/style';
import { FormError } from '@/ui/form';

const views = ['year', 'gallery', 'weekend'] as const;

export function ClubResults() {
  const [filters, setFilters] = useQueryStates(
    { view: parseAsStringLiteral(views).withDefault('year'), page: parseAsInteger },
    { shallow: true, scroll: false },
  );
  return (
    <section className="mt-8 space-y-3" aria-label="Výsledky klubu">
      <h2 className="text-lg font-bold">Výsledky klubu</h2>
      <TabMenu
        selected={filters.view}
        onSelect={(view) => void setFilters({ view: view as typeof filters.view, page: 1 })}
      >
        <Tab id="year" title="Úspěchy roku">
          <ClubResultReport view="year" />
        </Tab>
        <Tab id="gallery" title="Galerie mistrů">
          <ClubResultReport view="gallery" />
        </Tab>
        <Tab id="weekend" title="Víkendové výsledky">
          <ClubResultReport view="weekend" />
        </Tab>
      </TabMenu>
    </section>
  );
}

function ClubResultReport({ view }: { view: (typeof views)[number] }) {
  const currentYear = new Date().getFullYear();
  const [filters, setFilters] = useQueryStates(
    {
      year: parseAsInteger,
      week: parseAsIsoDate.withDefault(startOfWeek(new Date(), { weekStartsOn: 1 })),
      page: parseAsInteger,
    },
    { shallow: true, scroll: false },
  );
  const year = filters.year ?? (view === 'gallery' ? null : currentYear);
  const week = startOfWeek(filters.week, { weekStartsOn: 1 });
  const condition: CompetitionResultCondition = { competitor: {}, highlight: true };
  if (view === 'weekend') {
    condition.date = {
      since: format(week, 'yyyy-MM-dd'),
      until: format(addDays(week, 7), 'yyyy-MM-dd'),
    };
  } else {
    if (year !== null)
      condition.date = { since: `${year}-01-01`, until: `${year + 1}-01-01` };
    condition.competitionTypes = ['CHAMPIONSHIP'];
    condition.maxRanking = view === 'gallery' ? 3 : 12;
  }
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionResultsDocument,
    variables: (offset) => ({
      condition,
      first: 50,
      offset,
    }),
    limit: 50,
    getTotal: (data) => data.competitionResults?.totalCount,
  });
  const results = (data?.competitionResults?.nodes ?? []).flatMap((result) =>
    result.competition ? [{ ...result, competition: result.competition }] : [],
  );
  const groups = Map.groupBy(results, (result) =>
    view === 'gallery'
      ? result.competitorId
      : `${result.competition.startDate}:${result.competition.event?.id ?? result.competition.id}`,
  );

  for (const entries of groups.values()) {
    entries.sort(
      (a, b) =>
        (view === 'gallery'
          ? b.competition.startDate.localeCompare(a.competition.startDate) ||
            compareDisciplines(
              a.competition.category?.discipline ?? '',
              b.competition.category?.discipline ?? '',
            )
          : 0) ||
        compareCategories(a.competition.category, b.competition.category) ||
        a.ranking - b.ranking ||
        (a.rankingTo ?? a.ranking) - (b.rankingTo ?? b.ranking) ||
        compareText(
          a.competitor?.name ?? a.competitorId,
          b.competitor?.name ?? b.competitorId,
        ),
    );
  }

  return (
    <>
      {view === 'weekend' ? (
        <WeekPicker
          title="Výsledky klubu"
          startDate={week}
          onChange={(value) =>
            void setFilters({
              week: typeof value === 'function' ? value(week) : value,
              page: 1,
            })
          }
        />
      ) : (
        <label className="flex items-center gap-2 text-sm">
          Rok
          <select
            className={inputCls({ className: 'w-auto' })}
            value={year ?? ''}
            onChange={(event) =>
              void setFilters({ year: Number(event.target.value) || null, page: 1 })
            }
          >
            {view === 'gallery' && <option value="">Všechny roky</option>}
            {Array.from(
              { length: currentYear - 1998 },
              (_, index) => currentYear - index,
            ).map((value) => (
              <option key={value} value={value}>
                {value}
              </option>
            ))}
          </select>
        </label>
      )}

      {fetching && (
        <p role="status" className="text-sm">
          Načítám…
        </p>
      )}
      {error && <FormError error={error} />}
      {!fetching && !error && (
        <ul className="space-y-4 text-sm">
          {[...groups].map(([key, entries]) => {
            const first = entries[0]!;
            const { competition, competitor } = first;
            const event = competition.event;
            return (
              <li key={key}>
                <details open>
                  <summary className="cursor-pointer font-semibold">
                    <Link
                      href={
                        view === 'gallery' && competitor
                          ? `/federated/${competition.federation}/competitors/${competitor.externalId}`
                          : `/federated/${competition.federation}/${event?.externalId ?? ''}`
                      }
                      className="text-accent-11 underline"
                      prefetch={false}
                    >
                      {view === 'gallery'
                        ? competitor?.name || first.competitorId
                        : `${formatDateOnly(competition.startDate)} · ${event?.name || event?.city || 'Soutěž'}`}
                    </Link>
                  </summary>
                  <ul className="mt-2 ml-4 space-y-2 border-l border-neutral-4 pl-3">
                    {entries.map(
                      ({
                        competition,
                        competitor,
                        competitionId,
                        competitorId,
                        ranking,
                        rankingTo,
                      }) => (
                        <li key={`${competitionId}:${competitorId}`}>
                          <span className="font-medium tabular-nums">
                            {ranking}
                            {rankingTo && rankingTo !== ranking ? `–${rankingTo}` : ''}.
                            místo
                          </span>
                          {' — '}
                          <Link
                            href={`/federated/${competition.federation}/${competition.event?.externalId ?? ''}/${competition.externalId}`}
                            className="text-accent-11 underline"
                            prefetch={false}
                          >
                            {formatCategoryName(competition)}
                          </Link>
                          {' · '}
                          {view === 'gallery' ? (
                            formatDateOnly(competition.startDate)
                          ) : (
                            <Link
                              href={`/federated/${competition.federation}/competitors/${competitor?.externalId ?? ''}`}
                              className="text-accent-11 underline"
                              prefetch={false}
                            >
                              {competitor?.name || competitorId}
                            </Link>
                          )}
                        </li>
                      ),
                    )}
                  </ul>
                </details>
              </li>
            );
          })}
        </ul>
      )}
      <Pagination {...pagination} />
    </>
  );
}

'use client';

import Link from 'next/link';
import { addDays, format, getWeek, startOfWeek } from 'date-fns';
import {
  parseAsArrayOf,
  parseAsInteger,
  parseAsIsoDate,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import { CompetitionResultsDocument } from '@/graphql/CompetitionParticipation';
import { CalendarViews } from '@/calendar/CalendarViews';
import { DateNavigator } from '@/calendar/DateNavigator';
import type { CompetitionResultCondition, CompetitorType } from '@/graphql';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { compareText } from '@/lib/compare';
import { Pagination } from '@/ui/Pagination';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { compareCategories, formatCategoryName, formatCompetitorType } from '@/ui/csts';
import { FacetFilter } from '@/ui/FacetFilter';
import { formatDateOnly } from '@/ui/format';
import { inputCls, typographyCls } from '@/ui/style';
import { FormError } from '@/ui/form';
import {
  MissingFederationCouples,
  MissingFederationMembers,
} from './FederationSuggestions';

const views = ['year', 'gallery', 'weekend', 'couples', 'members'] as const;
const competitorTypes = [
  'COUPLE',
  'SOLO',
  'DUO',
  'TRIO',
  'TEAM',
  'GROUP',
  'FORMATION',
] as const satisfies readonly CompetitorType[];
const defaultGalleryTypes = competitorTypes.filter((type) => type !== 'TEAM');

export function ClubResults() {
  const [filters, setFilters] = useQueryStates(
    { view: parseAsStringLiteral(views).withDefault('year'), page: parseAsInteger },
    { shallow: true, scroll: false },
  );
  return (
    <TabMenu
      className="mt-8"
      selected={filters.view}
      onSelect={(view) => void setFilters({ view: view as typeof filters.view, page: undefined })}
    >
      <Tab id="year" title="Úspěchy roku">
        <ClubResultReport view="year" />
      </Tab>
      <Tab id="gallery" title="Galerie mistrů">
        <ClubResultReport view="gallery" />
      </Tab>
      <Tab id="weekend" title="Poslední víkend">
        <ClubResultReport view="weekend" />
      </Tab>
      <Tab id="couples" title="Doplnit páry z ČSTS" requireAdmin>
        <MissingFederationCouples />
      </Tab>
      <Tab id="members" title="Doplnit členy z ČSTS" requireAdmin>
        <MissingFederationMembers />
      </Tab>
    </TabMenu>
  );
}

function ClubResultReport({ view }: { view: 'year' | 'gallery' | 'weekend' }) {
  const currentYear = new Date().getFullYear();
  const [filters, setFilters] = useQueryStates(
    {
      year: parseAsInteger,
      week: parseAsIsoDate.withDefault(startOfWeek(new Date(), { weekStartsOn: 1 })),
      page: parseAsInteger,
      competitorTypes: parseAsArrayOf(parseAsStringLiteral(competitorTypes)).withDefault(
        defaultGalleryTypes,
      ),
    },
    { shallow: true, scroll: false },
  );
  const year = filters.year ?? (view === 'gallery' ? null : currentYear);
  const week = startOfWeek(filters.week, { weekStartsOn: 1 });
  const condition: CompetitionResultCondition = { competitor: {}, highlight: true };
  if (view === 'gallery') condition.competitorTypes = filters.competitorTypes;
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
    variables: (offset) => ({ condition, first: 50, offset }),
    limit: 50,
    getTotal: (data) => data.competitionResults?.totalCount,
  });
  const results = (data?.competitionResults?.nodes ?? []).flatMap((x) =>
    x.competition ? [{ ...x, competition: x.competition }] : [],
  );
  const groups = Map.groupBy(results, (x) =>
    view === 'gallery'
      ? x.competitorId
      : `${x.competition.startDate}:${x.competition.event?.id ?? x.competition.id}`,
  );

  for (const entries of groups.values()) {
    entries.sort(
      (a, b) =>
        (view === 'gallery'
          ? b.competition.startDate.localeCompare(a.competition.startDate)
          : 0) ||
        compareCategories(a.competition.category, b.competition.category) ||
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
        <div>
          <div className="flex flex-wrap items-center justify-between gap-2">
            <h4 className={typographyCls({ variant: 'smallHeading', className: 'grow' })}>
              Výsledky klubu
            </h4>
            <DateNavigator
              variant="subtle"
              date={week}
              view={CalendarViews.week}
              setDate={(date) =>
                void setFilters({ week: startOfWeek(date, { weekStartsOn: 1 }), page: 1 })
              }
            />
          </div>
          <div className="mb-2 flex items-center text-neutral-11 text-sm">
            {CalendarViews.week.label(CalendarViews.week.range(week))}
            {` (${getWeek(week, { weekStartsOn: 1 })}. týden)`}
          </div>
        </div>
      ) : (
        <label className="flex items-center gap-2 text-sm">
          Rok
          <select
            className={inputCls({ className: 'w-auto' })}
            value={year ?? ''}
            onChange={(event) =>
              void setFilters({ year: Number(event.target.value) || null, page: undefined })
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

      {view === 'gallery' && (
        <div className="my-1">
        <FacetFilter
          inline
          label="Typ soutěžícího"
          value={filters.competitorTypes}
          defaultValue={defaultGalleryTypes}
          options={competitorTypes.map((value) => ({
            value,
            label: formatCompetitorType(value)!,
          }))}
          onChange={(competitorTypes) => void setFilters({ competitorTypes, page: undefined })}
        />
        </div>
      )}
      {fetching && <p className="text-sm">Načítám…</p>}
      {error && <FormError error={error} />}
      {!fetching && !error && (
        <ul className="space-y-4 text-sm">
          {[...groups].map(([key, entries]) => {
            const { competition, competitor, competitorId } = entries[0]!;
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
                        ? competitor?.name || competitorId
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

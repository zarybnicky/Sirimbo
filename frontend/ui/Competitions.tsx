import {
  CompetitionsDocument,
  type CompetitionsQueryVariables,
  type FederationEventSummaryFragment,
} from '@/graphql/CompetitionParticipation';
import type { CompetitionParticipantCondition } from '@/graphql';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { FormError } from '@/ui/form';
import { formatCategoryName } from '@/ui/csts';
import { cn } from '@/lib/cn';
import { compareText } from '@/lib/compare';
import {
  formatDateOnly,
  formatOfficialRole,
  formatPointsAndFinals,
  formatResultStatus,
} from '@/ui/format';
import { buttonCls, cardCls } from '@/ui/style';
import { ArrowLeftRight, ChevronLeft } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { EntityLink } from '@/ui/EntityLink';
import {
  groupCompetitionEvents,
  type CompetitionEntry,
  type CompetitionOfficialAssignment,
} from '@/calendar/competitions';
export { groupCompetitionEvents } from '@/calendar/competitions';
export type { CompetitionEntry, CompetitionOfficialAssignment } from '@/calendar/competitions';


function entryLabel(entry: CompetitionEntry, competition: boolean) {
  return competition
    ? formatCategoryName(entry.competition)
    : (entry.competitor?.name ?? entry.competitorId);
}

export function CompetitionEventCard({
  entries,
  officials = [],
  event,
  date: suppliedDate,
  collapsedEntries = entries,
  collapsedOfficials = officials,
  showDate = false,
  className,
}: {
  entries: readonly CompetitionEntry[];
  event: FederationEventSummaryFragment | null;
  date?: string;
  officials?: readonly CompetitionOfficialAssignment[];
  collapsedEntries?: readonly CompetitionEntry[];
  collapsedOfficials?: readonly CompetitionOfficialAssignment[];
  showDate?: boolean;
  className?: string;
}) {
  const [open, setOpen] = React.useState(false);
  const [override, setOverride] = React.useState<boolean | null>(null);
  const competition = entries[0]?.competition;
  const date = suppliedDate ?? competition?.startDate;
  if (!competition && !event) return null;
  const canExpand =
    entries.length > collapsedEntries.length ||
    officials.length > collapsedOfficials.length;
  const visible = open ? entries : collapsedEntries;
  const visibleOfficials = open ? officials : collapsedOfficials;
  const competitors = Map.groupBy(visible, (entry) => entry.competitorId);
  const competitions = Map.groupBy(visible, (entry) => entry.competitionId);
  const canToggle = competitors.size > 1 && competitions.size > 1;
  const byCompetition =
    canToggle && override !== null ? override : competitions.size < competitors.size;
  const groups = [...(byCompetition ? competitions : competitors)].toSorted(
    ([, a], [, b]) =>
      (byCompetition
        ? (a[0]!.competition.checkInEnd ?? '').localeCompare(
            b[0]!.competition.checkInEnd ?? '',
          )
        : 0) ||
      compareText(entryLabel(a[0]!, byCompetition), entryLabel(b[0]!, byCompetition)),
  );

  return (
    <div
      className={cardCls({
        className: cn('flex flex-col rounded-lg border-green-5 bg-green-2/80', className),
      })}
    >
      <div
        className={cn(
          'relative flex flex-col gap-1',
          canExpand && [
            '-mx-3 -mt-3 rounded-lg p-3 hover:bg-green-4',
            visible.length === 0 && visibleOfficials.length === 0 && '-mb-3',
          ],
        )}
      >
        {canExpand && (
          <button
            type="button"
            onClick={() => setOpen(!open)}
            className="absolute inset-0 flex cursor-pointer items-center justify-end rounded-lg p-3 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-green-8"
          >
            <ChevronLeft className={cn('size-4', open && '-rotate-90')} aria-hidden />
          </button>
        )}
        <div
          className={cn(
            'flex items-start justify-between gap-2 text-sm font-semibold leading-tight text-green-11',
            canExpand && 'relative pointer-events-none pr-4',
          )}
        >
          <CompetitionLink
            federation={competition?.federation ?? event?.federation}
            eventId={event?.externalId}
            className={cn(
              'text-green-11 hover:text-green-12',
              canExpand && 'pointer-events-auto',
            )}
          >
            {event?.name}
          </CompetitionLink>
        </div>
        <div
          className={cn(
            'flex items-baseline justify-between gap-2 text-xs leading-tight text-green-11',
            canExpand && 'relative pointer-events-none',
          )}
        >
          <span className="min-w-0 truncate">
            {[event?.location || event?.city, showDate && date && formatDateOnly(date)]
              .filter(Boolean)
              .join(', ')}
          </span>
        </div>
      </div>
      {groups.map(([key, entries], index) => (
        <article
          key={key}
          className="pt-1.5 pb-1 border-t border-green-6 first-of-type:border-t-0"
        >
          <CompetitionGroupContent
            entries={entries}
            event={event}
            byCompetition={byCompetition}
            onToggle={index === 0 && canToggle ? () => setOverride(!byCompetition) : undefined}
          />
        </article>
      ))}
      {[...Map.groupBy(visibleOfficials, (x) => x.role)]
        .toSorted(([a], [b]) => a.localeCompare(b))
        .map(([role, items]) => {
          const people = items
            .flatMap(({ person }) => person ?? [])
            .toSorted((a, b) =>
              compareText(a.canonicalName ?? '', b.canonicalName ?? ''),
            );
          if (people.length === 0) return null;
          return (
            <article key={role} className="border-t border-green-6 first-of-type:border-t-0 pt-2 text-sm">
              <span className="font-medium">{formatOfficialRole(role)}: </span>
              {people.map((person, index) => (
                <React.Fragment key={person.id}>
                  {index > 0 && ', '}
                  <EntityLink type="federation-person" entity={person} />
                </React.Fragment>
              ))}
            </article>
          );
        })}
    </div>
  );
}

function CompetitionLabel({
  event,
  entry,
  competition,
  cancelled = entry.cancelled,
}: {
  event: FederationEventSummaryFragment | null;
  entry: CompetitionEntry;
  competition: boolean;
  cancelled?: boolean;
}) {
  return competition ? (
    <CompetitionLink
      federation={entry.competition.federation}
      eventId={event?.externalId}
      competitionId={entry.competition.externalId}
      className={cn(
        'text-accent-11',
        (cancelled || entry.competition.status === 'CANCELLED') && 'line-through',
      )}
      title={cancelled ? 'Zrušená přihláška' : undefined}
    >
      {entryLabel(entry, true)}
    </CompetitionLink>
  ) : (
    <span
      className={cancelled ? 'line-through' : undefined}
      title={cancelled ? 'Zrušená přihláška' : undefined}
    >
      {entryLabel(entry, false)}
    </span>
  );
}

function CompetitionGroupContent({
  event,
  entries,
  byCompetition,
  onToggle,
}: {
  event: FederationEventSummaryFragment | null;
  entries: readonly CompetitionEntry[];
  byCompetition: boolean;
  onToggle?: () => void;
}) {
  const checkInEnd = byCompetition && entries[0]!.competition.checkInEnd;
  const showLeadingColumn =
    !byCompetition || entries.some((x) => x.ranking != null);
  return (
    <div
      className={cn(
        'grid gap-x-2 @max-[99px]/competition:grid-cols-1',
        showLeadingColumn
          ? 'grid-cols-[max-content_minmax(0,1fr)_max-content]'
          : 'grid-cols-[minmax(0,1fr)_max-content]',
      )}
    >
      <h5 className="col-span-full flex items-baseline gap-2 wrap-break-word pb-1 text-sm font-medium leading-tight text-neutral-12">
        {checkInEnd && (
          <span className="shrink-0 text-xs tabular-nums text-neutral-11">
            {checkInEnd.slice(0, 5)}
          </span>
        )}
        <span className="min-w-0 flex-1">
          <CompetitionLabel
            event={event}
            entry={entries[0]!}
            competition={byCompetition}
            cancelled={entries.every((x) => x.cancelled)}
          />
        </span>
        {onToggle && (
          <button
            type="button"
            className="inline-flex shrink-0 items-center gap-1 self-start rounded px-1 py-0.5 text-[11px] font-normal leading-tight text-green-11 hover:bg-green-4 focus-visible:outline-2 focus-visible:outline-offset-1 focus-visible:outline-green-8"
            onClick={onToggle}
          >
            <ArrowLeftRight className="size-3" aria-hidden />
            {byCompetition ? 'Soutěže' : 'Účastníci'}
          </button>
        )}
      </h5>
      {entries
        .toSorted(
          (a, b) =>
            (a.ranking ?? Infinity) - (b.ranking ?? Infinity) ||
            (a.ranking == null
              ? (a.competition.checkInEnd ?? '24:00').localeCompare(
                  b.competition.checkInEnd ?? '24:00',
                )
              : 0) ||
            compareText(entryLabel(a, !byCompetition), entryLabel(b, !byCompetition)),
        )
        .map((entry) => {
          const ranking = entry.ranking;
          const status = formatResultStatus(entry.completionStatus);
          return (
            <div
              key={`${entry.competitionId}:${entry.competitorId}`}
              className="col-span-full grid grid-cols-subgrid items-baseline py-1 text-xs"
            >
              {showLeadingColumn && (
                <div className="whitespace-nowrap font-semibold tabular-nums text-neutral-11">
                  {ranking != null ? (
                    <>
                      <span className={ranking <= 3 ? 'text-accent-11' : ''}>
                        {ranking}
                        {entry.rankingTo && entry.rankingTo !== ranking
                          ? `–${entry.rankingTo}`
                          : ''}
                        .
                      </span>
                      {entry.competition.participantsTotal != null &&
                        ` z ${entry.competition.participantsTotal}`}
                    </>
                  ) : (
                    !byCompetition && entry.competition.checkInEnd?.slice(0, 5)
                  )}
                </div>
              )}
              <div className="min-w-0 font-semibold text-neutral-12">
                <CompetitionLabel event={event} entry={entry} competition={!byCompetition} />
              </div>
              {ranking != null && (
                <div className="whitespace-nowrap font-semibold tabular-nums text-green-11">
                  {formatPointsAndFinals(entry, 'gain')}
                </div>
              )}
              {status && <div className="col-span-full text-neutral-11">{status}</div>}
            </div>
          );
        })}
    </div>
  );
}

export function CompetitionLink({
  federation,
  eventId,
  competitionId,
  children,
  className,
  title,
}: {
  federation?: string | null;
  eventId?: string | null;
  competitionId?: string | null;
  children: React.ReactNode;
  className?: string;
  title?: string;
}) {
  if (!federation || !eventId) return <>{children}</>;

  return (
    <Link
      href={`/federated/${federation}/${eventId}${competitionId ? `/${competitionId}` : ''}`}
      prefetch={false}
      className={cn('underline hover:text-accent-11', className)}
      title={title}
    >
      {children}
    </Link>
  );
}

export function CompetitionHistory({
  competitor,
}: {
  competitor: CompetitionParticipantCondition;
}) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionsDocument,
    variables: (offset): CompetitionsQueryVariables => ({
      competitor,
      orderBy: ['START_DATE_DESC', 'ID_DESC'],
      offset,
    }),
    limit: 50,
    pageKey: 'competitionPage',
    getTotal: (data) => data.competitions?.totalCount,
  });

  const planningParams = new URLSearchParams();
  if (competitor.personIds?.length)
    planningParams.set('personIds', competitor.personIds.join(','));
  if (competitor.coupleId) planningParams.set('coupleId', competitor.coupleId);
  if (competitor.cohortId) planningParams.set('cohortIds', competitor.cohortId);

  return (
    <>
      <Link
        href={`/planovani-soutezi${planningParams.size > 0 ? `?${planningParams}` : ''}`}
        className={buttonCls({ variant: 'outline', size: 'sm', className: 'self-end' })}
      >
        Plánování
      </Link>
      {error ? (
        <FormError error={error} />
      ) : fetching ? (
        <p className="text-sm text-neutral-11">Načítám...</p>
      ) : data?.competitions?.nodes.length ? (
        groupCompetitionEvents(data.competitions.nodes).map(({ key, event, date, entries }) => (
          <CompetitionEventCard key={key} event={event} date={date} entries={entries} showDate />
        ))
      ) : (
        <p className="text-sm text-neutral-11">Žádné přihlášky ani výsledky soutěží.</p>
      )}
      <Pagination {...pagination} />
    </>
  );
}

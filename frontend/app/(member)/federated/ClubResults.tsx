'use client';

import Link from 'next/link';
import { addDays, format, startOfWeek } from 'date-fns';
import {
  parseAsArrayOf,
  parseAsInteger,
  parseAsIsoDate,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import { CompetitionResultsDocument } from '@/graphql/CompetitionParticipation';
import type { CompetitionResultCondition, CompetitorType } from '@/graphql';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { compareText } from '@/lib/compare';
import { Pagination } from '@/ui/Pagination';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { WeekPicker } from '@/ui/WeekPicker';
import { compareCategories, formatCategoryName, formatCompetitorType } from '@/ui/csts';
import { FacetFilter } from '@/ui/FacetFilter';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { formatDateOnly, formatOpenDateRange } from '@/ui/format';
import { buttonCls, inputCls } from '@/ui/style';
import { FormError } from '@/ui/form';
import {
  FederationClubMembersDocument,
  MissingFederationCouplesDocument,
} from '@/graphql/Federation';
import {
  CreateCoupleDocument,
  CreateTenantMembershipDocument,
  DeleteCoupleDocument,
} from '@/graphql/Memberships';
import { TenantFormDocument } from '@/graphql/Tenant';
import { useMutation, useQuery } from 'urql';
import { useState } from 'react';
import { FederationLink, LocalPersonLinks } from '@/ui/federation-links';
import { Spinner } from '@/ui/Spinner';
import { Check, Undo2 } from 'lucide-react';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { CreatePersonDialog } from '@/ui/CreatePersonDialog';

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
    <section className="mt-8 space-y-3">
      <h2 className="text-lg font-bold">Výsledky klubu</h2>
      <TabMenu
        selected={filters.view}
        onSelect={(view) =>
          void setFilters({ view: view as typeof filters.view, page: 1 })
        }
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
        <Tab id="couples" title="Doplnit páry z ČSTS" requireAdmin>
          <MissingFederationCouples />
        </Tab>
        <Tab id="members" title="Doplnit členy z ČSTS" requireAdmin>
          <MissingFederationMembers />
        </Tab>
      </TabMenu>
    </section>
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
          ? b.competition.startDate.localeCompare(a.competition.startDate) ||
            compareCategories(a.competition.category, b.competition.category)
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

      {view === 'gallery' && (
        <FacetFilter
          inline
          label="Typ soutěžícího"
          value={filters.competitorTypes}
          defaultValue={defaultGalleryTypes}
          options={competitorTypes.map((value) => ({
            value,
            label: formatCompetitorType(value)!,
          }))}
          onChange={(competitorTypes) => void setFilters({ competitorTypes, page: 1 })}
        />
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

function MissingFederationCouples() {
  const [active, setActive] = useState(true);
  const [query] = useQuery({ query: MissingFederationCouplesDocument });
  const [creation, createCouple] = useMutation(CreateCoupleDocument);
  const [deletion, deleteCouple] = useMutation(DeleteCoupleDocument);
  const [created, setCreated] = useState<Record<string, string | undefined>>({});
  const people = query.data?.people?.nodes ?? [];
  const identities = Map.groupBy(
    people.flatMap((p) =>
      p.federationPeople.nodes.map((fp) => ({ ...p, federationId: fp.id })),
    ),
    (p) => p.federationId,
  );
  const couples = new Map(
    people
      .flatMap((p) =>
        p.federationPeople.nodes.flatMap((fp) => fp.competitorComponents.nodes),
      )
      .flatMap(({ competitor: c }) => {
        if (
          !c ||
          Boolean(c.active) !== active ||
          c.competitorType !== 'COUPLE' ||
          c.components.nodes.length !== 2
        )
          return [];
        const man = identities.get(
          c.components.nodes.find((p) => p.role === 'LEAD')?.personId ?? '',
        );
        const woman = identities.get(
          c.components.nodes.find((p) => p.role === 'FOLLOW')?.personId ?? '',
        );
        if (man?.length !== 1 || woman?.length !== 1 || man[0]!.id === woman[0]!.id)
          return [];
        const key = `${man[0]!.id}:${woman[0]!.id}`;
        if (c.couplesList.length > 0 && !created[key]) return [];
        return [[key, { ...c, man: man[0]!, woman: woman[0]! }] as const];
      }),
  );
  const saving = creation.fetching || deletion.fetching;

  if (!query.data) return query.error ? <FormError error={query.error} /> : <Spinner />;

  return (
    <div className="grid gap-3 text-sm">
      <FacetRadioGroup
        label=""
        name="couple-status"
        value={active ? 'active' : 'inactive'}
        options={[
          { value: 'active', label: 'Aktivní páry' },
          { value: 'inactive', label: 'Neaktivní páry' },
        ]}
        onChange={(value) => setActive(value === 'active')}
      />
      <FormError error={creation.error ?? deletion.error ?? query.error} />
      {query.fetching && <Spinner />}
      {couples.size === 0 && <p>Žádné chybějící páry</p>}
      {[...couples].map(([key, candidate]) => {
        const editDates =
          !candidate.since ||
          (candidate.active === false && !candidate.until) ||
          (!!candidate.until &&
            Date.parse(candidate.since) >= Date.parse(candidate.until));
        return (
          <div
            key={key}
            className={`flex flex-wrap items-center justify-between gap-3 rounded-md px-3 py-2 ${created[key] ? 'bg-accent-2' : 'bg-neutral-2'}`}
          >
            <div className="grid gap-1">
              <div className="font-bold">
                <LocalPersonLinks people={[candidate.man, candidate.woman]} />
              </div>
              <div className="flex flex-wrap gap-x-3 gap-y-1 text-neutral-11">
                <FederationLink
                  entity="competitors"
                  federation={candidate.federation}
                  externalId={candidate.externalId}
                >
                  {candidate.federation === 'csts'
                    ? 'ČSTS'
                    : candidate.federation.toUpperCase()}{' '}
                  {candidate.externalId}
                </FederationLink>
                {(candidate.since || candidate.until) && (
                  <span>{formatOpenDateRange(candidate)}</span>
                )}
                {(candidate.latestResultAt || candidate.latestEntryAt) && (
                  <span>
                    Poslední soutěž:{' '}
                    {candidate.latestResultAt
                      ? formatDateOnly(candidate.latestResultAt)
                      : ''}
                    {candidate.latestEntryAt &&
                    candidate.latestEntryAt > new Date().toISOString()
                      ? ' (+ budoucí)'
                      : ''}
                  </span>
                )}
                {created[key] && (
                  <Link
                    className="text-accent-11 underline"
                    href={`/pary/${created[key]}`}
                  >
                    Pár vytvořen
                  </Link>
                )}
              </div>
            </div>
            {!created[key] && editDates ? (
              <Dialog>
                <DialogTrigger.Add size="sm" text="Vytvořit pár" />
                <DialogContent>
                  <CreateCoupleForm
                    couple={{
                      manId: candidate.man.id,
                      womanId: candidate.woman.id,
                      since: candidate.since,
                      until: candidate.until,
                    }}
                    onCreated={(id) =>
                      setCreated((previous) => ({ ...previous, [key]: id }))
                    }
                  />
                </DialogContent>
              </Dialog>
            ) : (
              <button
                type="button"
                className={buttonCls({ size: 'sm', variant: 'outline' })}
                disabled={saving}
                onClick={async () => {
                  if (created[key]) {
                    const result = await deleteCouple({ id: created[key] });
                    if (result.error || !result.data?.deleteCouple) return;
                    setCreated((previous) => ({ ...previous, [key]: undefined }));
                  } else {
                    const result = await createCouple({
                      input: {
                        couple: {
                          manId: candidate.man.id,
                          womanId: candidate.woman.id,
                          since: candidate.since ?? undefined,
                          until: candidate.until ?? undefined,
                        },
                      },
                    });
                    const id = result.data?.createCouple?.couple?.id;
                    if (result.error || !id) return;
                    setCreated((previous) => ({ ...previous, [key]: id }));
                  }
                }}
              >
                {created[key] ? <Undo2 /> : <Check />}
                {created[key] ? 'Zpět' : 'Vytvořit pár'}
              </button>
            )}
          </div>
        );
      })}
    </div>
  );
}

function MissingFederationMembers() {
  const [tenant] = useQuery({ query: TenantFormDocument });
  const club = tenant.data?.clubs?.nodes.find(
    (c) => c.externalId === tenant.data?.tenant?.cstsId?.toString(),
  );
  const [membership, addMembership] = useMutation(CreateTenantMembershipDocument);
  const query = usePaginatedQuery({
    query: FederationClubMembersDocument,
    variables: (offset) => ({ clubId: club?.id ?? '', offset }),
    getTotal: (data) => data.federationPeople?.totalCount,
    limit: 100,
    pause: !club,
  });
  if (!tenant.data)
    return tenant.error ? <FormError error={tenant.error} /> : <Spinner />;
  if (!club)
    return (
      <Link className="text-accent-11 underline" href="/tanecni-klub">
        Propojte klub s ČSTS v nastavení klubu.
      </Link>
    );
  if (!query.data) return query.error ? <FormError error={query.error} /> : <Spinner />;

  const currentIds = new Set(
    query.data.people?.nodes.filter((p) => p.isMember).map((p) => p.cstsId),
  );
  const people =
    query.data.federationPeople?.nodes.filter(
      (p) => !currentIds.has(Number(p.externalId)),
    ) ?? [];
  return (
    <div className="grid gap-3 text-sm">
      <p className="font-bold">{club.name}</p>
      <FormError error={membership.error ?? query.error ?? tenant.error} />
      {people.length === 0 && <p>Žádní chybějící členové na této stránce.</p>}
      {people.map((person) => (
        <div key={person.id} className="grid gap-2 rounded-md bg-neutral-2 px-3 py-2">
          <FederationLink
            entity="people"
            federation="csts"
            externalId={person.externalId}
          >
            {person.canonicalName ||
              `${person.firstName ?? ''} ${person.lastName ?? ''}`.trim() ||
              person.externalId}{' '}
            · IDT {person.externalId}
          </FederationLink>
          {person.peopleList.length > 0 ? (
            person.peopleList.map((local) => (
              <div
                key={local.id}
                className="flex flex-wrap items-center justify-between gap-2"
              >
                <LocalPersonLinks people={[local]} />
                <button
                  type="button"
                  className={buttonCls({ size: 'sm', variant: 'outline' })}
                  disabled={membership.fetching}
                  onClick={() =>
                    addMembership({ input: { tenantMembership: { personId: local.id } } })
                  }
                >
                  <Check /> Přidat členství
                </button>
              </div>
            ))
          ) : (
            <CreatePersonDialog
              redirect={false}
              defaults={{
                firstName: person.firstName ?? '',
                lastName: person.lastName ?? '',
                gender:
                  person.gender === 'MALE'
                    ? 'MAN'
                    : person.gender === 'FEMALE'
                      ? 'WOMAN'
                      : 'UNSPECIFIED',
                birthDate: person.dob,
                cstsId: Number(person.externalId),
              }}
            />
          )}
        </div>
      ))}
      <Pagination {...query.pagination} />
    </div>
  );
}

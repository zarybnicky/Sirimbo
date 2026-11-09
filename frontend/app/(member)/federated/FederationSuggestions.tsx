'use client';

import Link from 'next/link';
import { useState } from 'react';
import { useMutation, useQuery } from 'urql';
import {
  FederationClubMembersDocument,
  MissingFederationCouplesDocument,
  type FederationClubMembersQuery,
} from '@/graphql/Federation';
import type { FederationCompetitor, Person } from '@/graphql';
import {
  CreateCoupleDocument,
  CreateTenantMembershipDocument,
  DeleteCoupleDocument,
} from '@/graphql/Memberships';
import { TenantFormOptionsDocument } from '@/graphql/Tenant';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { formatDateOnly, formatOpenDateRange } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { FormError } from '@/ui/form';
import { LocalPersonLinks } from '@/ui/federation-links';
import { EntityLink } from '@/ui/EntityLink';
import { Spinner } from '@/ui/Spinner';
import { Check, Undo2 } from 'lucide-react';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { CreatePersonDialog } from '@/ui/CreatePersonDialog';

export function MissingFederationCouples() {
  const [active, setActive] = useState(true);
  const [query] = useQuery({ query: MissingFederationCouplesDocument });
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
      <FormError error={query.error} />
      {query.fetching && <Spinner />}
      {couples.size === 0 && <p>Žádné chybějící páry</p>}
      {[...couples].map(([key, candidate]) => (
        <CoupleSuggestion
          key={key}
          candidate={candidate}
          createdId={created[key]}
          onChange={(id) => setCreated((previous) => ({ ...previous, [key]: id }))}
        />
      ))}
    </div>
  );
}

type SuggestedCouple = Pick<
  FederationCompetitor,
  | 'active'
  | 'externalId'
  | 'federation'
  | 'since'
  | 'until'
  | 'latestEntryAt'
  | 'latestResultAt'
> & { man: Pick<Person, 'id' | 'name'>; woman: Pick<Person, 'id' | 'name'> };

function CoupleSuggestion({
  candidate,
  createdId,
  onChange,
}: {
  candidate: SuggestedCouple;
  createdId?: string;
  onChange: (id?: string) => void;
}) {
  const [creation, createCouple] = useMutation(CreateCoupleDocument);
  const [deletion, deleteCouple] = useMutation(DeleteCoupleDocument);
  const saving = creation.fetching || deletion.fetching;
  const editDates =
    !candidate.since ||
    (candidate.active === false && !candidate.until) ||
    (!!candidate.until && Date.parse(candidate.since) >= Date.parse(candidate.until));
  return (
    <div
      className={`flex flex-wrap items-center justify-between gap-3 rounded-md px-3 py-2 ${createdId ? 'bg-accent-2' : 'bg-neutral-2'}`}
    >
      <div className="grid gap-1">
        <div className="font-bold">
          <LocalPersonLinks people={[candidate.man, candidate.woman]} />
        </div>
        <div className="flex flex-wrap gap-x-3 gap-y-1 text-neutral-11">
          <EntityLink type="federation-competitor" entity={candidate} fallback={false}>
            {candidate.federation === 'csts'
              ? 'ČSTS'
              : candidate.federation.toUpperCase()}{' '}
            {candidate.externalId}
          </EntityLink>
          {(candidate.since || candidate.until) && (
            <span>{formatOpenDateRange(candidate)}</span>
          )}
          {(candidate.latestResultAt || candidate.latestEntryAt) && (
            <span>
              Poslední soutěž:{' '}
              {candidate.latestResultAt ? formatDateOnly(candidate.latestResultAt) : ''}
              {candidate.latestEntryAt &&
              candidate.latestEntryAt > new Date().toISOString()
                ? ' (+ budoucí)'
                : ''}
            </span>
          )}
        </div>
      </div>
      {!createdId && editDates ? (
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
              onCreated={onChange}
            />
          </DialogContent>
        </Dialog>
      ) : (
        <button
          type="button"
          className={buttonCls({ size: 'sm', variant: 'outline' })}
          disabled={saving}
          onClick={async () => {
            if (createdId) {
              const result = await deleteCouple({ id: createdId });
              if (result.error || !result.data?.deleteCouple) return;
              onChange();
            } else {
              const result = await createCouple({
                input: {
                  couple: {
                    manId: candidate.man.id,
                    womanId: candidate.woman.id,
                    since: candidate.since,
                    until: candidate.until,
                  },
                },
              });
              const id = result.data?.createCouple?.couple?.id;
              if (result.error || !id) return;
              onChange(id);
            }
          }}
        >
          {createdId ? <Undo2 /> : <Check />}
          {createdId ? 'Zpět' : 'Vytvořit pár'}
        </button>
      )}
      <FormError error={creation.error ?? deletion.error} />
    </div>
  );
}

export function MissingFederationMembers() {
  const [tenant] = useQuery({ query: TenantFormOptionsDocument });
  const club = tenant.data?.clubs?.nodes.find(
    (c) => c.externalId === tenant.data?.tenant?.cstsId?.toString(),
  );
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

  const currentIds = new Set(query.data.people?.nodes.map((p) => p.cstsId));
  const people =
    query.data.federationPeople?.nodes.filter(
      (p) => !currentIds.has(Number(p.externalId)),
    ) ?? [];

  return (
    <div className="grid gap-3 text-sm">
      <FormError error={query.error ?? tenant.error} />
      {people.length === 0 && <p>Žádní chybějící členové.</p>}
      {people.map((person) => (
        <MemberSuggestion key={person.id} person={person} />
      ))}
      <Pagination {...query.pagination} />
    </div>
  );
}

function MemberSuggestion({
  person,
}: {
  person: NonNullable<FederationClubMembersQuery['federationPeople']>['nodes'][number];
}) {
  const [membership, addMembership] = useMutation(CreateTenantMembershipDocument);
  return (
    <div className="flex justify-between gap-2 rounded-md bg-neutral-2 px-3 py-2">
      <EntityLink type="federation-person" entity={{ federation: 'csts', ...person}}>
        {person.canonicalName} · IDT {person.externalId}
      </EntityLink>

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
      <FormError error={membership.error} />
    </div>
  );
}

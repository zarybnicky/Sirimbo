import {
  FederationIdIssuesDocument,
  MissingFederationIdCandidatesDocument,
  type MissingFederationIdCandidatesQuery,
  UpdatePersonDocument,
} from '@/graphql/Person.ts';
import { useAuth } from '@/lib/auth.ts';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import { useMutation, useQuery } from 'urql';
import { Spinner } from '@/ui/Spinner.tsx';
import { Tab, TabMenu } from '@/ui/TabMenu.tsx';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog.tsx';
import { EntityLink } from '@/ui/EntityLink';
import { FormError } from '@/ui/form.tsx';
import { formatDateOnly } from '@/ui/format.ts';
import { EditPersonForm } from '@/ui/forms/EditPersonForm.tsx';
import React from 'react';
import { DotSeparated } from '@/ui/DotSeparated.tsx';
import { formatAgeGroup } from '@/ui/csts.ts';
import { buttonCls } from '@/ui/style.tsx';
import { Check, Undo2 } from 'lucide-react';

type Federation = 'csts' | 'wdsf';
type Person = NonNullable<MissingFederationIdCandidatesQuery['people']>['nodes'][number];
type Candidate = Person['federationCandidatesList'][number];

export function FederationIdWidget({ federation }: { federation: Federation }) {
  const [tab, setTab] = useQueryState(
    'ids',
    parseAsString.withDefault('missing').withOptions({ history: 'push' }),
  );
  return (
    <TabMenu selected={tab} onSelect={setTab}>
      <Tab id="missing" title="Doplnit chybějící">
        <FederationIdBackfillWidget federation={federation} />
      </Tab>
      <Tab id="issues" title="Zkontrolovat vyplněné">
        <FederationIdIssues federation={federation} />
      </Tab>
    </TabMenu>
  );
}

function FederationIdBackfillWidget({ federation }: { federation: 'csts' | 'wdsf' }) {
  const idField = federation === 'csts' ? 'cstsId' : 'wdsfId';
  const idLabel = federation === 'csts' ? 'IDT' : 'MIN';
  const [{ data, error }] = useQuery({
    query: MissingFederationIdCandidatesDocument,
    variables: {
      federation,
      condition: { [idField]: null, membership: { state: 'CURRENT' } },
    },
  });
  const [{ error: mutationError, fetching: saving }, updatePerson] =
    useMutation(UpdatePersonDocument);
  const [selectedByPerson, setSelectedByPerson] = React.useState<
    Record<
      string,
      {
        candidate: Candidate;
        undo: { cstsId?: null; wdsfId?: null };
      } | null
    >
  >({});

  const people = (data?.people?.nodes ?? [])
    .filter((p) => p[idField] === null || selectedByPerson[p.id])
    .map((person) => {
      const selected = selectedByPerson[person.id];
      return {
        ...person,
        selected,
        candidates: selected ? [selected.candidate] : person.federationCandidatesList,
      };
    })
    .toSorted(
      (a, b) => (b.candidates[0]?.similarity ?? -1) - (a.candidates[0]?.similarity ?? -1),
    );

  async function selectCandidate(person: Person, candidate: Candidate | null) {
    const patch = candidate
      ? {
          cstsId: !person.cstsId ? (candidate.cstsId ?? undefined) : undefined,
          wdsfId: !person.wdsfId ? (candidate.wdsfId ?? undefined) : undefined,
        }
      : selectedByPerson[person.id]!.undo;
    const result = await updatePerson({
      input: { id: person.id, patch },
    });
    if (!result.error) {
      setSelectedByPerson((selected) => ({
        ...selected,
        [person.id]: candidate
          ? {
              candidate,
              undo: {
                cstsId: patch.cstsId ? null : undefined,
                wdsfId: patch.wdsfId ? null : undefined,
              },
            }
          : null,
      }));
    }
  }

  if (!data?.people) return error ? <FormError error={error} /> : <Spinner />;

  return (
    <div className="grid gap-2">
      <FormError error={mutationError ?? error} />
      {saving && <Spinner />}
      {people.length === 0 && (
        <div className="text-sm text-neutral-11">
          Všichni aktuální členové mají {federation === 'csts' ? 'ČSTS IDT' : 'WDSF MIN'}{' '}
          vyplněné.
        </div>
      )}
      {people.map((person) => (
        <div
          key={person.id}
          className="grid gap-2 border-b border-neutral-4 py-3 text-sm"
        >
          <div className="flex flex-wrap items-baseline gap-x-2 gap-y-1">
            <Link
              href={`/clenove/${person.id}`}
              className="font-bold text-accent-11 underline"
            >
              {person.name}
            </Link>
            <DotSeparated className="text-neutral-11">
              {formatAgeGroup(person.ageGroup)}
              {person.birthDate ? new Date(person.birthDate).getFullYear() : null}
            </DotSeparated>
          </div>

          <div className="grid gap-1">
            {person.candidates.length === 0 && (
              <div className="text-neutral-11">Žádní kandidáti.</div>
            )}
            {person.candidates.map((candidate) => (
              <div
                key={candidate.id}
                className={`flex flex-wrap items-center justify-between gap-2 rounded-md px-2 py-1.5 ${person.selected ? 'bg-accent-2 text-accent-12' : 'bg-neutral-2'}`}
              >
                <div className="min-w-0">
                  <div className="flex min-w-0 items-center gap-1 font-medium">
                    {person.selected && 'Vyplněno: '}
                    <EntityLink
                      type="federation-person"
                      entity={{ federation, externalId: candidate.id }}
                    >
                      {candidate.name || 'bez jména'}
                    </EntityLink>
                  </div>
                  <div className="flex flex-wrap gap-x-2 gap-y-1 text-xs text-neutral-11">
                    <span>
                      {idLabel} {candidate.id}
                    </span>
                    {federation === 'wdsf' && candidate.cstsId && (
                      <span>IDT {candidate.cstsId}</span>
                    )}
                    {federation === 'csts' && candidate.wdsfId && (
                      <span>MIN {candidate.wdsfId}</span>
                    )}
                    {candidate.birthYear && <span>{candidate.birthYear}</span>}
                    {candidate.ageGroup && (
                      <span>{formatAgeGroup(candidate.ageGroup)}</span>
                    )}
                  </div>
                </div>
                <button
                  type="button"
                  className={buttonCls({ size: 'sm', variant: 'outline' })}
                  disabled={saving}
                  onClick={() =>
                    selectCandidate(person, person.selected ? null : candidate)
                  }
                >
                  {person.selected ? <Undo2 /> : <Check />}
                  {person.selected ? 'Zpět' : 'Vybrat'}
                </button>
              </div>
            ))}
          </div>
        </div>
      ))}
    </div>
  );
}

function FederationIdIssues({ federation }: { federation: Federation }) {
  const auth = useAuth();
  const showClubs =
    auth.isSystemAdmin || (auth.isAdmin && auth.adminTenantIds.length > 1);
  const [query] = useQuery({
    query: FederationIdIssuesDocument,
    variables: { federation },
  });
  if (!query.data) return query.error ? <FormError error={query.error} /> : <Spinner />;

  const idField = federation === 'csts' ? 'cstsId' : 'wdsfId';
  const idLabel = federation === 'csts' ? 'ČSTS IDT' : 'WDSF MIN';
  const members = new Set(
    [...(query.data.current?.nodes ?? []), ...(query.data.former?.nodes ?? [])].map(
      (p) => p.id,
    ),
  );
  const groups = [
    ...Map.groupBy(
      (query.data.people?.nodes ?? []).filter(
        (p) => p[idField] !== null && (p[idField] > 0 || members.has(p.id)),
      ),
      (p) => p[idField]!,
    ),
  ].filter(([id, xs]) => (id <= 0 || xs.length > 1) && xs.some((p) => members.has(p.id)));

  return (
    <div className="grid gap-3 text-sm">
      <FormError error={query.error} />
      {groups.length === 0 && <p>Žádná sdílená ani neplatná ID.</p>}
      {groups.map(([id, people]) => (
        <section key={id} className="grid gap-2 border-b border-neutral-4 pb-3">
          <h3 className="font-bold">
            {idLabel} {id}{' '}
            {id > 0 ? (
              <EntityLink type="federation-person" entity={{ federation, externalId: id }}>
                {people[0]!.federationPeople.nodes[0]?.canonicalName ||
                  `${idLabel} ${id}`}
              </EntityLink>
            ) : (
              'Neplatné ID'
            )}
          </h3>
          {people.map((person) => (
            <div
              key={person.id}
              className="flex flex-wrap items-center justify-between gap-2 rounded-md bg-neutral-2 px-2 py-1.5"
            >
              <div className="flex flex-wrap items-baseline gap-x-2">
                <Link
                  href={`/clenove/${person.id}`}
                  className="font-bold text-accent-11 underline"
                >
                  {person.name}
                </Link>
                {person.birthDate && (
                  <span className="text-neutral-11">
                    {formatDateOnly(person.birthDate)}
                  </span>
                )}
                {showClubs &&
                  [
                    ...Map.groupBy(
                      [
                        ...person.tenantMembershipsList,
                        ...person.tenantTrainersList,
                        ...person.tenantAdministratorsList,
                      ].filter(
                        (m) =>
                          auth.isSystemAdmin || auth.adminTenantIds.includes(m.tenantId),
                      ),
                      (membership) => membership.tenantId,
                    ),
                  ].map(([tenantId, xs]) => (
                    <span key={tenantId} className="text-neutral-11">
                      {xs[0]!.tenantName ?? tenantId}
                      {xs.every((m) => m.status !== 'ACTIVE') &&
                        (xs.some((m) => m.status === 'PENDING')
                          ? ' (budoucí)'
                          : ' (bývalý)')}
                    </span>
                  ))}
              </div>
              <Dialog>
                <DialogTrigger.Edit size="sm" />
                <DialogContent className="sm:max-w-2xl">
                  <DialogTitle>{person.name}</DialogTitle>
                  <EditPersonForm id={person.id} />
                </DialogContent>
              </Dialog>
            </div>
          ))}
        </section>
      ))}
    </div>
  );
}

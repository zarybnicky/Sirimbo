import {
  MissingFederationIdCandidatesDocument,
  UpdatePersonFederationIdDocument,
  type MissingFederationIdCandidatesQuery,
} from '@/graphql/Person';
import { FederationLink } from '@/ui/federation-links';
import { formatAgeGroup, type AgeRange } from '@/ui/csts';
import { buttonCls } from '@/ui/style';
import { Check, Undo2 } from 'lucide-react';
import Link from 'next/link';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { FormError } from './form';

type Person = NonNullable<
  NonNullable<MissingFederationIdCandidatesQuery['people']>['nodes'][number]
>;
type Candidate = NonNullable<Person['federationCandidatesList']>[number];

export function FederationIdBackfillWidget({
  federation,
}: {
  federation: 'csts' | 'wdsf';
}) {
  const idField = federation === 'csts' ? 'cstsId' : 'wdsfId';
  const idLabel = federation === 'csts' ? 'IDT' : 'MIN';
  const [{ data, fetching, error }] = useQuery({
    query: MissingFederationIdCandidatesDocument,
    variables: {
      federation,
      condition: { [idField]: null, membership: { state: 'CURRENT' } },
    },
    requestPolicy: 'cache-and-network',
  });
  const updatePerson = useMutation(UpdatePersonFederationIdDocument)[1];
  const [pending, setPending] = React.useState<string | null>(null);
  const [mutationError, setMutationError] = React.useState<Error | null>(null);
  const [selectedByPerson, setSelectedByPerson] = React.useState<
    Record<string, Candidate>
  >({});

  const ageRanges = new Map(
    (data?.ageGroupsList ?? []).flatMap((group) =>
      [group.code, ...group.aliases]
        .filter(Boolean)
        .map((alias) => [ageKey(alias), group.personAge ?? group.age]),
    ),
  );
  const people = (data?.people?.nodes ?? [])
    .filter((person) => person[idField] === null || selectedByPerson[person.id])
    .map((person) => {
      const selected = selectedByPerson[person.id];
      return {
        ...person,
        candidates: (selected ? [selected] : (person.federationCandidatesList ?? []))
          .map((candidate) => ({
            ...candidate,
            ageRank: ageMatchRank(
              ageRanges.get(ageKey(person.ageGroup)),
              ageRanges.get(ageKey(candidate.ageGroup)),
            ),
          }))
          .filter((candidate) => candidate.id !== null && candidate.ageRank < 3)
          .toSorted(
            (a, b) => a.ageRank - b.ageRank || (b.similarity ?? 0) - (a.similarity ?? 0),
          )
          .slice(0, 10),
      };
    })
    .toSorted(
      (a, b) => (b.candidates[0]?.similarity ?? -1) - (a.candidates[0]?.similarity ?? -1),
    );

  async function selectCandidate(person: Person, candidate: Candidate | null) {
    if (candidate && candidate.id === null) return;
    setPending(person.id);
    setMutationError(null);
    const result = await updatePerson({
      input: { id: person.id, patch: { [idField]: candidate?.id ?? null } },
    });
    setPending(null);
    if (result.error) {
      setMutationError(result.error);
      return;
    }
    setSelectedByPerson((selected) => {
      const next = { ...selected };
      if (candidate) next[person.id] = candidate;
      else delete next[person.id];
      return next;
    });
  }

  return (
    <>
      <FormError error={mutationError ?? error} />

      {fetching && !data ? (
        <div className="text-sm text-neutral-11">Načítám kandidáty...</div>
      ) : !data?.people ? null : people.length === 0 ? (
        <div className="text-sm text-neutral-11">
          Všichni aktuální členové mají {federation === 'csts' ? 'ČSTS IDT' : 'WDSF MIN'}{' '}
          vyplněné.
        </div>
      ) : (
        <div className="grid gap-2">
          {people.map((person) => {
            const selectedCandidate = selectedByPerson[person.id];

            return (
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
                  <div className="text-neutral-11">
                    {[
                      formatAgeGroup(person.ageGroup),
                      person.birthDate ? new Date(person.birthDate).getFullYear() : null,
                    ]
                      .filter(Boolean)
                      .join(' · ')}
                  </div>
                </div>

                <div className="grid gap-1">
                  {person.candidates.length === 0 && (
                    <div className="text-neutral-11">Žádní kandidáti.</div>
                  )}
                  {person.candidates.map(
                    (candidate) =>
                      candidate.id !== null && (
                        <div
                          key={candidate.id}
                          className={`flex flex-wrap items-center justify-between gap-2 rounded-md px-2 py-1.5 ${selectedCandidate ? 'bg-accent-2 text-accent-12' : 'bg-neutral-2'}`}
                        >
                          <div className="min-w-0">
                            <div className="flex min-w-0 items-center gap-1 font-medium">
                              {selectedCandidate && 'Vyplněno: '}
                              <FederationLink
                                entity="people"
                                federation={federation}
                                externalId={candidate.id}
                              >
                                {candidate.name || 'bez jména'}
                              </FederationLink>
                            </div>
                            <div className="flex flex-wrap gap-x-3 gap-y-1 text-xs text-neutral-11">
                              <span>
                                {idLabel} {candidate.id}
                              </span>
                              {candidate.ageGroup && <span>{candidate.ageGroup}</span>}
                              <span>
                                Shoda jména{' '}
                                {candidate.similarity === null
                                  ? '-'
                                  : `${Math.round(candidate.similarity * 100)} %`}
                              </span>
                            </div>
                          </div>
                          <button
                            type="button"
                            className={buttonCls({ size: 'sm', variant: 'outline' })}
                            disabled={pending !== null}
                            onClick={() =>
                              selectCandidate(
                                person,
                                selectedCandidate ? null : candidate,
                              )
                            }
                          >
                            {selectedCandidate ? <Undo2 /> : <Check />}
                            {pending === person.id
                              ? 'Ukládám...'
                              : selectedCandidate
                                ? 'Zpět'
                                : 'Vybrat'}
                          </button>
                        </div>
                      ),
                  )}
                </div>
              </div>
            );
          })}
        </div>
      )}
    </>
  );
}

function ageKey(value: string | null) {
  return value?.toLowerCase().replaceAll(/[^a-z0-9]/g, '') ?? '';
}

function ageMatchRank(a: AgeRange | null | undefined, b: AgeRange | null | undefined) {
  if (!a || !b) return 1;
  const start = Math.max(a.start?.value ?? -Infinity, b.start?.value ?? -Infinity);
  const end = Math.min(a.end?.value ?? Infinity, b.end?.value ?? Infinity);
  return start < end ? 0 : start === end ? 2 : 3;
}

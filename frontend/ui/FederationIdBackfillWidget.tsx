import { DotSeparated } from '@/ui/DotSeparated';
import {
  MissingFederationIdCandidatesDocument,
  UpdatePersonDocument,
  type MissingFederationIdCandidatesQuery,
} from '@/graphql/Person';
import { FederationLink } from '@/ui/federation-links';
import { formatAgeGroup } from '@/ui/csts';
import { buttonCls } from '@/ui/style';
import { Check, Undo2 } from 'lucide-react';
import Link from 'next/link';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { FormError } from './form';
import { Spinner } from './Spinner';

type Person = NonNullable<MissingFederationIdCandidatesQuery['people']>['nodes'][number];
type Candidate = Person['federationCandidatesList'][number];

export function FederationIdBackfillWidget({
  federation,
}: {
  federation: 'csts' | 'wdsf';
}) {
  const idField = federation === 'csts' ? 'cstsId' : 'wdsfId';
  const idLabel = federation === 'csts' ? 'IDT' : 'MIN';
  const [{ data, error }] = useQuery({
    query: MissingFederationIdCandidatesDocument,
    variables: {
      federation,
      condition: { [idField]: null, membership: { state: 'CURRENT' } },
    },
  });
  const [{ error: mutationError, fetching: saving }, updatePerson] = useMutation(
    UpdatePersonDocument,
  );
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
          cstsId: person.cstsId == null ? (candidate.cstsId ?? undefined) : undefined,
          wdsfId: person.wdsfId == null ? (candidate.wdsfId ?? undefined) : undefined,
        }
      : (selectedByPerson[person.id]?.undo ?? {});
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
    <div className="grid gap-2" aria-busy={saving}>
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
              {[
                formatAgeGroup(person.ageGroup),
                person.birthDate ? new Date(person.birthDate).getFullYear() : null,
              ]}
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

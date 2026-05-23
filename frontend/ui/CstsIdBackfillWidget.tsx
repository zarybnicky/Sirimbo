import {
  MissingCstsIdCandidatesDocument,
  UpdatePersonCstsIdDocument,
  type MissingCstsIdCandidatesQuery,
} from '@/graphql/Person';
import { formatAgeGroup } from '@/ui/format';
import { buttonCls, typographyCls } from '@/ui/style';
import { Check, Undo2 } from 'lucide-react';
import React from 'react';
import { useMutation, useQuery } from 'urql';

type Person = NonNullable<
  NonNullable<MissingCstsIdCandidatesQuery['people']>['nodes'][number]
>;
type Candidate = NonNullable<Person['cstsCandidatesList']>[number];

export function CstsIdBackfillWidget() {
  const [{ data, fetching, error }] = useQuery({
    query: MissingCstsIdCandidatesDocument,
    requestPolicy: 'cache-and-network',
  });
  const updatePersonCstsId = useMutation(UpdatePersonCstsIdDocument)[1];
  const [pending, setPending] = React.useState<string | null>(null);
  const [mutationError, setMutationError] = React.useState<Error | null>(null);
  const [selectedByPerson, setSelectedByPerson] = React.useState<
    Record<string, Candidate>
  >({});

  const people = React.useMemo(
    () =>
      (data?.people?.nodes ?? [])
        .filter((person) => !person.cstsId || selectedByPerson[person.id])
        .toSorted((a, b) => bestCandidateSimilarity(b) - bestCandidateSimilarity(a)),
    [data?.people?.nodes, selectedByPerson],
  );

  const fillCstsId = React.useCallback(
    async (person: Person, candidate: Candidate) => {
      if (candidate.id == null) return;

      const key = `${person.id}:fill:${candidate.id}`;
      setPending(key);
      setMutationError(null);
      const result = await updatePersonCstsId({
        input: {
          id: person.id,
          patch: { cstsId: candidate.id },
        },
      });
      setPending(null);

      if (result.error) {
        setMutationError(result.error);
        return;
      }

      setSelectedByPerson((selected) => ({ ...selected, [person.id]: candidate }));
    },
    [updatePersonCstsId],
  );

  const undoCstsId = React.useCallback(
    async (person: Person) => {
      const key = `${person.id}:undo`;
      setPending(key);
      setMutationError(null);
      const result = await updatePersonCstsId({
        input: {
          id: person.id,
          patch: { cstsId: null },
        },
      });
      setPending(null);

      if (result.error) {
        setMutationError(result.error);
        return;
      }

      setSelectedByPerson((selected) => {
        const next = { ...selected };
        delete next[person.id];
        return next;
      });
    },
    [updatePersonCstsId],
  );

  return (
    <section className="mt-6">
      <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
        Doplnit ČSTS IDT
      </h2>

      {(error || mutationError) && (
        <div className="mb-2 rounded-md border border-accent-6 bg-accent-2 p-2 text-sm text-accent-11">
          {(mutationError ?? error)?.message}
        </div>
      )}

      {fetching && !data ? (
        <div className="text-sm text-neutral-11">Načítám kandidáty...</div>
      ) : people.length === 0 ? (
        <div className="text-sm text-neutral-11">
          Všichni aktuální členové mají ČSTS IDT vyplněné.
        </div>
      ) : (
        <div className="grid gap-2">
          {people.map((person) => {
            const selectedCandidate = selectedByPerson[person.id];
            const undoKey = `${person.id}:undo`;

            return (
              <div
                key={person.id}
                className="grid gap-2 border-b border-neutral-4 py-3 text-sm"
              >
                <div className="flex flex-wrap items-baseline gap-x-2 gap-y-1">
                  <div className="font-bold">{person.name}</div>
                  <div className="text-neutral-11">
                    {formatBirthInfo(person.birthDate)}
                  </div>
                </div>

                {selectedCandidate ? (
                  <div className="flex flex-wrap items-center justify-between gap-2 rounded-md bg-accent-2 px-2 py-1.5 text-accent-12">
                    <div className="min-w-0">
                      <div className="truncate font-medium">
                        Vyplněno: IDT {selectedCandidate.id}
                      </div>
                      <div className="flex flex-wrap gap-x-3 gap-y-1 text-xs text-accent-11">
                        <span>{selectedCandidate.name ?? 'bez jména'}</span>
                        {selectedCandidate.ageGroup ? (
                          <span>{selectedCandidate.ageGroup}</span>
                        ) : null}
                        <span>
                          Shoda {formatSimilarity(selectedCandidate.similarity)}
                        </span>
                      </div>
                    </div>
                    <button
                      type="button"
                      className={buttonCls({ size: 'sm', variant: 'outline' })}
                      disabled={pending !== null}
                      onClick={() => undoCstsId(person)}
                    >
                      <Undo2 />
                      {pending === undoKey ? 'Vracím...' : 'Zpět'}
                    </button>
                  </div>
                ) : (
                  <div className="grid gap-1">
                    {person.cstsCandidatesList?.length ? (
                      person.cstsCandidatesList.map((candidate) => {
                        if (candidate.id == null) return null;
                        const key = `${person.id}:fill:${candidate.id}`;

                        return (
                          <div
                            key={candidate.id}
                            className="flex flex-wrap items-center justify-between gap-2 rounded-md bg-neutral-2 px-2 py-1.5"
                          >
                            <div className="min-w-0">
                              <div className="truncate font-medium">
                                {candidate.name ?? 'bez jména'}
                              </div>
                              <div className="flex flex-wrap gap-x-3 gap-y-1 text-xs text-neutral-11">
                                <span>IDT {candidate.id}</span>
                                {candidate.ageGroup ? (
                                  <span>{candidate.ageGroup}</span>
                                ) : null}
                                <span>
                                  Shoda {formatSimilarity(candidate.similarity)}
                                </span>
                              </div>
                            </div>
                            <button
                              type="button"
                              className={buttonCls({ size: 'sm', variant: 'outline' })}
                              disabled={pending !== null}
                              onClick={() => fillCstsId(person, candidate)}
                            >
                              <Check />
                              {pending === key ? 'Ukládám...' : 'Vybrat'}
                            </button>
                          </div>
                        );
                      })
                    ) : (
                      <span className="text-neutral-11">Žádný kandidát</span>
                    )}
                  </div>
                )}
              </div>
            );
          })}
        </div>
      )}
    </section>
  );
}

function formatBirthInfo(value: string | null) {
  if (!value) return 'bez data narození';
  return [new Date(value).getFullYear(), formatAgeGroup(value)]
    .filter(Boolean)
    .join(' · ');
}

function formatSimilarity(value: number | null) {
  if (value == null) return '-';
  return `${Math.round(value * 100)} %`;
}

function bestCandidateSimilarity(person: Person) {
  return Math.max(
    -1,
    ...(person.cstsCandidatesList ?? []).map((candidate) => candidate.similarity ?? -1),
  );
}

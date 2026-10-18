import { DialogTitle } from '@/ui/dialog';
import { CoupleFormPeopleDocument, CreateCoupleDocument } from '@/graphql/Memberships';
import { AgeGroupRulesDocument } from '@/graphql/AgeGroups';
import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { formatAgeGroup, getCoupleAgeGroups } from '@/ui/csts';
import { Pencil } from 'lucide-react';
import { FieldLabel, FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm, useWatch } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { GenderType } from '@/graphql';

const Form = z.object({
  man: z.string().min(1, 'Vyberte partnera'),
  woman: z.string().min(1, 'Vyberte partnerku'),
});

export function CreateCoupleForm({
  person: initialPerson,
}: {
  person?: {
    id: string;
    gender: GenderType;
  };
}) {
  const { onSuccess } = useFormResult();
  const [showEveryone, setShowEveryone] = React.useState(false);
  const [result, doCreate] = useMutation(CreateCoupleDocument);
  const [peopleResult] = useQuery({
    query: CoupleFormPeopleDocument,
    variables: { personId: initialPerson?.id ?? '0', withPerson: !!initialPerson },
  });
  const [rulesResult] = useQuery({ query: AgeGroupRulesDocument });
  const people = [
    ...new Map(
      [
        ...(peopleResult.data?.people?.nodes ?? []),
        ...(peopleResult.data?.person ? [peopleResult.data.person] : []),
      ].map((person) => [
        person.id,
        {
          ...person,
          label: [
            person.name + (person.birthDate ? ` (${person.birthDate.slice(0, 4)})` : ''),
            formatAgeGroup(person.ageGroup),
          ]
            .filter(Boolean)
            .join(' · '),
        },
      ]),
    ).values(),
  ];
  const { control, handleSubmit, setValue } = useForm({
    resolver: zodResolver(Form),
    values: {
      man: initialPerson?.gender === 'MAN' ? initialPerson.id : '',
      woman: initialPerson?.gender === 'WOMAN' ? initialPerson.id : '',
    },
  });
  const [manId, womanId] = useWatch({ control, name: ['man', 'woman'] });
  const man = people.find((person) => person.id === manId);
  const woman = people.find((person) => person.id === womanId);
  const anchor = manId && !womanId ? man : womanId && !manId ? woman : undefined;
  const options = people
    .filter(
      (person) =>
        (person.gender === 'MAN' || person.gender === 'WOMAN') &&
        (!manId || person.gender === 'WOMAN') &&
        (!womanId || person.gender === 'MAN'),
    )
    .flatMap((person) => {
      const groups = getCoupleAgeGroups(
        anchor?.birthDate,
        person.birthDate,
        rulesResult.data?.ageGroupsList,
      );
      if (anchor && !showEveryone && groups.length === 0) return [];
      return [
        {
          ...person,
          ageDifference:
            anchor?.birthDate && person.birthDate
              ? Math.abs(
                  Number(anchor.birthDate.slice(0, 4)) -
                    Number(person.birthDate.slice(0, 4)),
                )
              : Infinity,
        },
      ];
    })
    .toSorted((a, b) => a.ageDifference - b.ageDifference);

  const selectedGroups = getCoupleAgeGroups(
    man?.birthDate,
    woman?.birthDate,
    rulesResult.data?.ageGroupsList,
  );
  const loading =
    peopleResult.fetching || (!!anchor && !showEveryone && rulesResult.fetching);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await doCreate({
      input: {
        couple: {
          manId: values.man,
          womanId: values.woman,
          since: new Date().toISOString(),
        },
      },
    });
    if (!result.error && result.data?.createCouple?.couple?.id) onSuccess();
  };

  return (
    <form className="grid gap-3 text-sm" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Přidat do páru</DialogTitle>
      <FormError error={result.error ?? peopleResult.error ?? rulesResult.error} />
      {(['man', 'woman'] as const).map((field) =>
        (field === 'man' ? manId : womanId) ? (
          <div key={field}>
            <FieldLabel>{field === 'man' ? 'Partner' : 'Partnerka'}</FieldLabel>
            <button
              type="button"
              className="flex w-full items-center justify-between gap-3 rounded-md border border-accent-7 bg-accent-2 px-3 py-2 text-left hover:bg-accent-3 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-9"
              onClick={() => {
                setValue(field, '', { shouldDirty: true });
                setShowEveryone(false);
              }}
            >
              <span>{(field === 'man' ? man : woman)?.label ?? 'Načítám…'}</span>
              <Pencil className="size-4 shrink-0 opacity-50" aria-hidden="true" />
              <span className="sr-only">
                {field === 'man' ? 'Změnit partnera' : 'Změnit partnerku'}
              </span>
            </button>
          </div>
        ) : null,
      )}

      {selectedGroups.length > 0 && (
        <div className="text-neutral-11">
          Věková kategorie:{' '}
          {selectedGroups
            .map((group) => formatAgeGroup(group.code, group.name))
            .join(' / ')}
        </div>
      )}

      {(!manId || !womanId) && (
        <div>
          <div className="flex flex-wrap items-center justify-between gap-2">
            <FieldLabel>
              {manId ? 'Partnerka' : womanId ? 'Partner' : 'První osoba'}
            </FieldLabel>
            {anchor && (
              <button
                type="button"
                role="switch"
                aria-label="Zobrazit všechny"
                aria-checked={showEveryone}
                onClick={() => setShowEveryone(!showEveryone)}
                className="flex items-center gap-2 rounded-sm focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-9"
              >
                <span>Návrhy</span>
                <span
                  className="inline-flex h-5 w-9 items-center rounded-full bg-accent-7 p-0.5"
                  aria-hidden="true"
                >
                  <span
                    className={`size-4 rounded-full bg-neutral-1 motion-safe:transition-transform ${showEveryone ? 'translate-x-4' : 'translate-x-0'}`}
                  />
                </span>
                <span>Všichni</span>
              </button>
            )}
          </div>
          {loading ? (
            <p className="text-sm text-neutral-11">Načítám…</p>
          ) : (
            <>
              {anchor && !showEveryone && !anchor.birthDate && (
                <p className="text-sm text-neutral-11">
                  Chybí datum narození. Přepněte na Všichni.
                </p>
              )}
              <ComboboxSearchArea
                key={anchor?.id ?? 'first'}
                inline
                options={options}
                onChange={(id) => {
                  const person = people.find((person) => person.id === id);
                  if (!person) return;
                  setValue(person.gender === 'MAN' ? 'man' : 'woman', person.id, {
                    shouldDirty: true,
                    shouldValidate: true,
                  });
                }}
              />
            </>
          )}
        </div>
      )}
      <SubmitButton control={control} disabled={!manId || !womanId}>
        Spárovat
      </SubmitButton>
    </form>
  );
}

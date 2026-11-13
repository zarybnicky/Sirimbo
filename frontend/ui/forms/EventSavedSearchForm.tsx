import { eventTypes, savedSearchAtom } from '@/calendar/state';
import { calendarTypeOptions, calendarTypes } from '@/calendar/useCalendarData';
import { useAtom } from 'jotai';
import type { EventSavedSearchInput } from '@/graphql';
import {
  CreateEventSavedSearchDocument,
  EventFormOptionsDocument,
  type EventSavedSearchFragment,
  UpdateEventSavedSearchDocument,
} from '@/graphql/Event';
import { isTruthy } from '@/lib/truthyFilter';
import { FacetFilter } from '@/ui/FacetFilter';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { ComboboxButton, ComboboxElement } from '@/ui/fields/Combobox';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FieldHelper, FormError, useFormResult } from '@/ui/form';
import { fullDateFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { Copy, Plus, X } from 'lucide-react';
import { useMemo } from 'react';
import { Controller, useFieldArray, useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const optionalIds = z
  .array(z.string())
  .nullish()
  .transform((xs) => (xs?.length ? xs : null));

const Form = z.object({
  people: z
    .array(
      z.object({
        personId: z.string().nullable(),
        coupleId: z.string().nullable(),
        onlyTeaching: z.boolean().prefault(false),
      }),
    )
    .prefault([]),
  name: z.string().trim().min(1, 'Vyplňte název'),
  crossTenant: z.boolean().prefault(false),
  condition: z
    .object({
      types: z.array(z.enum(calendarTypes)),
      locationIds: optionalIds,
      parentId: z.string().nullable().prefault(null),
      cohortId: z.string().nullable().prefault(null),
      onlyMine: z.boolean().nullable().prefault(false),
      canRegister: z
        .boolean()
        .nullish()
        .transform((x) => x || null),
    })
    .prefault({ types: [...eventTypes] })
    .transform((condition) => ({
      ...condition,
      relevantOnly: !condition.parentId && !condition.locationIds?.length,
    })),
});

export function EventSavedSearchForm({
  savedSearch,
}: {
  savedSearch?: EventSavedSearchFragment;
}) {
  const { onSuccess } = useFormResult();
  const [selectedSearch, setSelectedSearch] = useAtom(savedSearchAtom);
  const [{ data, error }] = useQuery({
    query: EventFormOptionsDocument,
    variables: {
      withParents: !!savedSearch?.condition?.parentId,
      withPeople: true,
      peopleCondition: null,
    },
  });
  const [createResult, create] = useMutation(CreateEventSavedSearchDocument);
  const [updateResult, update] = useMutation(UpdateEventSavedSearchDocument);
  const record =
    createResult.data?.createEventSavedSearch?.eventSavedSearch ?? savedSearch;
  const condition = savedSearch?.condition;
  const defaultValues = useMemo(() => {
    const condition = savedSearch?.condition;
    return {
      name: savedSearch?.name ?? 'Moje události',
      crossTenant: savedSearch?.crossTenant ?? false,
      condition: {
        onlyMine: true,
        ...condition,
        types: calendarTypes.filter((type) =>
          type === 'COMPETITION'
            ? !condition || condition.withCompetitions
            : type === 'BIRTHDAY'
              ? condition?.withBirthdays
              : !condition?.types || condition.types.includes(type),
        ),
        locationIds: condition?.locationIds?.filter(isTruthy) ?? null,
      },
      people: [
        ...[
          ...new Set([
            ...(condition?.participantIds?.filter(isTruthy) ?? []),
            ...(condition?.trainerIds?.filter(isTruthy) ?? []),
          ]),
        ].map((personId) => ({
          personId,
          coupleId: null,
          onlyTeaching: condition?.trainerIds?.includes(personId) ?? false,
        })),
        ...(condition?.coupleId
          ? [
              {
                personId: null,
                coupleId: condition.coupleId,
                onlyTeaching: false,
              },
            ]
          : []),
      ],
    };
  }, [savedSearch]);
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues,
  });
  const { fields: people, append, remove } = useFieldArray({ control, name: 'people' });

  const options = useMemo(
    () => ({
      locations:
        data?.tenant?.locationsList?.map((location) => ({
          value: location.id,
          label: location.name,
        })) ?? [],
      cohorts:
        data?.tenant?.cohortsList
          ?.filter((cohort) => !cohort.isArchived)
          .map((cohort) => ({ value: cohort.id, label: cohort.name })) ?? [],
      people: new Map(
        [
          ...(data?.people?.nodes.map((person) => ({
            id: `person:${person.id}`,
            label: person.name,
            personId: person.id,
            coupleId: null,
          })) ?? []),
          ...(data?.couplesList?.map((couple) => ({
            id: `couple:${couple.id}`,
            label: couple.name,
            personId: null,
            coupleId: couple.id,
          })) ?? []),
        ].map((option) => [option.id, option]),
      ),
      parents: [
        { id: null, label: 'Bez omezení' },
        ...(data?.parents?.map((event) => ({
          id: event.id,
          label: `${event.name || 'Soustředění'} – ${fullDateFormatter.formatRange(
            new Date(event.since),
            new Date(event.until),
          )}`,
        })) ?? []),
      ],
    }),
    [data],
  );
  const availablePeople = useMemo(() => {
    const selected = new Set(
      people.map((person) =>
        person.personId ? `person:${person.personId}` : `couple:${person.coupleId}`,
      ),
    );
    const hasCouple = people.some((person) => person.coupleId);
    return [...options.people.values()].filter(
      (option) => !selected.has(option.id) && (!option.coupleId || !hasCouple),
    );
  }, [options.people, people]);

  const onSubmit = async (values: z.output<typeof Form>) => {
    const { people, ...search } = values;
    const { types, canRegister } = search.condition;
    const participantIds = people.flatMap((person) =>
      person.personId && !person.onlyTeaching ? [person.personId] : [],
    );
    const trainerIds = people.flatMap((person) =>
      person.personId && person.onlyTeaching ? [person.personId] : [],
    );
    const input = {
      ...search,
      condition: {
        ...search.condition,
        participantIds: participantIds.length > 0 ? participantIds : null,
        trainerIds: trainerIds.length > 0 ? trainerIds : null,
        coupleId: people.find((person) => person.coupleId)?.coupleId ?? null,
        types: eventTypes.every((type) => types.includes(type))
          ? null
          : types.filter((type) => type !== 'COMPETITION' && type !== 'BIRTHDAY'),
        withCompetitions: !canRegister && types.includes('COMPETITION'),
        withBirthdays: !canRegister && types.includes('BIRTHDAY'),
      },
    } satisfies EventSavedSearchInput;
    if (!record) {
      await create({ input });
      return;
    }
    const result = await update({ id: record.id, patch: input });
    if (result.error) return;
    const updated = result.data?.updateEventSavedSearch?.eventSavedSearch;
    if (updated && selectedSearch?.id === updated.id) setSelectedSearch(updated);
    onSuccess();
  };

  const feedUrl =
    record?.feedKey && typeof window !== 'undefined'
      ? `${window.location.origin}/api/calendar/${record.feedKey}`
      : '';

  return (
    <form className="grid gap-1" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={createResult.error ?? updateResult.error ?? error} />
      <TextFieldElement control={control} name="name" label="Název filtru" />

      <div className="grid grid-cols-1 gap-2 sm:grid-cols-[auto_1fr]">
        <Controller
          control={control}
          name="condition.types"
          render={({ field, fieldState }) => (
            <div className="col-span-full grid grid-cols-subgrid gap-y-1">
              <FacetFilter
                inline
                label="Typ"
                value={field.value ?? calendarTypes}
                defaultValue={calendarTypes}
                options={calendarTypeOptions}
                onChange={field.onChange}
              />
              <FieldHelper error={fieldState.error} />
            </div>
          )}
        />
        <Controller
          control={control}
          name="condition.locationIds"
          render={({ field, fieldState }) => (
            <div className="col-span-full grid grid-cols-subgrid gap-y-1">
              <FacetFilter
                inline
                label="Místa"
                value={field.value ?? []}
                options={options.locations}
                onChange={field.onChange}
              />
              <FieldHelper error={fieldState.error} />
            </div>
          )}
        />
        <Controller
          control={control}
          name="condition.cohortId"
          render={({ field, fieldState }) => (
            <div className="col-span-full grid grid-cols-subgrid gap-y-1">
              <FacetFilter
                inline
                label="Skupina"
                value={field.value ? [field.value] : []}
                options={options.cohorts}
                onChange={(cohorts) => field.onChange(cohorts.at(-1) ?? null)}
              />
              <FieldHelper error={fieldState.error} />
            </div>
          )}
        />
      </div>

      <fieldset className="grid min-w-0 gap-1">
        <legend className="sr-only">Lidé a páry</legend>
        <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
          <b aria-hidden>Lidé a páry</b>
          <ComboboxButton
            align="end"
            side="top"
            trigger={
              <button
                type="button"
                className={buttonCls({ size: 'xs', variant: 'outline' })}
              >
                <Plus /> Přidat
              </button>
            }
            options={availablePeople}
            onChange={(id) => {
              const person = id ? options.people.get(id) : undefined;
              if (person)
                append({
                  personId: person.personId,
                  coupleId: person.coupleId,
                  onlyTeaching: false,
                });
            }}
          />
        </div>
        {people.map((person, index) => {
          const key = person.personId
            ? `person:${person.personId}`
            : `couple:${person.coupleId}`;
          const label = options.people.get(key)?.label ?? key;
          return (
            <div key={person.id} className="flex flex-wrap items-center gap-2">
              <span className="grow">{label}</span>
              {person.personId && (
                <CheckboxElement
                  control={control}
                  name={`people.${index}.onlyTeaching`}
                  label="Pouze výuka"
                />
              )}
              <button
                type="button"
                className={buttonCls({ size: 'sm', variant: 'outline' })}
                onClick={() => remove(index)}
              >
                <X />
              </button>
            </div>
          );
        })}
        <p className="text-sm text-neutral-11">
          Události, kde vybraní lidé učí nebo se účastní.
        </p>
      </fieldset>

      {condition?.parentId && (
        <ComboboxElement
          control={control}
          name="condition.parentId"
          label="Součást soustředění"
          placeholder="Bez omezení"
          options={options.parents}
        />
      )}
      {condition?.canRegister && (
        <CheckboxElement
          control={control}
          name="condition.canRegister"
          label="Pouze volné lekce"
        />
      )}

      <CheckboxElement
        control={control}
        name="condition.onlyMine"
        label="Pouze moje události"
      />
      <CheckboxElement
        control={control}
        name="crossTenant"
        label="Kalendář ze všech klubů"
      />
      <p className="text-sm text-neutral-11">
        Pouze pro externí kalendáře, na webu se zobrazí pouze události tohoto klubu.
      </p>

      {record?.feedKey && (
        <div className="grid gap-1 border-t border-neutral-4 pt-3">
          <TextField value={feedUrl} readOnly />
          <div className="flex flex-wrap gap-2">
            <button
              type="button"
              className={buttonCls({ variant: 'outline', size: 'sm' })}
              onClick={async () => {
                await navigator.clipboard.writeText(feedUrl);
              }}
            >
              <Copy /> Kopírovat odkaz
            </button>
            <a
              href={feedUrl.replace(/^https?:/, 'webcal:')}
              className={buttonCls({ variant: 'outline', size: 'sm' })}
            >
              Přidat do Apple Kalendáře
            </a>
          </div>
          <details>
            <summary>Jak přidat do Google Kalendáře</summary>
            <ol className="list-decimal pl-5 space-y-1">
              <li>Na počítači zkopírujte odkaz tlačítkem výše.</li>
              <li>
                <a
                  href="https://calendar.google.com/calendar/u/0/r/settings/addbyurl"
                  target="_blank"
                  rel="noreferrer"
                  className="text-accent-11 underline"
                >
                  Otevřete přidání kalendáře v Googlu.
                </a>
              </li>
              <li>
                Vložte odkaz do „Adresa URL kalendáře“ a potvrďte „Přidat kalendář“.
              </li>
              <li>
                V telefonu použijte stejný účet Google a zapněte zobrazení přidaného
                kalendáře.
              </li>
            </ol>
          </details>
          <details>
            <summary>Jak přidat do Apple Kalendáře</summary>
            <p>
              Klikněte na tlačítko výše a potvrďte odběr. Pokud se aplikace neotevře, na
              iPhonu zvolte Kalendář → Kalendáře → Přidat kalendář → Přidat odebíraný
              kalendář a vložte odkaz.
            </p>
          </details>
        </div>
      )}

      <SubmitButton control={control}>{record ? 'Uložit' : 'Vytvořit'}</SubmitButton>
    </form>
  );
}
